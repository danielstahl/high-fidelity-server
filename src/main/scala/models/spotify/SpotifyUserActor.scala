package models.spotify

import java.util.Base64

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.{BasicHttpCredentials, OAuth2BearerToken}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import spray.json.DefaultJsonProtocol
import spray.json._
import DefaultJsonProtocol._
import akka.http.scaladsl.marshalling.Marshal

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

case class ArtistDigest(spotifyUri: String, name: String, imageUrl: Option[String])
case class TrackDigest(spotifyUri: String, name: String, durationMs: Long, artists: Seq[ArtistDigest])
case class DeviceDigest(id: String, name: String, theType: String, volumePercent: Int, active: Boolean)

case class SpotifyLoginCallback(token: String, code: String, requestor: ActorRef)
case class SpotifyStatus(loggedIn: Boolean)

case class SearchArtists(token: String, query: String, requestor: ActorRef)
case class ArtistSearchResult(result: Seq[ArtistDigest])
case class SearchError(query: String, cause: String)

case class PlaybackStatusRequest(token: String, requestor: ActorRef)
case class PlaybackStatus(track: Option[TrackDigest], device: Option[DeviceDigest], contextUri: Option[String], progressMs: Long, isPlaying: Boolean)

case class FetchAlbumRequest(token: String, albumUri: String, requestor: ActorRef)
case class FetchAlbumResult(album: SpotifyAlbum)

sealed trait PlaybackCommand
case object PLAY extends PlaybackCommand
case object PAUSE extends PlaybackCommand
case object NEXT extends PlaybackCommand
case object PREVIOUS extends PlaybackCommand

case class ExternalPlayRequest(uris: Option[Seq[String]], position: Option[Int])
case class PlaybackRequest(token: String, command: PlaybackCommand, deviceId: Option[String], uris: Option[Seq[String]], position: Option[Int], requestor: ActorRef)

case class SpotifyRequestError(cause: String)

case class AccessToken(access_token: String, token_type: String, scope: Option[String], expires_in: Int, refresh_token: String)

trait SpotifyUserJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val accessTokenFormat = jsonFormat5(AccessToken)
  implicit val searchErrorFormat = jsonFormat2(SearchError)
  implicit val artistDigestFormat = jsonFormat3(ArtistDigest)
  implicit val trackDigestFormat = jsonFormat4(TrackDigest)
  implicit val deviceDigestFormat = jsonFormat5(DeviceDigest)
  implicit val artistSearchResultFormat = jsonFormat1(ArtistSearchResult)
  implicit val playbackStatusFormat = jsonFormat5(PlaybackStatus)
  implicit val spotifyRequestErrorFormat = jsonFormat1(SpotifyRequestError)
  implicit val spotifyExternalPlayRequestFormat = jsonFormat2(ExternalPlayRequest)

}

class SpotifyUserActor(userActor: ActorRef, var accessToken: Option[AccessToken]) extends Actor with ActorLogging with SpotifyUserJsonSupport with SpotifyModelJsonSupport {

  import context.dispatcher

  private val clientId = context.system.settings.config.getString("services.user-account.spotify-client-id")
  private val clientSecret = context.system.settings.config.getString("services.user-account.spotify-client-secret")
  private val spotifyAccountsAuth = Base64.getEncoder.encodeToString(s"$clientId:$clientSecret".getBytes)

  final implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(context.system))

  val http = Http(context.system)

  def login(code: String): Future[AccessToken] = {
    val data = Map(
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> "http://localhost:8080/spotify-login-callback")

    val request = HttpRequest(uri = "https://accounts.spotify.com/api/token",
      method = HttpMethods.POST,
      entity = FormData(data).toEntity,
      headers = List(headers.Authorization(BasicHttpCredentials(spotifyAccountsAuth))))

    http.singleRequest(request)
      .flatMap(httpResponse => Unmarshal(httpResponse).to[AccessToken])
  }

  def getLastImage(spotifyArtist: SpotifyArtist): Option[String] = {
    if(spotifyArtist.images.isEmpty) {
      Option.empty
    } else {
      Option(spotifyArtist.images.last.url)
    }
  }

  def spotifySearchResult2ArtistSearchResult(spotifySearchResult: SpotifySearchResult): ArtistSearchResult = {
    val searchResult = spotifySearchResult.artists.map(spotifyArtistResult =>
      spotifyArtistResult.items.map(spotifyArtist =>
        ArtistDigest(spotifyArtist.uri, spotifyArtist.name, getLastImage(spotifyArtist))))
      .getOrElse(Seq())
    ArtistSearchResult(searchResult)
  }

  def spotifyTrack2TrackDigest(spotifyPlaybackStatus: SpotifyPlaybackStatus): Option[TrackDigest] =
    spotifyPlaybackStatus.item.map(spotifyTrack =>
      TrackDigest(spotifyTrack.uri, spotifyTrack.name, spotifyTrack.duration_ms, spotifyTrack.artists.map(spotifyArtist =>
        ArtistDigest(spotifyArtist.uri, spotifyArtist.name, None))))

  def spotifyDevice2DeviceDigeest(spotifyPlaybackStatus: SpotifyPlaybackStatus): Option[DeviceDigest] = {
    val device = spotifyPlaybackStatus.device
    device.id.map(deviceId =>
      DeviceDigest(deviceId, device.name, device.`type`, device.volume_percent.getOrElse(0), device.is_active))
  }


  def spotifyPlaybackStatus2PlaybackStatus(spotifyPlaybackStatus: SpotifyPlaybackStatus): PlaybackStatus = {
    PlaybackStatus(
      spotifyTrack2TrackDigest(spotifyPlaybackStatus),
      spotifyDevice2DeviceDigeest(spotifyPlaybackStatus),
      spotifyPlaybackStatus.context.uri,
      spotifyPlaybackStatus.progress_ms, spotifyPlaybackStatus.is_playing)
  }

  def searchArtists(query: String): Future[ArtistSearchResult] = {

    val uri = Uri("https://api.spotify.com/v1/search")
        .withQuery(Uri.Query(("query", query), ("market", "SE"), ("type", "artist")))

    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.GET,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token)))
    )

    val responseFuture: Future[HttpResponse] =
      http.singleRequest(request)

    val spotifySearchFuture =
      responseFuture.flatMap(response =>
        Unmarshal(response).to[SpotifySearchResult])

    spotifySearchFuture.map(spotifySearchResult => spotifySearchResult2ArtistSearchResult(spotifySearchResult))
  }


  def fetchAlbum(albumUri: String): Future[SpotifyAlbum] = {
    val uriParts = albumUri.split(":")
    val albumId = uriParts.last

    val uri = Uri(s"https://api.spotify.com/v1/albums/$albumId")

    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.GET,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token)))
    )

    val responseFuture: Future[HttpResponse] =
      http.singleRequest(request)

    responseFuture.flatMap(response =>
      Unmarshal(response).to[SpotifyAlbum])
  }

  def currentPlaybackStatus(): Future[PlaybackStatus] = {
    val uri = Uri("https://api.spotify.com/v1/me/player")
        .withQuery(Uri.Query(("market", "SE")))
    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.GET,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token)))
    )

    val responseFuture: Future[HttpResponse] =
      http.singleRequest(request)

    val spotifyPlaybackStatusFuture =
      responseFuture.flatMap {
        case response @ HttpResponse(StatusCodes.OK, _, _, _) =>
          Unmarshal(response).to[SpotifyPlaybackStatus]
        case response @ HttpResponse(status, headers, entity , _) =>
          Unmarshal(entity).to[SpotifyErrorStatus].flatMap(spotifyErrorStatus =>
            Future.failed(new RuntimeException(s"Status $status: ${spotifyErrorStatus.error.message}")))
      }

    spotifyPlaybackStatusFuture.map(spotifyPlaybackStatus =>
      spotifyPlaybackStatus2PlaybackStatus(spotifyPlaybackStatus))
  }


  def play(optionalDeviceId: Option[String], optionalUris: Option[Seq[String]], optionalPosition: Option[Int]): Future[PlaybackStatus] = {
    val (theUris, optionalContextUri) = optionalUris
        .filter(uris => uris.nonEmpty)
        .map(uris =>
          if(uris.forall(_.startsWith("spotify:track")))(Option(uris), None)
          else (None, Option(uris.head)))
        .getOrElse((None, None))
    var uri = Uri("https://api.spotify.com/v1/me/player/play")

    optionalDeviceId.foreach { deviceId =>
      uri = uri.withQuery(Uri.Query(("device_id", deviceId)))
    }

    val spotifyPlayContextOffset = optionalPosition.map(position => SpotifyPlayContextOffset(optionalPosition, None))
    val spotifyPlayContext = SpotifyPlayContext(optionalContextUri, theUris, spotifyPlayContextOffset)

    val responseFuture = Marshal(spotifyPlayContext).to[RequestEntity].flatMap {
      entity => {
        val request = HttpRequest(
          uri = uri,
          method = HttpMethods.PUT,
          headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token))),
          entity = entity)
        http.singleRequest(request)
      }
    }

    responseFuture.flatMap {
      case response @ HttpResponse(StatusCodes.NoContent, _, _, _) =>
        currentPlaybackStatus()
      case response @ HttpResponse(status, headers, entity , _) =>
        Unmarshal(entity).to[SpotifyErrorStatus].flatMap(spotifyErrorStatus =>
          Future.failed(new RuntimeException(s"Status $status: ${spotifyErrorStatus.error.message}")))
    }
  }

  def pause(optionalDeviceId: Option[String]): Future[PlaybackStatus] = {
    var uri = Uri("https://api.spotify.com/v1/me/player/pause")

    optionalDeviceId.foreach { deviceId =>
      uri = uri.withQuery(Uri.Query(("device_id", deviceId)))
    }

    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.PUT,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token))))
    val responseFuture =  http.singleRequest(request)

    responseFuture.flatMap {
      case response @ HttpResponse(StatusCodes.NoContent, _, _, _) =>
        currentPlaybackStatus()
      case response @ HttpResponse(status, headers, entity , _) =>
        Unmarshal(entity).to[SpotifyErrorStatus].flatMap(spotifyErrorStatus =>
          Future.failed(new RuntimeException(s"Status $status: ${spotifyErrorStatus.error.message}")))
    }
  }

  def next(optionalDeviceId: Option[String]): Future[PlaybackStatus] = {
    var uri = Uri("https://api.spotify.com/v1/me/player/next")

    optionalDeviceId.foreach { deviceId =>
      uri = uri.withQuery(Uri.Query(("device_id", deviceId)))
    }

    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.POST,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token))))
    val responseFuture =  http.singleRequest(request)

    responseFuture.flatMap {
      case response @ HttpResponse(StatusCodes.NoContent, _, _, _) =>
        currentPlaybackStatus()
      case response @ HttpResponse(status, headers, entity , _) =>
        Unmarshal(entity).to[SpotifyErrorStatus].flatMap(spotifyErrorStatus =>
          Future.failed(new RuntimeException(s"Status $status: ${spotifyErrorStatus.error.message}")))
    }
  }

  def previous(optionalDeviceId: Option[String]): Future[PlaybackStatus] = {
    var uri = Uri("https://api.spotify.com/v1/me/player/previous")

    optionalDeviceId.foreach { deviceId =>
      uri = uri.withQuery(Uri.Query(("device_id", deviceId)))
    }

    val request = HttpRequest(
      uri = uri,
      method = HttpMethods.POST,
      headers = List(headers.Authorization(OAuth2BearerToken(accessToken.get.access_token))))
    val responseFuture =  http.singleRequest(request)

    responseFuture.flatMap {
      case response @ HttpResponse(StatusCodes.NoContent, _, _, _) =>
        currentPlaybackStatus()
      case response @ HttpResponse(status, headers, entity , _) =>
        Unmarshal(entity).to[SpotifyErrorStatus].flatMap(spotifyErrorStatus =>
          Future.failed(new RuntimeException(s"Status $status: ${spotifyErrorStatus.error.message}")))
    }
  }

  def receive = {
    case SpotifyLoginCallback(token, code, requestor) =>
      login(code).map {
        accessToken =>
          this.accessToken = Option(accessToken)
          val spotifyStatus = SpotifyStatus(loggedIn = true)
          userActor ! spotifyStatus
          requestor ! spotifyStatus
      }
    case SearchArtists(token, query, requestor) =>
      val searchResultFuture = searchArtists(query)
      searchResultFuture.onComplete {
        case Success(searchResult) =>
          requestor ! searchResult
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SearchError(query, t.getMessage)
      }
    case PlaybackStatusRequest(token, requestor) =>
      val playbackStatusFuture = currentPlaybackStatus()
      playbackStatusFuture.onComplete {
        case Success(playbackStatus) =>
          requestor ! playbackStatus
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)
      }
    case PlaybackRequest(token, PLAY, deviceId, uris, position, requestor) =>
      val playbackStatusFuture = play(deviceId, uris, position)
      playbackStatusFuture.onComplete {
        case Success(playbackStatus) =>
          requestor ! playbackStatus
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)

      }

    case PlaybackRequest(token, PAUSE, deviceId, _, _, requestor) =>
      val playbackStatusFuture = pause(deviceId)
      playbackStatusFuture.onComplete {
        case Success(playbackStatus) =>
          requestor ! playbackStatus
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)

      }

    case PlaybackRequest(token, NEXT, deviceId, _, _, requestor) =>
      val playbackStatusFuture = next(deviceId)
      playbackStatusFuture.onComplete {
        case Success(playbackStatus) =>
          requestor ! playbackStatus
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)

      }

    case PlaybackRequest(token, PREVIOUS, deviceId, _, _, requestor) =>
      val playbackStatusFuture = previous(deviceId)
      playbackStatusFuture.onComplete {
        case Success(playbackStatus) =>
          requestor ! playbackStatus
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)

      }

    case FetchAlbumRequest(token, albumUri, requestor) =>
      val fetchAlbumFutureFuture = fetchAlbum(albumUri)
      fetchAlbumFutureFuture.onComplete {
        case Success(album) =>
          requestor ! FetchAlbumResult(album)
        case Failure(t) =>
          log.error(t, t.getMessage)
          requestor ! SpotifyRequestError(t.getMessage)

      }
  }
}
