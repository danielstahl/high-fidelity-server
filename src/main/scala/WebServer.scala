
import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpMethods.{GET, HEAD, OPTIONS, POST, PUT}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directives, ExceptionHandler, RejectionHandler, Route}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.StdIn
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import models.mediaitem.{MediaItemGraphRoute, UserMediaItemsActorResponse, _}
import models.spotify._
import models.user._
import service.Firebase

import scala.collection.immutable.Seq


/**
  * Main web server
  */
object WebServer extends Directives
  with SpotifyUserJsonSupport
  with MediaItemJsonSupport
  with MediaItemTreeJsonSupport
  with UserJsonSupport
  with UserMediaItemJsonSupport {

  implicit val system = ActorSystem("high-fidelity-system")
  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher

  val mediaItemQueryTagActor = system.actorOf(Props[MediaItemQueryTagActor], "mediaItemQueryTagActor")

  implicit val timeout = Timeout(5 seconds) // needed for `?` below

  val firebase = Firebase(system.settings.config)
  firebase.initializeFirebase()

  val userSupervisorActor =
    system.actorOf(Props[UserSupervisorActor](new UserSupervisorActor(firebase)), "userSupervisorActor")

  val rejectionHandler = corsRejectionHandler withFallback RejectionHandler.default

  val exceptionHandler = ExceptionHandler {
    case e: NoSuchElementException => complete(StatusCodes.NotFound -> e.getMessage)
    case e: Throwable =>  complete(StatusCodes.InternalServerError -> e.getMessage)
  }

  val handleErrors = handleRejections(rejectionHandler) & handleExceptions(exceptionHandler)

  val corsSettings = CorsSettings.defaultSettings.copy(
    allowedMethods = Seq(GET, POST, PUT, HEAD, OPTIONS)
  )

  val mediaItemGraphRoute = MediaItemGraphRoute(userSupervisorActor)

  def main(args: Array[String]) = {

    val route: Route =
      path("spotify-login-callback") {
        get {
          parameters('code, 'state) { (code, state) => {
            val spotifyUserLogin: Future[SpotifyStatus] =
              (userSupervisorActor ? SpotifyLoginCallback(state, code, null)).mapTo[SpotifyStatus]

            val redirectHtml =
              spotifyUserLogin.map(loginStatus =>
                HttpEntity(ContentTypes.`text/html(UTF-8)`,
                  "<meta http-equiv=\"refresh\" content=\"0; url=http://localhost:3000?spotify=" + loginStatus.loggedIn+ "\" />"))

            complete(redirectHtml)

            }
          }
        }
      } ~ path("genre-tree" / Segment / Segment / Segment / Segment) { (theToken, treeType, currentType, current) =>
        get {
          val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(theToken)).mapTo[UserMediaItemsActorResponse]
          val mediaItemTreeFuture = userMediaItemsFuture.map(
            userMediaItems =>
              MediaItemTreeService.mediaItemTree(userMediaItems.mediaItems, current, currentType, treeType)
          )

          val response = mediaItemTreeFuture.flatMap {
            case mediItemTree: MediaItemTree =>
              Marshal(StatusCodes.OK -> mediItemTree).to[HttpResponse]
            //case error: FailureResponse =>
            //  Marshal(StatusCodes.InternalServerError -> error).to[HttpResponse]
          }
          complete(response)
        }
      } ~ path("media-items" / Segment ) { (theToken) =>
        get {
          parameters('type, 'tag.*) { (theType, tags) =>
            val splitTags = tags.map(tag => tag.split(":", 2))
            val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(theToken)).mapTo[UserMediaItemsActorResponse]
            val mediaItemsResponse = userMediaItemsFuture.map(
              userMediaItems =>
                MediItemQueryTagResponse(
                  userMediaItems.mediaItems.values
                    .filter(mediaItem =>
                      mediaItem.types.contains(theType))
                    .filter(mediaItem =>
                      splitTags.forall(splitTag =>
                        mediaItem.hasTag(splitTag(0), splitTag(1))))
                    .toSeq)
            )

            complete(mediaItemsResponse)
          }
        }
      } ~ path("firebase-token-login" / Segment ) { (theToken) =>
        get {
          val user = (userSupervisorActor ? LoginRequest(theToken)).mapTo[User]

          complete(user)
        }
      } ~ path("media-items" / Segment / Segment) { (theToken, theSlug) =>
        get {

          val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(theToken)).mapTo[UserMediaItemsActorResponse]

          val response = userMediaItemsFuture.map(
            userMediaItems => userMediaItems.mediaItems.get(theSlug) match {
              case Some(mediaItem) => Marshal(StatusCodes.OK -> mediaItem).to[HttpResponse]
              case None => Marshal(StatusCodes.NotFound -> "Media item not found").to[HttpResponse]
            }
          )
          complete(response)
        }
      } ~ path("media-items" / Segment ) { theToken =>
        post {
          entity(as[MediaItem]) {
            mediaItem =>
              val mediaItemCreateResult = userSupervisorActor ? UpdateMediaItem(theToken, mediaItem, ADD, null)
              val response = mediaItemCreateResult.flatMap {
                case mediaItemUpdateSuccess: MediaItemUpdateSuccess =>
                  Marshal(StatusCodes.OK -> mediaItemUpdateSuccess).to[HttpResponse]
                case mediaItemUpdateError: MediaItemUpdateError =>
                  Marshal(StatusCodes.InternalServerError -> mediaItemUpdateError).to[HttpResponse]
              }
              complete(response)
          }
        }

      } ~ path("media-items" / Segment / Segment) { (theToken, slugs) =>
        put {
          entity(as[MediaItem]) {
            mediaItem =>
              val mediaItemCreateResult = userSupervisorActor ? UpdateMediaItem(theToken, mediaItem, CHANGE, null)
              val response = mediaItemCreateResult.flatMap {
                case mediaItemUpdateSuccess: MediaItemUpdateSuccess =>
                  Marshal(StatusCodes.OK -> mediaItemUpdateSuccess).to[HttpResponse]
                case mediaItemUpdateError: MediaItemUpdateError =>
                  Marshal(StatusCodes.InternalServerError -> mediaItemUpdateError).to[HttpResponse]
              }
              complete(response)
          }
        }

      } ~ path("media-items" / Segment / Segment ) { (theToken, slugs) =>
        delete {
          val mediaItemCreateResult = userSupervisorActor ? RemoveMediaItem(theToken, slugs, null)
          val response = mediaItemCreateResult.flatMap {
            case mediaItemRemoveSuccess: MediaItemRemoveSuccess =>
              Marshal(StatusCodes.OK -> mediaItemRemoveSuccess).to[HttpResponse]
            case mediaItemRemoveError: MediaItemUpdateError =>
              Marshal(StatusCodes.InternalServerError -> mediaItemRemoveError).to[HttpResponse]
          }
          complete(response)
        }
      } ~ path("search" / "artist" / Segment ) { (theToken) =>
        get {
          parameters('query) { (query) =>
            val artistSearchResult = userSupervisorActor ? SearchArtists(theToken, query, null)
            val response = artistSearchResult.flatMap {
              case searchArtistResult: ArtistSearchResult =>
                Marshal(StatusCodes.OK -> searchArtistResult).to[HttpResponse]
              case searchError: SearchError =>
                Marshal(StatusCodes.InternalServerError -> searchError).to[HttpResponse]
            }
            complete(response)
          }
        }
      } ~ path("playback" / "status" / Segment ) { (theToken) =>
        get {
          val spotifyPlaybackStatusResult = userSupervisorActor ? PlaybackStatusRequest(theToken, null)
          val response = spotifyPlaybackStatusResult.flatMap {
            case playbackStatus: PlaybackStatus =>
              Marshal(StatusCodes.OK -> playbackStatus).to[HttpResponse]
            case spotifyRequestError: SpotifyRequestError =>
              Marshal(StatusCodes.InternalServerError -> spotifyRequestError).to[HttpResponse]
          }
          complete(response)
        }
      } ~ path("playback" / "play" / Segment ) { (theToken) =>
        put {
          parameters('device.?) { (device) =>
            entity(as[ExternalPlayRequest]) {
              case ExternalPlayRequest(uris, position) =>
                val spotifyPlaybackStatusResult = userSupervisorActor ? PlaybackRequest(theToken, PLAY, device, uris, position, null)
                val response = spotifyPlaybackStatusResult.flatMap {
                  case playbackStatus: PlaybackStatus =>
                    Marshal(StatusCodes.OK -> playbackStatus).to[HttpResponse]
                  case spotifyRequestError: SpotifyRequestError =>
                    Marshal(StatusCodes.InternalServerError -> spotifyRequestError).to[HttpResponse]
                }
                complete(response)
            }
          }

        }
      } ~ path("playback" / "pause" / Segment ) { (theToken) =>
        put {
          parameters('device.?) { (device) =>
            val spotifyPlaybackStatusResult = userSupervisorActor ? PlaybackRequest(theToken, PAUSE, device, None, None, null)
            val response = spotifyPlaybackStatusResult.flatMap {
              case playbackStatus: PlaybackStatus =>
                Marshal(StatusCodes.OK -> playbackStatus).to[HttpResponse]
              case spotifyRequestError: SpotifyRequestError =>
                Marshal(StatusCodes.InternalServerError -> spotifyRequestError).to[HttpResponse]
            }
            complete(response)
          }
        }
      } ~ path("playback" / "next" / Segment ) { (theToken) =>
        put {
          parameters('device.?) { (device) =>
            val spotifyPlaybackStatusResult = userSupervisorActor ? PlaybackRequest(theToken, NEXT, device, None, None, null)
            val response = spotifyPlaybackStatusResult.flatMap {
              case playbackStatus: PlaybackStatus =>
                Marshal(StatusCodes.OK -> playbackStatus).to[HttpResponse]
              case spotifyRequestError: SpotifyRequestError =>
                Marshal(StatusCodes.InternalServerError -> spotifyRequestError).to[HttpResponse]
            }
            complete(response)
          }
        }
      } ~ path("playback" / "previous" / Segment ) { (theToken) =>
        put {
          parameters('device.?) { (device) =>
            val spotifyPlaybackStatusResult = userSupervisorActor ? PlaybackRequest(theToken, PREVIOUS, device, None, None, null)
            val response = spotifyPlaybackStatusResult.flatMap {
              case playbackStatus: PlaybackStatus =>
                Marshal(StatusCodes.OK -> playbackStatus).to[HttpResponse]
              case spotifyRequestError: SpotifyRequestError =>
                Marshal(StatusCodes.InternalServerError -> spotifyRequestError).to[HttpResponse]
            }
            complete(response)
          }
        }
      } ~ mediaItemGraphRoute()



    val fullRoute = handleErrors {
      cors(corsSettings) {
        route
      }
    }

    val bindingFuture = Http().bindAndHandle(fullRoute, "0.0.0.0", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine()

    bindingFuture
      .flatMap(binding => binding.unbind())
      .onComplete(_ => system.terminate())
  }
}
