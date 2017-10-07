package models.mediaitem

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import models.user.UserMediaItemsRequest
import akka.pattern.ask
import akka.util.Timeout
import models.spotify._

import scala.concurrent.duration._
import spray.json.{DefaultJsonProtocol, NullOptions}

import scala.concurrent.ExecutionContext

trait GraphType {
  val graphType: String
}

case class Uri(uriType: String, uri: String, url: String, name: String)

case class Genre(slugs: String, name: String)

case class Instrument(slugs: String, name: String)

case class MusicalForm(slugs: String, name: String)

case class Era(slugs: String, name: String, genre: String)

case class Composer(slugs: String, name: String, era: String)

case class Piece(slugs: String, name: String, composer: String, musicalForms: Seq[String], instruments: Seq[String])

case class Artist(slugs: String, name: String, instrument: Option[String], genre: String)

case class Album(slugs: String, name: String, artists: Seq[String], composers: Seq[String])

case class RootGraph(genres: Seq[Genre], graphType: String = "root")

case class GenreGraph(genre: Genre,
                      uris: Seq[Uri],
                      instruments: Seq[InstrumentGraph],
                      eras: Seq[Era],
                      artists: Seq[Artist],
                      graphType: String = "genre") extends GraphType

case class InstrumentGraph(instrument: Instrument, artists: Seq[Artist], graphType: String = "instrument") extends GraphType

case class EraGraph(era: Era, genre: Genre, uris: Seq[Uri], composers: Seq[Composer], graphType: String = "era") extends GraphType

case class ArtistGraph(artist: Artist, genre: Genre, instrument: Seq[Instrument], uris: Seq[Uri], albums: Seq[AlbumGraph], graphType: String = "artist") extends GraphType

case class ComposerGraph(composer: Composer, era: Era, genre: Genre, albums: Seq[Album], pieces: Seq[Piece], graphType: String = "composer") extends GraphType

case class PieceGraph(piece: Piece, composer: Composer, musicalForms: Seq[MusicalForm], instruments: Seq[Instrument], graphType: String = "piece") extends GraphType

case class AlbumGraph(album: Album, artists: Seq[Artist], composers: Seq[Composer], uris: Seq[Uri], graphType: String = "album") extends GraphType

case class AlbumArtistInfo(spotifyUri: String, name: String, slugs: Option[String], artistTypes: Seq[String])

case class AlbumInfo(spotifyUri: String, name: String, imageUri: Option[String], artists: Seq[AlbumArtistInfo])

trait MediaItemGraphJsonSupport extends SprayJsonSupport with DefaultJsonProtocol with NullOptions {
  implicit val uriFormat = jsonFormat4(Uri)
  implicit val genreFormat = jsonFormat2(Genre)
  implicit val instrumentFormat = jsonFormat2(Instrument)
  implicit val musicalFormFormat = jsonFormat2(MusicalForm)
  implicit val eraFormat = jsonFormat3(Era)
  implicit val artistFormat = jsonFormat4(Artist)
  implicit val composerFormat = jsonFormat3(Composer)
  implicit val albumFormat = jsonFormat4(Album)
  implicit val rootGraphFormat = jsonFormat2(RootGraph)
  implicit val instrumentGraphFormat = jsonFormat3(InstrumentGraph)
  implicit val genreGraphFormat = jsonFormat6(GenreGraph)
  implicit val eraGraphFormat = jsonFormat5(EraGraph)
  implicit val albumGraphFormat = jsonFormat5(AlbumGraph)
  implicit val artistGraphFormat = jsonFormat6(ArtistGraph)
  implicit val albumArtistInfoFormat = jsonFormat4(AlbumArtistInfo)
  implicit val albumInfoFormat = jsonFormat4(AlbumInfo)
}

case class MediaItemGraphRoute(userSupervisorActor: ActorRef)(implicit ec: ExecutionContext) extends Directives with MediaItemGraphJsonSupport {

  implicit val timeout = Timeout(5 seconds)

  def toGenre(mediaItem: MediaItem): Genre =
    Genre(mediaItem.slugs, mediaItem.name)

  def allGenres(userMediaItems: Iterable[MediaItem]): RootGraph = {
    RootGraph(
      userMediaItems
        .filter(mediaItem => mediaItem.types.contains("genre"))
        .map(mediaItem => toGenre(mediaItem))
        .toSeq)
  }

  def toInstrument(mediaItem: MediaItem): Instrument =
    Instrument(mediaItem.slugs, mediaItem.name)

  def toMusicalForm(mediaItem: MediaItem): MusicalForm =
    MusicalForm(mediaItem.slugs, mediaItem.name)

  def toEra(mediaItem: MediaItem): Era =
    Era(mediaItem.slugs, mediaItem.name, mediaItem.getTag("genre").head)

  def toArtist(mediaItem: MediaItem): Artist =
    Artist(mediaItem.slugs, mediaItem.name, mediaItem.getTag("instrument").headOption, mediaItem.getTag("genre").head)

  def toComposer(mediaItem: MediaItem): Composer =
    Composer(mediaItem.slugs, mediaItem.name, mediaItem.getTag("era").head)

  def toAlbum(mediaItem: MediaItem): Album =
    Album(mediaItem.slugs, mediaItem.name, mediaItem.getTag("artist"), mediaItem.getTag("composer"))

  def toUris(mediaItem: MediaItem, uriInfos: Map[String, UriInfo]): Seq[Uri] =
    mediaItem.uris.keys.flatMap(uriType =>
      mediaItem.uris(uriType).map(uri =>
        uriInfos.get(uri)
          .map(uriInfo => Uri(uriInfo.uriType, uriInfo.uri, uriInfo.url, uriInfo.name))
          .getOrElse(Uri(uriType, uri, uri, uri))))
      .toSeq

  def toAlbumGraph(userMediaItemsResponse: UserMediaItemsActorResponse, mediaItem: MediaItem): AlbumGraph = {
    val userMediaItems = userMediaItemsResponse.mediaItems
    val uriInfos = userMediaItemsResponse.uriInfos

    val artists = mediaItem.tags.get("artist")
        .map(artists =>
          artists.map(
            artistSlugs => userMediaItems(artistSlugs))
          .map(mediaItem => toArtist(mediaItem)))
          .getOrElse(Seq.empty)

    val composers = mediaItem.tags.get("composer")
      .map(composers =>
        composers.map(
          composerSlugs => userMediaItems(composerSlugs))
          .map(mediaItem => toComposer(mediaItem)))
      .getOrElse(Seq.empty)

    val uris = toUris(mediaItem, uriInfos)

    AlbumGraph(
      album = toAlbum(mediaItem),
      artists = artists,
      composers = composers,
      uris = uris
    )
  }

  def getEraGraph(userMediaItemsResponse: UserMediaItemsActorResponse, eraSlugs: String): Option[EraGraph] = {
    val userMediaItems = userMediaItemsResponse.mediaItems
    val uriInfos = userMediaItemsResponse.uriInfos
    val optionalEra = userMediaItems.get(eraSlugs)
    optionalEra.map(
      era => {
        val genreSlug = era.getTag("genre").head

        val genre = userMediaItems(genreSlug)

        val uris = toUris(era, uriInfos)

        val composers = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("composer") &&
              mediaItem.hasTag("genre", genre.slugs) &&
              mediaItem.hasTag("era", era.slugs))

        EraGraph(
          era = toEra(era),
          genre = toGenre(genre),
          uris = uris,
          composers = composers.map(mediaItem => toComposer(mediaItem)).toSeq)
      }
    )

  }

  def getGenreGraph(userMediaItemsResponse: UserMediaItemsActorResponse, slugs: String): Option[GenreGraph] = {
    val userMediaItems = userMediaItemsResponse.mediaItems
    val uriInfos = userMediaItemsResponse.uriInfos

    val optionalGenre = userMediaItems.get(slugs)
    optionalGenre.map(
      genre => {
        val instruments = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("instrument") && mediaItem.hasTag("genre", slugs))
        val musicalForms = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("form") && mediaItem.hasTag("genre", slugs))
        val eras = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("era") && mediaItem.hasTag("genre", slugs))
        val artists = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("artist") && mediaItem.hasTag("genre", slugs))

        val instrumentGraphs = instruments
          .map(
            instrument =>
              InstrumentGraph(instrument = toInstrument(instrument),
                artists = artists
                  .filter(_.hasTag("instrument", instrument.slugs))
                  .map(toArtist)
                  .toSeq))
          .filter(instrumentGraph => instrumentGraph.artists.nonEmpty)
          .toSeq

        val artistsWithoutInstruments = artists
          .filter(artist => !artist.tags.contains("instrument"))
          .map(toArtist)
          .toSeq

        val uris = toUris(genre, uriInfos)

        GenreGraph(
          genre = toGenre(genre),
          uris = uris,
          instruments = instrumentGraphs,
          eras = eras.map(mediaItem => toEra(mediaItem)).toSeq,
          artists = artistsWithoutInstruments
        )
      }
    )
  }

  def getArtistGraph(userMediaItemsResponse: UserMediaItemsActorResponse, slugs: String): Option[ArtistGraph] = {
    val userMediaItems = userMediaItemsResponse.mediaItems
    val uriInfos = userMediaItemsResponse.uriInfos

    val optionalArtist = userMediaItems.get(slugs)

    optionalArtist.map(
      artist => {
        val genreSlug = artist.getTag("genre").head

        val genre = userMediaItems(genreSlug)

        val uris = toUris(artist, uriInfos)

        val instruments = artist.tags.get("instrument")
          .map(instr => instr.map(instrumentSlugs => userMediaItems(instrumentSlugs)))
          .getOrElse(List.empty)

        val albums = userMediaItems.values
          .filter(mediaItem =>
            mediaItem.types.contains("album") && mediaItem.hasTag("artist", slugs))

        ArtistGraph(
          artist = toArtist(artist),
          instrument = instruments.map(instr => toInstrument(instr)),
          genre = toGenre(genre),
          uris = uris,
          albums = albums.map(album => toAlbumGraph(userMediaItemsResponse, album)).toSeq
        )
      }
    )
  }

  def getLastImage(spotifyAlbum: SpotifyAlbum): Option[String] = {
    if(spotifyAlbum.images.isEmpty) {
      Option.empty
    } else {
      Option(spotifyAlbum.images.last.url)
    }
  }

  def getAlbumArtistInfo(userMediaItemsResponse: UserMediaItemsActorResponse, spotifyArtist: SpotifySimpleArtist): AlbumArtistInfo = {
    val userMediaItems = userMediaItemsResponse.mediaItems
    val optionalMediaItem =
      userMediaItems.values.find(mediaItem => mediaItem.hasUri("spotifyUri", spotifyArtist.uri))

    optionalMediaItem match {
      case Some(mediaItem) =>
        AlbumArtistInfo(spotifyUri = spotifyArtist.uri, name = mediaItem.name, Option(mediaItem.slugs), mediaItem.types)
      case None =>
        AlbumArtistInfo(spotifyUri = spotifyArtist.uri, spotifyArtist.name, Option.empty, Seq.empty)
    }
  }

  def getAlbumInfo(userMediaItemsResponse: UserMediaItemsActorResponse, spotifyAlbum: SpotifyAlbum): AlbumInfo = {
    val albumArtists = spotifyAlbum.artists.map(spotifyArtist => getAlbumArtistInfo(userMediaItemsResponse, spotifyArtist))
    AlbumInfo(spotifyUri = spotifyAlbum.uri, name = spotifyAlbum.name, imageUri = getLastImage(spotifyAlbum), albumArtists)
  }

  val allGenresRoute = (get & path("genres") & headerValueByName("X-user-token")) {
    userToken => {
      val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(userToken)).mapTo[UserMediaItemsActorResponse]
      val genresResponse = userMediaItemsFuture.flatMap(
        userMediaItems => {
          val rootGraph = allGenres(userMediaItems.mediaItems.values)
          Marshal(StatusCodes.OK -> rootGraph).to[HttpResponse]
        }
      )
      complete(genresResponse)
    }
  }

  val genreRoute = (get & path("genres" / Segment) & headerValueByName("X-user-token")) {
    (genreSlug, userToken) => {
      val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(userToken)).mapTo[UserMediaItemsActorResponse]
      val genreResponse = userMediaItemsFuture.flatMap(
        userMediaItems => {
          val optionalGenreGraph = getGenreGraph(userMediaItems, genreSlug)
          optionalGenreGraph match {
            case Some(genreGraph) => Marshal(StatusCodes.OK -> genreGraph).to[HttpResponse]
            case None => Marshal(StatusCodes.NotFound -> s"Genre $genreSlug not found").to[HttpResponse]
          }
        }
      )
      complete(genreResponse)
    }
  }

  val eraRoute = (get & path("eras" / Segment) & headerValueByName("X-user-token")) {
    (eraSlug, userToken) => {
      val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(userToken)).mapTo[UserMediaItemsActorResponse]
      val eraResponse = userMediaItemsFuture.flatMap(
        userMediaItems => {
          val optionalEraGraph = getEraGraph(userMediaItems, eraSlug)
          optionalEraGraph match {
            case Some(eraGraph) => Marshal(StatusCodes.OK -> eraGraph).to[HttpResponse]
            case None => Marshal(StatusCodes.NotFound -> s"Era $eraSlug not found").to[HttpResponse]
          }
        }
      )
      complete(eraResponse)
    }
  }

  val artistRoute = (get & path("artists" / Segment) & headerValueByName("X-user-token")) {
    (artistSlug, userToken) => {
      val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(userToken)).mapTo[UserMediaItemsActorResponse]
      val artistResponse = userMediaItemsFuture.flatMap(
        userMediaItems => {
          val optionalArtistGraph = getArtistGraph(userMediaItems, artistSlug)
          optionalArtistGraph match {
            case Some(artistGraph) => Marshal(StatusCodes.OK -> artistGraph).to[HttpResponse]
            case None => Marshal(StatusCodes.NotFound -> s"Era $artistSlug not found").to[HttpResponse]
          }
        }
      )
      complete(artistResponse)
    }
  }

  val albumInfoRoute = (get & path("albums" / "info" / Segment) & headerValueByName("X-user-token")) {
    (albumUri, userToken) => {
      val userMediaItemsFuture = (userSupervisorActor ? UserMediaItemsRequest(userToken)).mapTo[UserMediaItemsActorResponse]
      val spotifyAlbumFuture = (userSupervisorActor ? FetchAlbumRequest(userToken, albumUri, null)).mapTo[FetchAlbumResult]
      val albumInfo = for {
        userMediaItems <- userMediaItemsFuture
        spotifyAlbum <- spotifyAlbumFuture
      } yield getAlbumInfo(userMediaItems, spotifyAlbum.album)
      complete(albumInfo)
    }
  }

  def apply(): Route = {
    allGenresRoute ~ genreRoute ~ eraRoute ~ artistRoute ~ albumInfoRoute
  }
}
