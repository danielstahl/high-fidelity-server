package models.spotify

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol

case class SpotifyError(status: Int, message: String)

case class SpotifyErrorStatus(error: SpotifyError)

case class ExternalUrls(spotify: String)
case class ExternalIds(isrc: Option[String], ean: Option[String], upc: Option[String])

case class Followers(href: Option[String], total: Int)
case class Image(url: String, height: Int, width: Int)
case class Copyright(text: String, `type`: String)
case class SpotifyTrackLink(external_urls: ExternalUrls, href: String, id: String, `type`: String, uri: String)

case class SpotifySimpleTrack(artists: Seq[SpotifySimpleArtist],
                              available_markets: Seq[String],
                              disc_number: Int,
                              duration_ms: Int,
                              explicit: Boolean,
                              external_urls: ExternalUrls,
                              href: String,
                              id: String,
                              is_playable: Option[Boolean],
                              linked_from: Option[SpotifyTrackLink],
                              name: String,
                              preview_url: Option[String],
                              track_number: Int,
                              `type`: String,
                              uri: String)

case class PagingObject[T](href: String,
                           items: Seq[T],
                           limit: Int,
                           next: Option[String],
                           offset: Int,
                           previous: Option[String],
                           total: Int)

case class SpotifyArtist(external_urls: ExternalUrls,
                         followers: Followers,
                         genres: Seq[String],
                         href: String,
                         images: Seq[Image],
                         name: String,
                         popularity: Int,
                         `type`: String,
                         uri: String)

case class SpotifySimpleArtist(external_urls: ExternalUrls,
                               href: String,
                               id: String,
                               name: String,
                               `type`: String,
                               uri: String)

case class SpotifySimpleAlbum(album_type: String,
                              artists: Seq[SpotifySimpleArtist],
                              available_markets: Option[Seq[String]],
                              external_urls: ExternalUrls,
                              href: String,
                              id: String,
                              images: Seq[Image],
                              name: String,
                              `type`: String,
                              uri: String)


case class SpotifyAlbum(album_type: String,
                        artists: Seq[SpotifySimpleArtist],
                        available_markets: Option[Seq[String]],
                        copyrights: Seq[Copyright],
                        external_ids: ExternalIds,
                        external_urls: ExternalUrls,
                        genres: Seq[String],
                        href: String,
                        id: String,
                        images: Seq[Image],
                        label: String,
                        name: String,
                        popularity: Int,
                        release_date: String,
                        release_date_precision: String,
                        tracks: PagingObject[SpotifySimpleTrack],
                        `type`: String,
                        uri: String)

case class SpotifyTrack(album: SpotifySimpleAlbum,
                        artists: Seq[SpotifySimpleArtist],
                        available_markets: Option[Seq[String]],
                        disc_number: Int,
                        duration_ms: Long,
                        explicit: Boolean,
                        external_urls: ExternalUrls,
                        href: String,
                        id: String,
                        is_playable: Option[Boolean],
                        linked_from: Option[SpotifyTrackLink],
                        name: String,
                        popularity: Int,
                        preview_url: Option[String],
                        track_number: Int,
                        `type`: String,
                        uri: String)

case class SpotifyArtistSearchResult(href: String, items: Seq[SpotifyArtist])

case class SpotifySearchResult(artists: Option[SpotifyArtistSearchResult])

case class SpotifyDevice(id: Option[String],
                         is_active: Boolean,
                         is_restricted: Boolean,
                         name: String,
                         `type`: String,
                         volume_percent: Option[Int])

case class SpotifyContext(uri: Option[String], href: Option[String], external_urls: Option[ExternalUrls], `type`: String)

case class SpotifyPlaybackStatus(device: SpotifyDevice,
                                 repeat_state: String,
                                 shuffle_state: Boolean,
                                 context: SpotifyContext,
                                 timestamp: Long,
                                 progress_ms: Int,
                                 is_playing: Boolean,
                                 item: Option[SpotifyTrack])


case class SpotifyPlayContextOffset(position: Option[Int], uri: Option[String])

case class SpotifyPlayContext(context_uri: Option[String], uris: Option[Seq[String]], offset: Option[SpotifyPlayContextOffset])

trait SpotifyModelJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val spotifyErrorFormat = jsonFormat2(SpotifyError)
  implicit val spotifyErrorStatusFormat = jsonFormat1(SpotifyErrorStatus)
  implicit val externalUrlsFormat = jsonFormat1(ExternalUrls)
  implicit val externalidsFormat = jsonFormat3(ExternalIds)
  implicit val followersFormat = jsonFormat2(Followers)
  implicit val imageFormat = jsonFormat3(Image)
  implicit val spotifyArtistFormat = jsonFormat9(SpotifyArtist)
  implicit val spotifyArtistSearchResultFormat = jsonFormat2(SpotifyArtistSearchResult)
  implicit val spotifySearchResultFormat = jsonFormat1(SpotifySearchResult)
  implicit val spotifySimpleArtistFormat = jsonFormat6(SpotifySimpleArtist)
  implicit val spotifySimpleAlbumFormat = jsonFormat10(SpotifySimpleAlbum)
  implicit val spotifyTrackLinkFormat = jsonFormat5(SpotifyTrackLink)
  implicit val spotifyTrackFormat = jsonFormat17(SpotifyTrack)
  implicit val spotifyDeviceFormat = jsonFormat6(SpotifyDevice)
  implicit val spotifyContextFormat = jsonFormat4(SpotifyContext)
  implicit val spotifyPlaybackStatusFormat = jsonFormat8(SpotifyPlaybackStatus)
  implicit val spotifyPlayContextOffsetFormat = jsonFormat2(SpotifyPlayContextOffset)
  implicit val spotifyPlayContextFormat = jsonFormat3(SpotifyPlayContext)
  implicit val spotifyCopyrightFormat = jsonFormat2(Copyright)
  implicit val spotifySimpleTrackFormat = jsonFormat15(SpotifySimpleTrack)
  implicit val pagingObjectFormat = jsonFormat7(PagingObject[SpotifySimpleTrack])
  implicit val spotifyAlbumFormat = jsonFormat18(SpotifyAlbum)
}
