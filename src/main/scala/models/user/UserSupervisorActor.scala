package models.user

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.google.firebase.auth.FirebaseToken
import models.mediaitem.{ADD, UpdateMediaItem, UserMediaItemActor, UserMediaItemsActorRequest}
import models.spotify._
import service.Firebase

case class UserRequest(uid: String, requestor: ActorRef)

class UserActor(var user: User, firebase: Firebase) extends Actor with ActorLogging {

  val userMediaItemActor = context.actorOf(Props[UserMediaItemActor](new UserMediaItemActor(user.uid, firebase)), "userMediaItemActor")
  val spotifyUserActor = context.actorOf(Props[SpotifyUserActor](new SpotifyUserActor(self, Option.empty)), "spotifyUserActor")

  def receive = {
    case UserRequest(uid, requestor) =>
      requestor ! user
    case userMediaItemRequest: UserMediaItemsActorRequest =>
      userMediaItemActor ! userMediaItemRequest
    case updateMediaItem: UpdateMediaItem =>
      userMediaItemActor ! updateMediaItem
    case SpotifyStatus(loggedIn) =>
      this.user = user.copy(spotify = loggedIn)
    case spotifyLoginCallback: SpotifyLoginCallback =>
      spotifyUserActor ! spotifyLoginCallback
    case searchArtists: SearchArtists =>
      spotifyUserActor ! searchArtists
    case playbackStatusRequest: PlaybackStatusRequest =>
      spotifyUserActor ! playbackStatusRequest
    case playbackRequest: PlaybackRequest =>
      spotifyUserActor ! playbackRequest
    case fetchAlbumRequest: FetchAlbumRequest =>
      spotifyUserActor ! fetchAlbumRequest
  }
}

case class LoginRequest(token: String)
case class UserMediaItemsRequest(token: String)

class UserSupervisorActor(firebase: Firebase) extends Actor with ActorLogging {
  import context.dispatcher

  def firebaseTokenToUser(firebaseToken: FirebaseToken): User =
    User(uid = firebaseToken.getUid, email = firebaseToken.getEmail, loggedIn = true, spotify = false)

  def userActorFromToken(firebaseToken: FirebaseToken): UserActor =
    new UserActor(firebaseTokenToUser(firebaseToken), firebase)

  def makeUserActor(firebaseToken: FirebaseToken): ActorRef = {
    context.child(firebaseToken.getUid)
      .getOrElse(context.actorOf(Props[UserActor](userActorFromToken(firebaseToken)), firebaseToken.getUid))
  }

  def receive = {
    case LoginRequest(token) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! UserRequest(firebaseToken.getUid, requestor)
        })
    case UserMediaItemsRequest(token) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
          .foreach(firebaseToken => {
            val userActor = makeUserActor(firebaseToken)
            userActor ! UserMediaItemsActorRequest(firebaseToken.getUid, requestor)
          })

    case UpdateMediaItem(token, mediaItem, operation, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! UpdateMediaItem(token, mediaItem, operation, requestor)
        })
    case SpotifyLoginCallback(token, code, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! SpotifyLoginCallback(token, code, requestor)
        })
    case SearchArtists(token, query, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! SearchArtists(token, query, requestor)
        })
    case PlaybackStatusRequest(token, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! PlaybackStatusRequest(token, requestor)
        })
    case PlaybackRequest(token, command, deviceId, uris, position, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! PlaybackRequest(token, command, deviceId, uris, position, requestor)
        })

    case FetchAlbumRequest(token, albumUri, _) =>
      val requestor = sender()
      firebase.verifyIdToken(token)
        .foreach(firebaseToken => {
          val userActor = makeUserActor(firebaseToken)
          userActor ! FetchAlbumRequest(token, albumUri, requestor)
        })
  }
}
