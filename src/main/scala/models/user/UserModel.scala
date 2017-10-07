package models.user

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.DefaultJsonProtocol


case class User(uid: String, email: String, loggedIn: Boolean, spotify: Boolean)

case class UserProfile(uid: String, spotifyAccessToken: Option[String], spotifyRefreshToken: Option[String])

trait UserJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val userFormat = jsonFormat4(User)
  implicit val userProfileFormat = jsonFormat3(UserProfile)
}