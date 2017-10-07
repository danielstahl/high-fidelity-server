package service

import com.google.firebase.{FirebaseApp, FirebaseOptions}
import com.google.firebase.auth.{FirebaseAuth, FirebaseCredentials, FirebaseToken}
import com.google.firebase.database.{DatabaseReference, FirebaseDatabase}
import com.typesafe.config.Config

import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
  * Main loader for the firebase db
  */
case class Firebase(config: Config) {

  def initializeFirebase(): Unit = {

    val serviceAccount = getClass.getResourceAsStream("/firebase-service-account.json")

    val options = new FirebaseOptions.Builder()
      .setCredential(FirebaseCredentials.fromCertificate(serviceAccount))
      .setDatabaseUrl("https://high-fidelity-676ee.firebaseio.com")
      .build()
    FirebaseApp.initializeApp(options)

    println(s"Initialized database ${FirebaseApp.getInstance().getName}")
  }

  def verifyIdToken(idToken: String): Future[FirebaseToken] = {
    val promise = Promise[FirebaseToken]()
    FirebaseAuth.getInstance().verifyIdToken(idToken)
        .addOnCompleteListener(task => {
          if(task.isSuccessful) {
            promise.complete(Try(task.getResult))
          } else {
            promise.failure(task.getException)
          }
        })
    promise.future
  }

  def getDatebaseReference(path: String): DatabaseReference =
    FirebaseDatabase.getInstance()
        .getReference(path)
}
