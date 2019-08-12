package drs.localizer

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Paths}

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.http.scaladsl.unmarshalling._
import akka.stream.ActorMaterializer
import cats.effect.{ExitCode, IO, IOApp}
import com.google.auth.oauth2.GoogleCredentials
import com.google.cloud.storage.Storage.BlobGetOption
import com.google.cloud.storage.{Blob, StorageException, StorageOptions}
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import drs.localizer.MarthaResponseJsonSupport._
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

object DrsLocalizerMain extends IOApp {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val logger = LoggerFactory.getLogger("DrsLocalizerLogger")

  val RequesterPaysErrorMsg = "Bucket is requester pays bucket but no user project provided."

  val CloudPlatformAuthScope = "https://www.googleapis.com/auth/cloud-platform"
  val UserInfoEmailScope = "https://www.googleapis.com/auth/userinfo.email"
  val UserInfoProfileScope = "https://www.googleapis.com/auth/userinfo.profile"

  val GcsScheme = "gs://"


  def resolveDrsThroughMartha(drsUrl: String, marthaUrl: Uri): IO[MarthaResponse] = {

    val requestBody = raw"""{"url":"$drsUrl"}"""
    val scopes = List(UserInfoEmailScope, UserInfoProfileScope)

    val credentials = GoogleCredentials.getApplicationDefault()
    val scopedCredentials = credentials.createScoped(scopes.asJava)
    val accessToken = scopedCredentials.refreshAccessToken().getTokenValue

    val requestHeader = List[HttpHeader](Authorization(OAuth2BearerToken(accessToken)))
    val requestEntity = HttpEntity(ContentTypes.`application/json`, requestBody)
    val httpRequest = HttpRequest(method = HttpMethods.POST, uri = marthaUrl, headers = requestHeader, entity = requestEntity)

    for {
      httpResponse <- IO.fromFuture(IO(Http().singleRequest(httpRequest)))
      marthaResponse <- httpResponse.status match {
        case StatusCodes.OK => {
          logger.info("Received successful response from Martha")
          IO.fromFuture(IO(Unmarshal(httpResponse).to[MarthaResponse]))
        }
        case _ => {
          //TODO: Saloni- need to fix this
          val errorMsg = Unmarshal(httpResponse).to[MarthaErrorResponse] onComplete {
            case Success(samResponse) => samResponse.status.toString
            case Failure(e) => s"Failed to parse the response sent from Sam. Error: $e"
          }
          IO.raiseError(new Exception(s"Something went wrong while trying to resolve $drsUrl through Martha $marthaUrl. " +
            s"Expected 200 got ${httpResponse.status}. Response from Sam: $errorMsg"))
        }
      }
    } yield marthaResponse
  }


  def extractFirstGcsUrl(urlArray: Array[Url]): IO[String] = {
    val urlObjOption = urlArray.find(urlObj => urlObj.url.startsWith(GcsScheme))

    urlObjOption match {
      case Some(urlObj) => IO(urlObj.url)
      case None => IO.raiseError(new Exception("No resolved url starting with 'gs://' found from Martha response!"))
    }
  }


  def downloadFileFromGcs(gcsUrl: String,
                          serviceAccountJsonOption: Option[String],
                          downloadLoc: String,
                          requesterPaysProjectIdOption: Option[String]) : IO[Unit] = {
    logger.info(s"Requester Pays project ID is $requesterPaysProjectIdOption")

    val gcsUrlArray = gcsUrl.replace(GcsScheme, "").split("/", 2)
    val fileToBeLocalized = gcsUrlArray(1)
    val gcsBucket = gcsUrlArray(0)

    val unscopedCredentials = serviceAccountJsonOption match {
      case None => GoogleCredentials.getApplicationDefault()
      case Some(serviceAccountJson) =>
        GoogleCredentials.fromStream(new ByteArrayInputStream(serviceAccountJson.getBytes()))
    }

    val credentials = unscopedCredentials.createScoped(List(CloudPlatformAuthScope).asJava)

    val storage = StorageOptions.newBuilder().setCredentials(credentials).build().getService
    Files.createDirectories(Paths.get(downloadLoc).getParent)

    logger.info(s"Attempting to download $gcsUrl")

    IO.delay {
      val blob = storage.get(gcsBucket, fileToBeLocalized)
      blob.downloadTo(Paths.get(downloadLoc))
      logger.info(s"Download complete without using Requester Pays")
    } handleErrorWith { throwable =>
      (requesterPaysProjectIdOption, throwable) match {
        case (Some(requesterPaysProjectId), storageException: StorageException)
          if storageException.getMessage == RequesterPaysErrorMsg => {
          logger.info(s"Received 'Bucket is requester pays' error. Attempting again using Requester Pays billing project")
          IO.delay {
            val blob = storage.get(gcsBucket, fileToBeLocalized, BlobGetOption.userProject(requesterPaysProjectId))
            blob.downloadTo(Paths.get(downloadLoc), Blob.BlobSourceOption.userProject(requesterPaysProjectId))
          }
        }
        case _ => IO.raiseError(throwable)
      }
    }
  }


  //args(0) - dos url
  //args(1) - download location
  //args(2) - martha url for now


  override def run(args: List[String]): IO[ExitCode] = {
    val existStateIO = for {
      marthaUri <- IO(Uri(args(2)))
      marthaResponse <- resolveDrsThroughMartha(args.head, marthaUri)
      gcsUrl <- extractFirstGcsUrl(marthaResponse.dos.data_object.urls)
      exitState <- downloadFileFromGcs(gcsUrl, marthaResponse.googleServiceAccount.map(_.data.toString), args(1), None)
      _  = system.terminate()
    } yield exitState


    existStateIO.map { s =>
      println(s)
      ExitCode.Success
    }
  }
}
