package cromwell.cloudsupport.auth

import java.io.{ByteArrayInputStream, FileNotFoundException, InputStream}
import java.net.HttpURLConnection._

import better.files.File
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.HttpResponseException
import com.google.auth.http.HttpTransportFactory
import com.google.auth.oauth2.{GoogleCredentials, OAuth2Credentials, ServiceAccountCredentials, UserCredentials}
import cromwell.cloudsupport.auth.AuthMode.{GoogleCredentialsValidation, OptionLookup, OptionLookupException}
import org.slf4j.LoggerFactory
import com.google.cloud.NoCredentials
import cromwell.cloudsupport.auth.ServiceAccountMode.{CredentialFileFormat, JsonFileFormat, PemFileFormat}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

sealed trait GoogleAuthMode extends AuthMode[OAuth2Credentials] {

  private[cloudsupport] var credentialsValidation : GoogleCredentialsValidation = (credentials: OAuth2Credentials) =>
    Try { refreshCredentials(credentials)
    credentials
    }

  lazy val log = LoggerFactory.getLogger(getClass.getSimpleName)

  /**
   * Creates OAuth credentials with the specified scopes.
   */
  def credentials(options: OptionLookup, scopes: Iterable[String]): OAuth2Credentials

  /** Used for both checking that the credential is valid and creating a fresh credential. */

  private[auth] def refreshCredentials(credentials: OAuth2Credentials): Unit = credentials.refresh()

  /**
   * Alias for credentials(GoogleAuthMode.NoOptionLookup, scopes).
   * Only valid for credentials that are NOT externally provided, such as ApplicationDefault.
   */
  def credentials(scopes: Iterable[String]): OAuth2Credentials = credentials(GoogleAuthMode.NoOptionLookup, scopes)


  /**
   * Alias for credentials(GoogleAuthMode.NoOptionLookup, Nil).
   * Only valid for credentials that are NOT externally provided and do not need scopes, such as ApplicationDefault.
   */
  private[cloudsupport] def credentials(): OAuth2Credentials = credentials(GoogleAuthMode.NoOptionLookup, Nil)


  /**
   * Alias for credentials(options, Nil).
   * Only valid for credentials that are NOT externally provided and do not need scopes, such as ApplicationDefault.
   */
  override def credentials(options: OptionLookup): OAuth2Credentials = credentials(options, Nil)

  def requiresAuthFile: Boolean = false

  /**
   * Enables swapping out credential validation for various testing purposes ONLY.
   *
   * All traits in this file are sealed, all classes final, meaning things like Mockito or other java/scala overrides
   * cannot work.
   */
}

object GoogleAuthMode {

  private[cloudsupport] val NoCredentialsValidation = mouse.ignore _

  def NoOptionLookup(string: String): Nothing =
    throw new UnsupportedOperationException(s"cannot lookup $string")

  lazy val httpTransport = GoogleNetHttpTransport.newTrustedTransport
  lazy val HttpTransportFactory = new HttpTransportFactory {
    override def create() = httpTransport
  }

  val RefreshTokenOptionKey = "refresh_token"
  val UserServiceAccountKey = "user_service_account_json"
  val DockerCredentialsEncryptionKeyNameKey = "docker_credentials_key_name"
  val DockerCredentialsTokenKey = "docker_credentials_token"

  def checkReadable(file: File) = {
    if (!file.isReadable) throw new FileNotFoundException(s"File $file does not exist or is not readable")
  }

  def isFatal(ex: Throwable) = {
    ex match {
      case http: HttpResponseException =>
        // Using HttpURLConnection fields as com.google.api.client.http.HttpStatusCodes doesn't have Bad Request (400)
        http.getStatusCode == HTTP_UNAUTHORIZED ||
          http.getStatusCode == HTTP_FORBIDDEN ||
          http.getStatusCode == HTTP_BAD_REQUEST
      case _: OptionLookupException => true
      case _ => false
    }
  }

  def extract(options: OptionLookup, key: String): String = {
    Try(options(key)) match {
      case Success(result) => result
      case Failure(throwable) => throw OptionLookupException(key, throwable)
    }
  }
}

case object GoogleMockAuthMode extends GoogleAuthMode {

  override val name = "no_auth"

  override def credentials(unusedOptions: OptionLookup, unusedScopes: Iterable[String]): OAuth2Credentials =
    NoCredentials.getInstance
}

object ServiceAccountMode {

  sealed trait CredentialFileFormat {
    def file: String
  }

  case class PemFileFormat(accountId: String, file: String) extends CredentialFileFormat

  case class JsonFileFormat(file: String) extends CredentialFileFormat

}

final case class ServiceAccountMode(override val name: String,
                                    fileFormat: CredentialFileFormat)
  extends GoogleAuthMode {

  import CredentialValidatorInstances.gcsCredentialValidation
  import CredentialValidatorSyntax._

  private val credentialsFile = File(fileFormat.file)
  GoogleAuthMode.checkReadable(credentialsFile)

  private lazy val serviceAccountCredentials: ServiceAccountCredentials = {
    fileFormat match {
      case PemFileFormat(accountId, _) =>
        log.warn("The PEM file format will be deprecated in the upcoming Cromwell version. Please use JSON instead.")
        ServiceAccountCredentials.fromPkcs8(accountId, accountId, credentialsFile.contentAsString, null, null)
      case _: JsonFileFormat => ServiceAccountCredentials.fromStream(credentialsFile.newInputStream)
    }
  }

  override def credentials(unusedOptions: OptionLookup,
                           scopes: Iterable[String]): OAuth2Credentials = {

    val scopedCredentials: OAuth2Credentials = serviceAccountCredentials.createScoped(scopes.asJavaCollection)
    CredentialValidationStorage.apply(None, Option(scopedCredentials), None, None, Option(credentialsValidation)).validate
  }
}

final case class UserServiceAccountMode(override val name: String) extends GoogleAuthMode {

  import CredentialValidatorInstances.gcsCredentialValidation
  import CredentialValidatorSyntax._

  private def extractServiceAccount(options: OptionLookup): String = {
    GoogleAuthMode.extract(options, GoogleAuthMode.UserServiceAccountKey)
  }

  private def credentialStream(options: OptionLookup): InputStream = {
    new ByteArrayInputStream(extractServiceAccount(options).getBytes("UTF-8"))
  }

  override def credentials(options: OptionLookup, scopes: Iterable[String]): OAuth2Credentials = {
    val newCredentials = ServiceAccountCredentials.fromStream(credentialStream(options))
    val scopedCredentials: GoogleCredentials = newCredentials.createScoped(scopes.asJavaCollection)
    CredentialValidationStorage.apply(None, Option(scopedCredentials), None, None, Option(credentialsValidation)).validate
  }
}

final case class UserMode(override val name: String,
                          user: String,
                          secretsPath: String,
                          datastoreDir: String) extends GoogleAuthMode {

  private lazy val secretsStream = {
    val secretsFile = File(secretsPath)
    GoogleAuthMode.checkReadable(secretsFile)
    secretsFile.newInputStream
  }

  import CredentialValidatorInstances.gcsCredentialValidation
  import CredentialValidatorSyntax._

  override def credentials(unusedOptions: OptionLookup, unusedScopes: Iterable[String]): OAuth2Credentials = {
    CredentialValidationStorage.apply(None,
      Option(UserCredentials.fromStream(secretsStream)),
      None,
      None,
      Option(credentialsValidation))
      .validate
  }
}

object ApplicationDefaultMode {

  private lazy val applicationDefaultCredentials: GoogleCredentials = GoogleCredentials.getApplicationDefault
}

final case class ApplicationDefaultMode(name: String) extends GoogleAuthMode {

  override def credentials(unusedOptions: OptionLookup,
                           unusedScopes: Iterable[String]): OAuth2Credentials = ApplicationDefaultMode.applicationDefaultCredentials
}

final case class RefreshTokenMode(name: String,
                                  clientId: String,
                                  clientSecret: String) extends GoogleAuthMode with ClientSecrets {

  import CredentialValidatorInstances.gcsCredentialValidation
  import CredentialValidatorSyntax._

  override def requiresAuthFile = true

  private def extractRefreshToken(options: OptionLookup): String = {
    GoogleAuthMode.extract(options, GoogleAuthMode.RefreshTokenOptionKey)
  }

  override def credentials(options: OptionLookup, unusedScopes: Iterable[String]): OAuth2Credentials = {
    val refreshToken = extractRefreshToken(options)
    val newCredentials: UserCredentials = UserCredentials
      .newBuilder()
      .setClientId(clientId)
      .setClientSecret(clientSecret)
      .setRefreshToken(refreshToken)
      .setHttpTransportFactory(GoogleAuthMode.HttpTransportFactory)
      .build()

    CredentialValidationStorage.apply(None, Option(newCredentials), None, None, Option(credentialsValidation)).validate
  }
}

sealed trait ClientSecrets {
  val clientId: String
  val clientSecret: String
}

final case class SimpleClientSecrets(clientId: String, clientSecret: String) extends ClientSecrets

