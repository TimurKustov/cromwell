package cromwell.cloudsupport.auth

import com.google.api.client.json.jackson2.JacksonFactory
import com.google.auth.oauth2.OAuth2Credentials
import cromwell.cloudsupport.auth.AuthMode.OptionLookup
import software.amazon.awssdk.auth.credentials.AwsCredentials

import scala.util.Try

object AuthMode {

  case class OptionLookupException(key: String, cause: Throwable) extends RuntimeException(key, cause)

  type OptionLookup = String => String
  type GoogleCredentialsValidation = OAuth2Credentials => Try[OAuth2Credentials]
  type AwsCredentialsValidation = (AwsCredentials, Option[String]) => Try[AwsCredentials]

  lazy val jsonFactory = JacksonFactory.getDefaultInstance

}

trait AuthMode[A] {

  def name: String

  def credentials(options: OptionLookup): A

}