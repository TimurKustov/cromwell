package cromwell.cloudsupport.auth

import com.google.auth.oauth2.OAuth2Credentials
import cromwell.cloudsupport.auth.AuthMode.{AwsCredentialsValidation, GoogleCredentialsValidation}
import software.amazon.awssdk.auth.credentials.AwsCredentials

import scala.util.Success

sealed trait CredentialValidator[A] {

  def validateCredentials(credentials: CredentialValidationStorage): A
}

case class CredentialValidationStorage(awsCredentials: Option[AwsCredentials],
                 gcsCredentials: Option[OAuth2Credentials],
                 options: Option[String],
                 awsCredentialValidation: Option[AwsCredentialsValidation],
                 gcsCredentialValidation: Option[GoogleCredentialsValidation])

object CredentialValidatorInstances {

  implicit val awsCredentialValidation: CredentialValidator[AwsCredentials] =
    new CredentialValidator[AwsCredentials] {

      override def validateCredentials(credentials: CredentialValidationStorage): AwsCredentials = {
        val CredentialValidationStorage(awsCredentials, _, region, awsCredentialsValidation, _) = credentials

        awsCredentialsValidation.flatMap(f => awsCredentials.map(x => f(x, region))) match {
          case Some(Success(value)) => value
          case _ => throw new RuntimeException("Invalid credentials")
        }
      }
    }

  implicit val gcsCredentialValidation: CredentialValidator[OAuth2Credentials] =
    new CredentialValidator[OAuth2Credentials] {

      override def validateCredentials(credentials: CredentialValidationStorage): OAuth2Credentials = {
        val CredentialValidationStorage(_, gcsCredentials, _, _, gcsCredentialsValidation) = credentials

        gcsCredentialsValidation.flatMap(f => gcsCredentials.map(x => f(x))) match {
          case Some(Success(value)) => value
          case _ => throw new RuntimeException("Invalid credentials")
        }
      }
    }
}

object CredentialValidatorSyntax {

  implicit class CredentialValidatorSyntax[A](credentials: CredentialValidationStorage) {
    def validate(implicit v: CredentialValidator[A]): A = v.validateCredentials(credentials)
  }
}