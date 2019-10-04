package cromwell.cloudsupport.auth

import cromwell.cloudsupport.auth.AuthMode.{AwsCredentialsValidation, OptionLookup}
import software.amazon.awssdk.auth.credentials.{AnonymousCredentialsProvider, AwsBasicCredentials, AwsCredentials, AwsSessionCredentials, DefaultCredentialsProvider, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.sts.StsClient
import software.amazon.awssdk.services.sts.model.{AssumeRoleRequest, GetCallerIdentityRequest}

import scala.util.Try


sealed trait AwsAuthMode extends AuthMode[AwsCredentials] {

  private[cloudsupport] def credential: AwsCredentials

  var credentialsValidation: AwsCredentialsValidation = (credentials: AwsCredentials, region: Option[String]) => {
    val builder = StsClient.builder
    //If the region argument exists in config, set it in the builder.
    //Otherwise it is left unset and the AwsCredential builder will look in various places to supply,
    //ultimately using US-EAST-1 if none is found
    Try {
      region.map(Region.of).foreach(builder.region)

      builder.credentialsProvider(StaticCredentialsProvider.create(credentials))
        .build
        .getCallerIdentity(GetCallerIdentityRequest.builder.build)
      credentials
    }

  }

}

case object MockAuthMode extends AwsAuthMode {

  override val credential: AwsCredentials = AnonymousCredentialsProvider.create.resolveCredentials()

  override val name = "no_auth"

  override def credentials(options: OptionLookup): AwsCredentials = credential
}


final case class CustomKeyMode(override val name: String,
                               accessKey: String,
                               secretKey: String,
                               region: Option[String]
                              ) extends AwsAuthMode {

  import CredentialValidatorInstances.awsCredentialValidation
  import CredentialValidatorSyntax._

  override val credential: AwsCredentials = {

    val a = CredentialValidationStorage.apply(Option(AwsBasicCredentials.create(accessKey, secretKey)),
      None,
      region,
      Option(credentialsValidation),
      None)

    a.validate
    // Validate credentials synchronously here, without retry.
    // It's very unlikely to fail as it should not happen more than a few times
    // (one for the engine and for each backend using it) per Cromwell instance.
  }

  override def credentials(options: OptionLookup): AwsCredentials = credential
}

final case class DefaultMode(override val name: String, region: Option[String]) extends AwsAuthMode {

  import CredentialValidatorInstances.awsCredentialValidation
  import CredentialValidatorSyntax._
  // The ProfileCredentialsProvider will return your [default]
  // credential profile by reading from the credentials file located at
  // (~/.aws/credentials).
  //
  // Validate credentials synchronously here, without retry.
  // It's very unlikely to fail as it should not happen more than a few times
  // (one for the engine and for each backend using it) per Cromwell instance.
  override val credential: AwsCredentials =
  CredentialValidationStorage.apply(Option(DefaultCredentialsProvider.create.resolveCredentials()),
    None,
    region,
    Option(credentialsValidation),
    None).validate

  override def credentials(options: OptionLookup): AwsCredentials = credential
}


final case class AssumeRoleMode(override val name: String,
                                baseAuthName: String,
                                roleArn: String,
                                externalId: String,
                                region: Option[String]
                               ) extends AwsAuthMode {

  import CredentialValidatorInstances.awsCredentialValidation
  import CredentialValidatorSyntax._

  private var baseAuthObj: Option[AuthMode[AwsCredentials]] = None

  override val credential: AwsCredentials = {
    val requestBuilder = AssumeRoleRequest
      .builder
      .roleSessionName("cromwell")
      .roleArn(roleArn)
      .durationSeconds(3600)

    // The builder is simply mutating itself (TODO: find good ref, as v2
    // uses generated code)
    // So we can get away with a val and discard the return value
    if (externalId.nonEmpty) requestBuilder.externalId(externalId)
    val request = requestBuilder.build

    val builder = StsClient.builder
    region.foreach(str => builder.region(Region.of(str)))
    // See comment gnabove regarding builder
    baseAuthObj match {
      case Some(auth) => builder.credentialsProvider(StaticCredentialsProvider.create(auth.credentials(_ => "")))
      case _ => throw new RuntimeException(s"Base auth configuration required for assume role")
    }

    val stsCredentials = builder.build.assumeRole(request).credentials

    val sessionCredentials = AwsSessionCredentials.create(
      stsCredentials.accessKeyId,
      stsCredentials.secretAccessKey,
      stsCredentials.sessionToken)

    CredentialValidationStorage.apply(Option(sessionCredentials),
      None,
      region,
      Option(credentialsValidation),
      None).validate
  }

  override def credentials(options: OptionLookup): AwsCredentials = credential

  def assign(baseAuth: AuthMode[AwsCredentials]): Unit = {
    baseAuthObj = baseAuthObj match {
      case None => Some(baseAuth)
      case _ => throw new RuntimeException(s"Base auth object has already been assigned")
    }
  }

  // We want to allow our tests access to the value
  // of the baseAuthObj
  def baseAuthentication(): AuthMode[AwsCredentials] =
    baseAuthObj.getOrElse(throw new RuntimeException(s"Base auth object has not been set"))
}