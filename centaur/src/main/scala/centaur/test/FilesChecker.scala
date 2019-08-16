package centaur.test

import com.amazonaws.services.s3.AmazonS3
import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, Storage}

import scala.collection.JavaConverters._
import scala.language.implicitConversions

sealed trait FilesChecker {
  def countObjectsAtPath: String => Int
}

case object PipelinesFilesChecker extends FilesChecker {

  import GCS._

  private lazy val storage = Operations.storage

  def countObjectsAtPath: String => Int =
    storage.parsePath andThen storage.countObjectsAtPath
}

case object LocalFilesChecker extends FilesChecker {
  def countObjectsAtPath: String => Int = { s =>
    val d = new java.io.File(s)
    if (d.exists && d.isDirectory)
      d.listFiles.length
    else if (d.exists && d.isFile)
      1
    else
      0
  }
}

case object AWSFilesChecker extends FilesChecker {
  import AwsS3._

  private lazy val s3Client = Operations.buildAmazonS3Client

  override def countObjectsAtPath: String => Int =
    s3Client.parsePath andThen s3Client.countObjectsAtPath
}

case class GCSPath(bucket: String, directory: String)

case class AwsS3Path(bucket: String, directory: String)

object GCS {

  implicit def gcsOps(s: Storage): GCSOps = GCSOps(s)
}

object AwsS3 {
  implicit def awsOps(amazonS3: AmazonS3): AwsS3Ops = AwsS3Ops(amazonS3)
}

case class AwsS3Ops(amazonS3: AmazonS3) {

  def parsePath: String => AwsS3Path = { fullPath =>
    val bucketAndDashes = fullPath.drop(5).split('/')
    val bucket = bucketAndDashes.head
    val directory = bucketAndDashes.tail.mkString("/")

    AwsS3Path(bucket, directory)
  }

  private def isFileExists(s3: AmazonS3, bucket: String, prefix: String): Boolean =
    s3.listObjects(bucket, prefix).getObjectSummaries.asScala.nonEmpty

  implicit def boolToInt(b: Boolean) = if (b) 1 else 0

  def countObjectsAtPath: AwsS3Path => Int = {
    AwsS3Path => isFileExists(amazonS3, AwsS3Path.bucket, AwsS3Path.directory).toInt
  }

}

case class GCSOps(storage: Storage) {

  def parsePath: String => GCSPath = { fullPath =>
    val bucketAndDashes = fullPath.drop(5).split('/')
    val bucket = bucketAndDashes.head
    val directory = bucketAndDashes.tail.mkString("/")

    GCSPath(bucket, directory)
  }

  private def listObjectsAtPath: GCSPath => Iterable[Blob] =
    g =>
      storage.list(g.bucket, BlobListOption.prefix(g.directory)).iterateAll.asScala

  def countObjectsAtPath: GCSPath => Int =
    listObjectsAtPath(_).size
}
