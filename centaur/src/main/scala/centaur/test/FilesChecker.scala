package centaur.test

import com.amazonaws.services.s3.model.{ListObjectsRequest, ObjectListing, S3ObjectSummary}
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, Storage}

import scala.collection.JavaConversions.{collectionAsScalaIterable => asScala}
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

  private lazy val s3storageRequest = Operations.awsS3storageRequest

  override def countObjectsAtPath: String => Int =
    s3storageRequest.parsePath andThen s3storageRequest.countObjectsAtPath


}

case class GCSPath(bucket: String, directory: String)

case class AwsS3Path(bucket: String, directory: String)

object GCS {

  implicit def gcsOps(s: Storage): GCSOps = GCSOps(s)
}

object AwsS3 {

  implicit def awsS3Ops(s: ListObjectsRequest): AwsS3Ops = AwsS3Ops(s)

}

case class AwsS3Ops(request: ListObjectsRequest) {

  def parsePath: String => AwsS3Path = { fullPath =>
    val bucketAndDashes = fullPath.drop(5).split('/')
    val bucket = bucketAndDashes.head
    val directory = bucketAndDashes.tail.mkString("/")

    AwsS3Path(bucket, directory)
  }


  private def scan[T](s3: AmazonS3, bucket: String, prefix: String, f: S3ObjectSummary => T) = {
    def scanInner(acc: List[T], listing: ObjectListing): List[T] = {
      val summaries = asScala[S3ObjectSummary](listing.getObjectSummaries)
      val mapped = (for (summary <- summaries) yield f(summary)).toList

      if (!listing.isTruncated) mapped
      else scanInner(acc ::: mapped, s3.listNextBatchOfObjects(listing))
    }

    scanInner(List(), s3.listObjects(bucket, prefix))
  }

  def countObjectsAtPath: AwsS3Path => Int = {
    AwsS3Path => scan( AmazonS3ClientBuilder.standard().build(), AwsS3Path.bucket, AwsS3Path.directory, s => s.getSize).sum.toInt
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
