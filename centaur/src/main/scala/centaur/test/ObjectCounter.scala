package centaur.test

import com.amazonaws.services.s3.AmazonS3
import com.google.cloud.storage.{Blob, Storage}
import com.google.cloud.storage.Storage.BlobListOption
import scala.collection.JavaConverters._
import scala.language.implicitConversions

trait ObjectCounter[A] {
  def parsePath(regex: String): String => Path = { fullPath =>
    if (fullPath.matches(regex)) {
      val prefixLength = 5
      val bucketAndDashes = fullPath.drop(prefixLength).split("/", 2)
      val bucket = bucketAndDashes.head
      val directory = bucketAndDashes.tail.mkString("/")
      Path(bucket, directory)
    } else throw IllegalPathException()
  }

  def countObjectsAtPath(client: A): Path => Int
}

object ObjectCounterInstances {

  implicit val awsS3ObjectCounter: ObjectCounter[AmazonS3] = (amazonS3: AmazonS3) => {
    implicit def boolToInt(b: Boolean) = if (b) 1 else 0

    def isFileExists(s3: AmazonS3, bucket: String, prefix: String): Boolean =
      s3.listObjects(bucket, prefix).getObjectSummaries.asScala.nonEmpty

    AwsS3Path => isFileExists(amazonS3, AwsS3Path.bucket, AwsS3Path.directory).toInt
  }

  implicit val gcsObjectCounter: ObjectCounter[Storage] = (storage: Storage) => {
    val listObjectsAtPath: Path => Iterable[Blob] = g =>
      storage.list(g.bucket, BlobListOption.prefix(g.directory)).iterateAll.asScala
    listObjectsAtPath(_).size
  }
}

object ObjectCounterSyntax {

  implicit class ObjectCounterSyntax[A](client: A) {
    def countObjects(regex: String)(implicit c: ObjectCounter[A]): String => Int = c.parsePath(regex) andThen c.countObjectsAtPath(client)
  }

}

final case class Path(bucket: String, directory: String)

final case class IllegalPathException(private val message: String = "This is not valid path") extends Exception(message)
