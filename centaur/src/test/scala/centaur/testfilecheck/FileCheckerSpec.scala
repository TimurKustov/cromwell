package centaur.testfilecheck

import java.util

import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.{ObjectListing, S3ObjectSummary}
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import org.specs2.matcher.ShouldMatchers

class FileCheckerSpec extends FlatSpec with ShouldMatchers with MockFactory {

  import centaur.test.ObjectCounterInstances._

  val amazonS3Stub = stub[AmazonS3]
  val objListing = stub[ObjectListing]
  val testPath = "s3://my-cool-bucket/path/to/file"
  val bucketName = "my-cool-bucket"
  val dirName = "path/to/file"
  val awsS3Path = awsS3ObjectCounter.parsePath("^s3:\\/\\/.*")(testPath)


  "parsePath" should "return a bucket and directories" in {

    val objectListing = new ObjectListing
    objectListing.setBucketName(bucketName)
    objectListing.setPrefix(dirName)

    (amazonS3Stub.listObjects(_: String, _: String)).when(bucketName, dirName).returns(objectListing)

     assert(awsS3Path.bucket == bucketName)
     assert(awsS3Path.directory == dirName)

  }

  "countObjectAtPath" should "should return 1 if the file exist else 0" in {

    val objSummary = new S3ObjectSummary
    objSummary.setSize(1)
    objSummary.setBucketName("my-cool-bucket")

    val summaries = new util.ArrayList[S3ObjectSummary]
    summaries.add(objSummary)
    (objListing.getObjectSummaries _).when().returns(summaries)
    (amazonS3Stub.listObjects(_ : String, _ : String)).when(bucketName, dirName).returns(objListing)

    val isExist =  awsS3ObjectCounter.countObjectsAtPath(amazonS3Stub)(awsS3Path)
    assert(isExist == 1)
  }
}
