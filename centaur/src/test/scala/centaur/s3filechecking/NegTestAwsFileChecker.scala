package centaur.s3filechecking

import java.util

import centaur.test.AwsS3Ops
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.{ObjectListing, S3ObjectSummary}
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import org.specs2.matcher.ShouldMatchers

class NegTestAwsFileChecker extends FlatSpec with ShouldMatchers with MockFactory {
  val amazonS3Stub = stub[AmazonS3]
  val awsS3Ops = AwsS3Ops(amazonS3Stub)
  val testPath = "s3://my-cool-bucket/cromwell/somelogs/myfile"
  val wrongTestPath = "s3://my-not-so-cool-bucket/somelogs/empty"
  val objectListing = new ObjectListing
  val bucketName = "my-cool-bucket"
  val dirName = "cromwell/somelogs/myfile"

  "AwsFilesChecker" should "return a count of objects at concrete s3 path" in {

    objectListing.setBucketName(bucketName)
    objectListing.setPrefix(dirName)
    (amazonS3Stub.listObjects(_: String, _: String)).when(bucketName, dirName).returns(objectListing)
    val awsS3Path = awsS3Ops.parsePath(wrongTestPath)
    assert(awsS3Path.bucket != bucketName)
    assert(awsS3Path.directory != dirName)
  }

  "countObjectsAtPath" should "return 0 for empty Summaries" in {

    val isEmpty = true
    val objectListingStub = stub[ObjectListing]
    val S3OSList = new util.ArrayList[S3ObjectSummary]
    assert(S3OSList.isEmpty == isEmpty)
    (objectListingStub.getObjectSummaries _).when().returns(S3OSList)
    (amazonS3Stub.listObjects(_: String, _: String)).when(bucketName, dirName).returns(objectListingStub)
    val AwsS3Path = awsS3Ops.parsePath(testPath)
    val objectExists = 0
    assert(awsS3Ops.countObjectsAtPath(AwsS3Path) == objectExists)
  }
}



