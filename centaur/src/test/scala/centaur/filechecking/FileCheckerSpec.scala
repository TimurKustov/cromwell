package centaur.filechecking

import java.util

import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.{Bucket, ObjectListing, S3ObjectSummary}
import com.google.cloud.storage.Storage.BlobListOption
import com.google.cloud.storage.{Blob, Storage}
import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec
import org.specs2.matcher.ShouldMatchers

class FileCheckerSpec extends FlatSpec with ShouldMatchers with MockFactory {

  import centaur.test.ObjectCounterInstances._

  val amazonS3Stub = stub[AmazonS3]
  val objListing = stub[ObjectListing]
  val testPath = "s3://my-cool-bucket/path/to/file"
  val wrongBucketPrefix = "s3Bucket://my-not-so-cool-bucket/somelogs/empty"
  val wrongPrefixSlashesNumber = "s3Bucket://my-not-so-cool-bucket//somelogs//empty"
  val wrongPrefixSlashes = """s3://my-not-so-cool-bucket\somelogs\empty"""
  val EmptyTestPath = ""
  val bucketName = "my-cool-bucket"
  val dirName = "path/to/file"
  val gsPathType = "gs://"
  val testGsPath = "gs://my-cool-bucket/path/to/file"
  println(testGsPath)



  "parsePath" should "return a bucket and directories" in {

    val objectListing = new ObjectListing
    objectListing.setBucketName(bucketName)
    objectListing.setPrefix(dirName)
    val awsS3Path = awsS3ObjectCounter.parsePath(testPath)

     assert(awsS3Path.bucket == bucketName)
     assert(awsS3Path.directory == dirName)
  }

//todo: 1. test with wrong prefix (handle exception) 2. test with wrong dashes
  "parsePath" should "throw Exception" in {

    val objectListing = new ObjectListing
    objectListing.setBucketName(bucketName)
    objectListing.setPrefix(dirName)

    assertThrows[centaur.test.IllegalPathException] {awsS3ObjectCounter.parsePath(wrongBucketPrefix)}
    assertThrows[centaur.test.IllegalPathException] {awsS3ObjectCounter.parsePath(testGsPath)}
    assertThrows[centaur.test.IllegalPathException] {awsS3ObjectCounter.parsePath(EmptyTestPath)}
//    assertThrows[centaur.test.IllegalPathException] {awsS3ObjectCounter.parsePath(wrongPrefixSlashes)}
//    assertThrows[centaur.test.IllegalPathException] {awsS3ObjectCounter.parsePath(wrongPrefixSlashesNumber)}
  }

  "countObjectAtPath" should "should return 1 if the file exist else 0" in {

    val objSummary = new S3ObjectSummary
    objSummary.setSize(1)
    objSummary.setBucketName("my-cool-bucket")
    val awsS3Path = awsS3ObjectCounter.parsePath(testPath)
    val summaries = new util.ArrayList[S3ObjectSummary]
    summaries.add(objSummary)
    (objListing.getObjectSummaries _).when().returns(summaries)
    (amazonS3Stub.listObjects(_ : String, _ : String)).when(bucketName, dirName).returns(objListing)

    val isExist =  awsS3ObjectCounter.countObjectsAtPath(amazonS3Stub)(awsS3Path)
    assert(isExist == 1)
  }

  "countObjectsAtPath" should "return 0 for empty Summaries" in {

    val isEmpty = true
    val objectListingStub = stub[ObjectListing]
    val S3ObjectSummaryList = new util.ArrayList[S3ObjectSummary]
    assert(S3ObjectSummaryList.isEmpty == isEmpty)
    (objectListingStub.getObjectSummaries _).when().returns(S3ObjectSummaryList)
    (amazonS3Stub.listObjects(_: String, _: String)).when(bucketName, dirName).returns(objectListingStub)
    val awsS3Path = awsS3ObjectCounter.parsePath(testPath)
    val objectNotExists = 0
    assert(awsS3ObjectCounter.countObjectsAtPath(amazonS3Stub)(awsS3Path) == objectNotExists)
  }

  "gcsObjectCounter.parsePath" should "return a bucket and directories" in {

    val gsPath = gcsObjectCounter.parsePath(testGsPath)

    assert(gsPath.bucket == bucketName)
    assert(gsPath.directory == dirName)
  }

  "gcsObjectCounter.parsePath" should "return " in {

    val gcsMock = mock[Storage]
    val gsPath = gcsObjectCounter.parsePath(testGsPath)
    val bucket = new Bucket
    bucket.setName(bucketName)

    import com.google.api.gax.paging.Page
    val pageStub = stub[Page[Blob]]
    val blob = null
    val blobbies = new util.ArrayList[Blob]()
    blobbies.add(blob)
    blobbies.add(blob)
    (pageStub.iterateAll _).when().returns(blobbies)
    when(gcsMock.list(bucketName, ArgumentMatchers.any[BlobListOption]())).thenReturn(pageStub)
//    (gcsMock.list _).expects(Seq(BlobListOption.prefix(dirName))).returns(pageStub)
//    val awsS3Path = awsS3ObjectCounter.parsePath(testPath)
//    val objectNotExists = 0
//    assert(awsS3ObjectCounter.countObjectsAtPath(amazonS3Stub)(awsS3Path) == objectNotExists)

    val objectNotExists = 0
    assert(gcsObjectCounter.countObjectsAtPath(gcsMock)(gsPath) == objectNotExists)
  }

}
