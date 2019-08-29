package centaur.testfilecheck

import java.util

import org.scalatest.FlatSpec
import org.specs2.matcher.ShouldMatchers
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.{ListObjectsRequest, ListObjectsResponse, S3Object}
import org.specs2.mock.Mockito
import org.mockito.Mockito._

class FileCheckerSpec extends FlatSpec with ShouldMatchers with Mockito {

  import centaur.test.ObjectCounterInstances._

  val amazonS3mock = mock[S3Client]
  val testPath = "s3://my-cool-bucket/path/to/file"
  val bucketName = "my-cool-bucket"
  val dirName = "path/to/file"
  val objResponse = ListObjectsResponse.builder().contents(util.Arrays.asList(S3Object.builder().build())).build()
  val objRequest = ListObjectsRequest.builder().bucket(bucketName).prefix(dirName).build()
  val awsS3Path = awsS3ObjectCounter.parsePath("^s3:\\/\\/.*")(testPath)

  "parsePath" should "return a bucket and directories" in {
    assert(awsS3Path.bucket == bucketName)
    assert(awsS3Path.directory == dirName)
  }

  "countObjectAtPath" should "should return 1 if the file exist else 0" in {
    when(amazonS3mock.listObjects(objRequest)).thenReturn(objResponse)
    val isExist = awsS3ObjectCounter.countObjectsAtPath(amazonS3mock)(awsS3Path)
    assert(isExist == 1)
  }
}
