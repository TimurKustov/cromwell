package cromwell.cloudsupport.auth

import org.scalatest.{FlatSpec, Matchers}

class MockAuthModeSpec extends FlatSpec with Matchers {

  behavior of "GoogleMockAuthMode"

  it should "generate a credential" in {
    val mockAuthMode = GoogleMockAuthMode
    val credentials = mockAuthMode.credentials()
    credentials.getAuthenticationType should be("OAuth2")
  }

  it should "requiresAuthFile" in {
    val mockAuthMode = GoogleMockAuthMode
    mockAuthMode.requiresAuthFile should be(false)
    succeed
  }

}
