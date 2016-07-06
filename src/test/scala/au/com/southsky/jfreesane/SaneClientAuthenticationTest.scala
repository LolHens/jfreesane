package au.com.southsky.jfreesane

import java.io.{IOException, Reader, StringReader}
import java.util.UUID

import com.google.common.io.CharSource
import junit.framework.Assert
import org.junit.Test

class SaneClientAuthenticationTest {
  @Test def testSaneClientAuthentication = {
    val sca: SaneClientAuthentication = new SaneClientAuthentication
    Assert.assertNotNull(sca)
  }

  @Test def testSaneClientAuthenticationWithMissingFileDoesNotFail = {
    val filepath: String = "NON_EXISTENT_PATH_" + UUID.randomUUID.toString
    val sca: SaneClientAuthentication = new SaneClientAuthentication(filepath)
    Assert.assertNotNull(sca)
  }

  @Test def testInitialize = {
    val sca: SaneClientAuthentication = new SaneClientAuthentication(getTestConfigurationSource)
    val pixmaCreds: SaneClientAuthentication.ClientCredential = sca.getCredentialForResource("pixma")
    Assert.assertEquals("pixma", pixmaCreds.backend)
    Assert.assertEquals("sane-user", pixmaCreds.username)
    Assert.assertEquals("password", pixmaCreds.password)
    val netCreds: SaneClientAuthentication.ClientCredential = sca.getCredentialForResource("net")
    Assert.assertEquals("net", netCreds.backend)
    Assert.assertEquals("other-user", netCreds.username)
    Assert.assertEquals("strongPassword", netCreds.password)
    val mustekCreds: SaneClientAuthentication.ClientCredential = sca.getCredentialForResource("mustek")
    Assert.assertEquals("mustek", mustekCreds.backend)
    Assert.assertEquals("user", mustekCreds.username)
    Assert.assertEquals("", mustekCreds.password)
  }

  @Test def testCanAuthenticateNullResourceFailure = {
    val sca: SaneClientAuthentication = new SaneClientAuthentication(getTestConfigurationSource)
    Assert.assertFalse(sca.canAuthenticate(null))
  }

  @Test def testCanAuthenticateFalse = {
    val rcString: String = "missing$MD5$iamarandomstring"
    val sca: SaneClientAuthentication = new SaneClientAuthentication(getTestConfigurationSource)
    Assert.assertFalse(sca.canAuthenticate(rcString))
  }

  @Test def testCanAuthenticateTrue = {
    val rcString: String = "pixma$MD5$iamarandomstring"
    val sca: SaneClientAuthentication = new SaneClientAuthentication(getTestConfigurationSource)
    Assert.assertTrue(sca.canAuthenticate(rcString))
  }

  @Test def testCanAuthenticateTrueWithoutMD5 = {
    val rcString: String = "mustek"
    val sca: SaneClientAuthentication = new SaneClientAuthentication(getTestConfigurationSource)
    Assert.assertTrue(sca.canAuthenticate(rcString))
  }

  private def getTestConfigurationSource: CharSource = {
    val users: StringBuffer = new StringBuffer
    users.append("sane-user:password:pixma\n")
    users.append("other-user:strongPassword:net\n")
    users.append("user::mustek\n")
    users.append("user1::bad-backend\n")
    users.append("user2::bad-backend\n")

    new CharSource() {
      @throws[IOException]
      def openStream: Reader = new StringReader(users.toString)
    }
  }
}
