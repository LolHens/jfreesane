package au.com.southsky.jfreesane

import java.io._
import java.util
import java.util.logging.{Level, Logger}

import com.google.common.base.{Charsets, Joiner}
import com.google.common.collect.{HashBasedTable, Table}
import com.google.common.io.{CharSource, CharStreams, LineProcessor}

/**
  * Represents the authentication configuration used by SANE clients. The SANE
  * utilities like {@code scanimage} will read the {@code ~/.sane/pass} directory
  * (if it exists), this class provides an implementation of that behavior.
  * Returns a new {@code SaneClientAuthentication} whose configuration is
  * represented by the characters supplied by the given {@link CharSource}.
  *
  * <p>
  * Threadsafe.
  */
class SaneClientAuthentication(val configurationSource: CharSource) extends SanePasswordProvider {
  private val credentials: Table[String, String, String] = HashBasedTable.create()
  private var initialized: Boolean = false

  def this(path: String) {
    this(new CharSource() {
      @throws[IOException]
      def openStream: Reader = new InputStreamReader(new FileInputStream(path), Charsets.US_ASCII)
    })
  }

  def this() {
    this(SaneClientAuthentication.DEFAULT_CONFIGURATION_PATH)
  }

  private def initializeIfRequired = {
    if (!initialized) {
      initialized = true

      try {
        CharStreams.readLines(configurationSource.openStream, new LineProcessor[Void]() {
          private var lineNumber: Int = 0

          @throws[IOException]
          def processLine(line: String): Boolean = {
            lineNumber += 1

            val credential: SaneClientAuthentication.ClientCredential = SaneClientAuthentication.ClientCredential.fromAuthString(line)

            if (credential == null)
              SaneClientAuthentication.logger.log(Level.WARNING, "ignoring invalid configuration format (line {0}): {1}", Array(lineNumber, line))
            else {
              credentials.put(credential.backend, credential.username, credential.password)
              if (credentials.row(credential.backend).size > 1)
                SaneClientAuthentication.logger.log(Level.WARNING, "ignoring line {0}, we already have a configuration for resource [{1}]", Array(lineNumber, credential.backend))
            }
            true
          }

          def getResult: Void = null
        })
      } catch {
        case e: IOException =>
          SaneClientAuthentication.logger.log(Level.WARNING, "could not read auth configuration due to IOException", e)
      }
    }
  }

  /**
    * Returns {@code true} if the configuration contains an entry for the given
    * resource.
    */
  def canAuthenticate(resource: String): Boolean = {
    if (resource == null)
      false
    else {
      val credential: SaneClientAuthentication.ClientCredential = getCredentialForResource(resource)
      credential != null
    }
  }

  def getCredentialForResource(rc: String): SaneClientAuthentication.ClientCredential = {
    initializeIfRequired

    val resource: String =
      if (rc.contains(SaneClientAuthentication.MARKER_MD5))
        rc.substring(0, rc.indexOf(SaneClientAuthentication.MARKER_MD5))
      else
        rc

    val credentialsForResource: util.Map[String, String] = credentials.row(resource)

    import scala.collection.JavaConversions._
    val first = credentialsForResource.entrySet.headOption

    first.map(credential => new SaneClientAuthentication.ClientCredential(resource, credential.getKey, credential.getValue)).orNull
  }

  def username(resource: String): String = getCredentialForResource(resource).username

  def password(resource: String): String = getCredentialForResource(resource).password
}

object SaneClientAuthentication {
  private val logger: Logger = Logger.getLogger(classOf[SaneClientAuthentication].getName)

  val MARKER_MD5: String = "$MD5$"

  private val DEFAULT_CONFIGURATION_PATH: String = Joiner.on(File.separator).join(System.getProperty("user.home"), ".sane", "pass")

  /**
    * Class to hold Sane client credentials organised by backend.
    *
    * @author paul
    */
  class ClientCredential(val backend: String,
                         val username: String,
                         val password: String)

  object ClientCredential {
    def fromAuthString(authString: String): SaneClientAuthentication.ClientCredential = {
      val fields = authString.split(":")
      if (fields.size < 3)
        null
      else
        new SaneClientAuthentication.ClientCredential(fields(2), fields(0), fields(1))
    }
  }

}