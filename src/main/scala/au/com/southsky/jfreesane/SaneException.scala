package au.com.southsky.jfreesane

/**
  * Represents an application level exception thrown by Sane.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneException(val status: SaneStatus, message: String) extends Exception(
  if (message != null)
    message
  else if (status == null)
    "no status"
  else
    status.toString) {

  def this(status: SaneStatus) = this(status, null)

  def this(message: String) = this(null, message)

  /**
    * Returns the reason that this exception was thrown, or {@code null} if none is known.
    */
  def getStatus: SaneStatus = status
}

object SaneException {
  def fromStatusWord(statusWord: SaneWord): SaneException = {
    val status: SaneStatus = SaneStatus.fromWireValue(statusWord)
    if (status != null) {
      return new SaneException(status)
    }
    else {
      return new SaneException("unknown status (" + statusWord.integerValue + ")")
    }
  }
}