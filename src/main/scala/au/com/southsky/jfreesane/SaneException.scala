package au.com.southsky.jfreesane

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

class SaneException(val status: SaneStatus, message: String) extends Exception(
  if (message != null)
    message
  else if (status == null)
    "no status"
  else
    status.toString) {

  def this(status: SaneStatus) = this(status, null)

  def this(message: String) = this(null, message)

  def getStatus: SaneStatus = status
}
