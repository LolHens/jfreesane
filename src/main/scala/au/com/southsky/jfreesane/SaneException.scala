package au.com.southsky.jfreesane

import au.com.southsky.jfreesane.enums.SaneStatus

/**
  * Represents an application level exception thrown by Sane.
  *
  * status: Returns the reason that this exception was thrown, or {@code null} if none is known.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneException private(message: String,
                            val status: Option[SaneStatus] = None) extends Exception(message)

object SaneException {
  def apply(message: String) = new SaneException(message)

  def apply(status: SaneStatus) = new SaneException(
    Option(status).map(_.toString).getOrElse("no status"),
    Some(status))

  def apply(statusWord: SaneWord): SaneException = SaneStatus.fromWireValue(statusWord) match {
    case null =>
      SaneException(s"unknown status (${statusWord.intValue})")

    case status =>
      SaneException(status)
  }
}