package au.com.southsky.jfreesane

/**
  * Created by u016595 on 01.07.2016.
  */
sealed class SaneStatus(name: String, wireValue: Int) extends SaneEnum2[SaneStatus](name, wireValue)

object SaneStatus {

  object STATUS_GOOD extends SaneStatus("STATUS_GOOD", 0)

  object STATUS_UNSUPPORTED extends SaneStatus("STATUS_UNSUPPORTED", 1)

  object STATUS_CANCELLED extends SaneStatus("STATUS_CANCELLED", 2)

  object STATUS_DEVICE_BUSY extends SaneStatus("STATUS_DEVICE_BUSY", 3)

  object STATUS_INVAL extends SaneStatus("STATUS_INVAL", 4)

  object STATUS_EOF extends SaneStatus("STATUS_EOF", 5)

  object STATUS_JAMMED extends SaneStatus("STATUS_JAMMED", 6)

  object STATUS_NO_DOCS extends SaneStatus("STATUS_NO_DOCS", 7)

  object STATUS_COVER_OPEN extends SaneStatus("STATUS_COVER_OPEN", 8)

  object STATUS_IO_ERROR extends SaneStatus("STATUS_IO_ERROR", 9)

  object STATUS_NO_MEM extends SaneStatus("STATUS_NO_MEM", 10)

  object STATUS_ACCESS_DENIED extends SaneStatus("STATUS_ACCESS_DENIED", 11)

  val tmp_STATUS_NO_DOCS = STATUS_NO_DOCS

  val tmp_STATUS_ACCESS_DENIED = STATUS_ACCESS_DENIED

  def fromWireValue(wireValue: Int): SaneStatus = SaneEnums.valueOf(classOf[SaneStatus], wireValue)

  def fromWireValue(statusWord: SaneWord): SaneStatus = fromWireValue(statusWord.integerValue)
}