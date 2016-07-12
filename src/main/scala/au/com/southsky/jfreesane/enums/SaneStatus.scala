package au.com.southsky.jfreesane.enums

import au.com.southsky.jfreesane.SaneWord

/**
  * Created by u016595 on 01.07.2016.
  */
sealed class SaneStatus(wireValue: Int) extends SaneEnum[SaneStatus](wireValue)

object SaneStatus extends SaneEnumObject[SaneStatus] {

  object STATUS_GOOD extends SaneStatus(0)

  object STATUS_UNSUPPORTED extends SaneStatus(1)

  object STATUS_CANCELLED extends SaneStatus(2)

  object STATUS_DEVICE_BUSY extends SaneStatus(3)

  object STATUS_INVAL extends SaneStatus(4)

  object STATUS_EOF extends SaneStatus(5)

  object STATUS_JAMMED extends SaneStatus(6)

  object STATUS_NO_DOCS extends SaneStatus(7)

  object STATUS_COVER_OPEN extends SaneStatus(8)

  object STATUS_IO_ERROR extends SaneStatus(9)

  object STATUS_NO_MEM extends SaneStatus(10)

  object STATUS_ACCESS_DENIED extends SaneStatus(11)

  def fromWireValue(wireValue: Int): SaneStatus = SaneStatus(wireValue)

  def fromWireValue(statusWord: SaneWord): SaneStatus = fromWireValue(statusWord.intValue)

  override def values: Set[SaneStatus] = Set(
    STATUS_GOOD,
    STATUS_UNSUPPORTED,
    STATUS_CANCELLED,
    STATUS_DEVICE_BUSY,
    STATUS_INVAL,
    STATUS_EOF,
    STATUS_JAMMED,
    STATUS_NO_DOCS,
    STATUS_COVER_OPEN,
    STATUS_IO_ERROR,
    STATUS_NO_MEM,
    STATUS_ACCESS_DENIED
  )
}