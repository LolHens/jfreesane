package au.com.southsky.jfreesane.enums

/**
  * Represents the various types of image frames in SANE.
  */
sealed class FrameType(wireValue: Int) extends SaneEnum[FrameType](wireValue)

object FrameType extends SaneEnumObject[FrameType] {

  object GRAY extends FrameType(0)

  object RGB extends FrameType(1)

  object RED extends FrameType(2)

  object GREEN extends FrameType(3)

  object BLUE extends FrameType(4)

  override def values: Set[FrameType] = Set(
    GRAY,
    RGB,
    RED,
    GREEN,
    BLUE
  )
}
