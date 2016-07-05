package au.com.southsky.jfreesane

/**
  * Represents the various types of image frames in SANE.
  */
sealed class FrameType(name: String, wireValue: Int) extends SaneEnum2[FrameType](name, wireValue)

object FrameType {

  object GRAY extends FrameType("GRAY", 0)

  object RGB extends FrameType("RGB", 1)

  object RED extends FrameType("RED", 2)

  object GREEN extends FrameType("GREEN", 3)

  object BLUE extends FrameType("BLUE", 4)

}
