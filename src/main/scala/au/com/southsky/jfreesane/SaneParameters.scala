package au.com.southsky.jfreesane

import com.google.common.base.MoreObjects

class SaneParameters(frame: Int,
                     val lastFrame: Boolean,
                     val bytesPerLine: Int,
                     val pixelsPerLine: Int,
                     private var _lineCount: Int,
                     val depthPerPixel: Int) {
  val frameType: FrameType = FrameType(frame)

  def lineCount: Int = _lineCount

  private[jfreesane] def lineCount_=(value: Int) = _lineCount = value

  override def toString: String =
    MoreObjects.toStringHelper(classOf[SaneParameters])
      .add("frameType", frameType)
      .add("isLastFrame", lastFrame)
      .add("bytesPerLine", bytesPerLine)
      .add("pixelsPerLine", pixelsPerLine)
      .add("lineCount", lineCount)
      .add("depthPerPixel", depthPerPixel)
      .toString
}