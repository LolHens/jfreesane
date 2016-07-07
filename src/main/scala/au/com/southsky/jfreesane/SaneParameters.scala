package au.com.southsky.jfreesane

import com.google.common.base.MoreObjects

class SaneParameters private[jfreesane](val frame: Int, val lastFrame: Boolean, val bytesPerLine: Int, val pixelsPerLine: Int, var lineCount: Int, val depthPerPixel: Int) {
  private val frameType: FrameType = FrameType(frame)

  def getFrameType: FrameType = frameType

  def isLastFrame: Boolean = lastFrame

  def getBytesPerLine: Int = bytesPerLine

  def getPixelsPerLine: Int = pixelsPerLine

  def getLineCount: Int = lineCount

  private[jfreesane] def setLineCount(lineCount: Int) = this.lineCount = lineCount

  def getDepthPerPixel: Int = depthPerPixel

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