package au.com.southsky.jfreesane

/**
  * Represents one frame of a {@link SaneImage}. A SANE image is composed of one
  * or more of these frames.
  */
class Frame(val parameters: SaneParameters, val data: Array[Byte]) {
  def getType: FrameType = parameters.getFrameType

  def getData: Array[Byte] = data

  def getBytesPerLine: Int = parameters.getBytesPerLine

  def getWidth: Int = parameters.getPixelsPerLine

  def getHeight: Int = parameters.getLineCount

  def getPixelDepth: Int = parameters.getDepthPerPixel
}