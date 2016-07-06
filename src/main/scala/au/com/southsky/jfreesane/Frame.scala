package au.com.southsky.jfreesane

/**
  * Represents one frame of a {@link SaneImage}. A SANE image is composed of one
  * or more of these frames.
  */
class Frame(parameters: SaneParameters,
            val data: Array[Byte]) {

  def `type`: FrameType = parameters.getFrameType

  def bytesPerLine: Int = parameters.getBytesPerLine

  def width: Int = parameters.getPixelsPerLine

  def height: Int = parameters.getLineCount

  def pixelDepth: Int = parameters.getDepthPerPixel
}