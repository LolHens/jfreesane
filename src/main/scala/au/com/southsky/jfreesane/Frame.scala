package au.com.southsky.jfreesane

import au.com.southsky.jfreesane.enums.FrameType

/**
  * Represents one frame of a {@link SaneImage}. A SANE image is composed of one
  * or more of these frames.
  */
class Frame(parameters: SaneParameters,
            val data: Array[Byte]) {

  def `type`: FrameType = parameters.frameType

  def bytesPerLine: Int = parameters.bytesPerLine

  def width: Int = parameters.pixelsPerLine

  def height: Int = parameters.lineCount

  def pixelDepth: Int = parameters.depthPerPixel
}