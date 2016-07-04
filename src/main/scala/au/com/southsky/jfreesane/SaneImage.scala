package au.com.southsky.jfreesane

import java.awt.color.ColorSpace
import java.awt.image._
import java.awt.{Point, Transparency}
import java.util

import au.com.southsky.jfreesane.SaneImage._
import com.google.common.base.{Function, Preconditions}
import com.google.common.collect.{Lists, Ordering, Sets}

/**
  * Represents a SANE image, which are composed of one or more {@link Frame frames}.
  */
class SaneImage private(_frames: util.List[Frame],
                        val depthPerPixel: Int,
                        val width: Int,
                        val height: Int,
                        val bytesPerLine: Int) {

  // this ensures that in the 3-frame situation, they are always
  // arranged in the following order: red, green, blue
  private val frames: util.List[Frame] =
    Ordering.explicit(FrameType.RED, FrameType.GREEN, FrameType.BLUE, FrameType.RGB, FrameType.GRAY)
      .onResultOf(
        new Function[Frame, FrameType]() {
          def apply(input: Frame): FrameType = input.getType
        }).immutableSortedCopy(_frames)

  private def getFrames: util.List[Frame] = frames

  private def getDepthPerPixel: Int = depthPerPixel

  private def getWidth: Int = width

  private def getHeight: Int = height

  private def getBytesPerLine: Int = bytesPerLine

  private[jfreesane] def toBufferedImage: BufferedImage = {
    val buffer: DataBuffer = asDataBuffer

    if (getFrames.size == redGreenBlueFrameTypes.size) {
      // 3 frames, one or two bytes per sample, 3 samples per pixel
      val raster: WritableRaster = Raster.createBandedRaster(buffer, getWidth, getHeight, getBytesPerLine,
        Array[Int](0, 1, 2),
        Array[Int](0, 0, 0),
        new Point(0, 0))

      val model: ColorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB),
        false, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)

      new BufferedImage(model, raster, false, null)
    }


    // Otherwise we're in a one-frame situation
    if (depthPerPixel == 1)
      if (getFrames.get(0).getType eq FrameType.GRAY)
        decodeSingleBitGrayscaleImage
      else
        decodeSingleBitColorImage

    if (getDepthPerPixel == 8 || getDepthPerPixel == 16) {
      var colorSpace: ColorSpace = null
      var bandOffsets: Array[Int] = null

      val bytesPerSample: Int = getDepthPerPixel / java.lang.Byte.SIZE

      if (getFrames.get(0).getType eq FrameType.GRAY) {
        colorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY)
        bandOffsets = Array[Int](0)
      }
      else {
        colorSpace = ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB)
        bandOffsets = Array[Int](0, 1 * bytesPerSample, 2 * bytesPerSample)
      }

      val raster: WritableRaster = Raster.createInterleavedRaster(buffer, width, height, bytesPerLine,
        bytesPerSample * bandOffsets.length, bandOffsets, new Point(0, 0))

      val model: ColorModel = new ComponentColorModel(colorSpace, false, false, Transparency.OPAQUE, DataBuffer.TYPE_BYTE)

      new BufferedImage(model, raster, false, null)
    }
    throw new IllegalStateException("Unsupported SaneImage type")
  }

  private def decodeSingleBitGrayscaleImage: BufferedImage = {
    val data: Array[Byte] = frames.get(0).getData
    val image: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (y <- 0 until height)
      for (x <- 0 until width) {
        val lineStartByte = y * bytesPerLine
        val offsetWithinLine = x / java.lang.Byte.SIZE
        val offsetWithinByte = 1 << (java.lang.Byte.SIZE - (x % java.lang.Byte.SIZE) - 1)

        // for a GRAY frame of single bit depth, the value is intensity: 1 is lowest intensity (black), 0 is highest (white)
        val rgb = if ((data(lineStartByte + offsetWithinLine) & offsetWithinByte) == 0) 0xffffff else 0
        image.setRGB(x, y, rgb)
      }

    image
  }

  private def decodeSingleBitColorImage: BufferedImage = {
    val data: Array[Byte] = frames.get(0).getData
    val image: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    val componentCount: Int = 3 // red, green, blue. One bit per sample, byte interleaved

    for (y <- 0 until height)
      for (x <- 0 until width) {
        val lineStartByte = y * bytesPerLine
        val offsetWithinLine = (x / java.lang.Byte.SIZE) * componentCount
        val offsetWithinByte = 1 << (java.lang.Byte.SIZE - (x % java.lang.Byte.SIZE) - 1)

        val red = (data(lineStartByte + offsetWithinLine) & offsetWithinByte) != 0
        val green = (data(lineStartByte + offsetWithinLine + 1) & offsetWithinByte) != 0
        val blue = (data(lineStartByte + offsetWithinLine + 2) & offsetWithinByte) != 0

        var rgb = if (red) 0xff0000 else 0
        rgb = rgb | (if (green) 0x00ff00 else 0)
        rgb = rgb | (if (blue) 0x0000ff else 0)

        image.setRGB(x, y, rgb);
      }

    image
  }

  private def asDataBuffer: DataBuffer = {
    val buffers: Array[Array[Byte]] = new Array[Array[Byte]](getFrames.size)

    for (i <- 0 until getFrames.size())
      buffers(i) = getFrames.get(i).getData

    new DataBufferByte(buffers, getFrames.get(0).getData.length)
  }
}

object SaneImage {
  private val singletonFrameTypes: util.Set[FrameType] = Sets.immutableEnumSet(FrameType.GRAY, FrameType.RGB)
  private val redGreenBlueFrameTypes: util.Set[FrameType] = Sets.immutableEnumSet(FrameType.RED, FrameType.GREEN, FrameType.BLUE)

  class Builder {
    final private val frames: util.List[Frame] = Lists.newArrayList()
    final private val frameTypes: util.Set[FrameType] = util.EnumSet.noneOf(classOf[FrameType])
    final private val depthPerPixel: WriteOnce[Integer] = new WriteOnce[Integer]
    final private val width: WriteOnce[Integer] = new WriteOnce[Integer]
    final private val height: WriteOnce[Integer] = new WriteOnce[Integer]
    final private val bytesPerLine: WriteOnce[Integer] = new WriteOnce[Integer]

    def addFrame(frame: Frame) {
      Preconditions.checkArgument(!frameTypes.contains(frame.getType), "Image already contains a frame of this type": Object)
      Preconditions.checkArgument(frameTypes.isEmpty || !singletonFrameTypes.contains(frame.getType), "The frame type is singleton but this image " + "contains another frame": Object)
      Preconditions.checkArgument(frames.isEmpty || frames.get(0).getData.length == frame.getData.length, "new frame has an inconsistent size": Object)
      setPixelDepth(frame.getPixelDepth)
      setBytesPerLine(frame.getBytesPerLine)
      setWidth(frame.getWidth)
      setHeight(frame.getHeight)
      frameTypes.add(frame.getType)
      frames.add(frame)
    }

    def setPixelDepth(depthPerPixel: Int) {
      Preconditions.checkArgument(depthPerPixel > 0, "depth must be positive": Object)
      this.depthPerPixel.set(depthPerPixel)
    }

    def setWidth(width: Int) {
      this.width.set(width)
    }

    def setHeight(height: Int) = this.height.set(height)

    def setBytesPerLine(bytesPerLine: Int) = this.bytesPerLine.set(bytesPerLine)

    def build: SaneImage = {
      Preconditions.checkState(!frames.isEmpty, "no frames": Object)
      Preconditions.checkState(depthPerPixel.get != null, "setPixelDepth must be called": Object)
      Preconditions.checkState(width.get != null, "setWidth must be called": Object)
      Preconditions.checkState(height.get != null, "setHeight must be called": Object)
      Preconditions.checkState(bytesPerLine.get != null, "setBytesPerLine must be called": Object)

      // does the image contains a single instance of a singleton frame?
      if (frames.size == 1 && singletonFrameTypes.contains(frames.get(0).getType))
        new SaneImage(frames, depthPerPixel.get, width.get, height.get, bytesPerLine.get)

      // otherwise, does it contain a red, green and blue frame?
      else if (frames.size == redGreenBlueFrameTypes.size && redGreenBlueFrameTypes.containsAll(frameTypes))
        new SaneImage(frames, depthPerPixel.get, width.get, height.get, bytesPerLine.get)
      else
        throw new IllegalStateException("Image is not fully constructed. Frame types present: " + frameTypes)
    }
  }

  private class WriteOnce[T] {
    private var value: T = _

    def set(value: T) =
      if (this.value == null)
        this.value = value
      else if (!(value == this.value))
        throw new IllegalArgumentException("Cannot overwrite with a different value")

    def get: T = value
  }

}
