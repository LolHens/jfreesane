package au.com.southsky.jfreesane

import java.awt.color.ColorSpace
import java.awt.image._
import java.awt.{Point, Transparency}

import au.com.southsky.jfreesane.SaneImage._
import au.com.southsky.jfreesane.enums.FrameType
import au.com.southsky.jfreesane.saneutil.WriteOnce
import com.google.common.base.Preconditions

/**
  * Represents a SANE image, which are composed of one or more {@link Frame frames}.
  */
class SaneImage private(_frames: List[Frame],
                        val depthPerPixel: Int,
                        val width: Int,
                        val height: Int,
                        val bytesPerLine: Int) {
  // this ensures that in the 3-frame situation, they are always
  // arranged in the following order: red, green, blue
  private val frames: List[Frame] = {
    val order = List(
      FrameType.RED,
      FrameType.GREEN,
      FrameType.BLUE,
      FrameType.RGB,
      FrameType.GRAY
    )

    def ordinal(frameType: FrameType) = order.indexOf(frameType)

    _frames.sortBy(frame => ordinal(frame.`type`))
  }

  private[jfreesane] def toBufferedImage: BufferedImage = {
    val buffer: DataBuffer = asDataBuffer

    if (frames.size == redGreenBlueFrameTypes.size) {
      // 3 frames, one or two bytes per sample, 3 samples per pixel
      val raster: WritableRaster = Raster.createBandedRaster(
        buffer,
        width,
        height,
        bytesPerLine,
        Array(0, 1, 2),
        Array(0, 0, 0),
        new Point(0, 0))

      val model: ColorModel = new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        false,
        false,
        Transparency.OPAQUE,
        DataBuffer.TYPE_BYTE)

      new BufferedImage(model, raster, false, null)
    }


    // Otherwise we're in a one-frame situation
    if (depthPerPixel == 1)
      if (frames.head.`type` == FrameType.GRAY)
        decodeSingleBitGrayscaleImage(buffer)
      else
        decodeSingleBitColorImage

    if (depthPerPixel == 8 || depthPerPixel == 16) {
      var colorSpace: ColorSpace = null
      var bandOffsets: Array[Int] = null

      if (frames.head.`type` == FrameType.GRAY) {
        colorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY)
        bandOffsets = Array(0)
      } else {
        colorSpace = ColorSpace.getInstance(ColorSpace.CS_sRGB)
        bandOffsets = Array(0, 1, 2)
      }

      val raster: WritableRaster = Raster.createInterleavedRaster(
        buffer,
        width,
        height,
        bytesPerLine * java.lang.Byte.SIZE / depthPerPixel,
        bandOffsets.length,
        bandOffsets,
        new Point(0, 0)
      )

      val model: ColorModel = new ComponentColorModel(
        colorSpace,
        false,
        false,
        Transparency.OPAQUE,
        if (depthPerPixel == 8) DataBuffer.TYPE_BYTE else DataBuffer.TYPE_USHORT
      )

      new BufferedImage(model, raster, false, null)
    }
    throw new IllegalStateException("Unsupported SaneImage type")
  }

  private def decodeSingleBitGrayscaleImage(buffer: DataBuffer): BufferedImage = {
    val colorModel = new IndexColorModel(
      1,
      2,
      Array[Byte](0xFF.toByte, 0),
      Array[Byte](0xFF.toByte, 0),
      Array[Byte](0xFF.toByte, 0)
    )

    val raster = Raster.createPackedRaster(buffer, width, height, 1, new Point(0, 0))

    new BufferedImage(colorModel, raster, false, null)
  }

  private def decodeSingleBitColorImage: BufferedImage = {
    val data: Array[Byte] = frames.head.data
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

        image.setRGB(x, y, rgb)
      }

    image
  }

  private def asDataBuffer: DataBuffer =
    if (depthPerPixel == 1 || depthPerPixel == 8) {
      val buffers = new Array[Array[Byte]](frames.size)

      for (i <- frames.indices)
        buffers(i) = frames(i).data

      new DataBufferByte(buffers, frames.head.data.length)
    } else {
      val buffers = new Array[Array[Short]](frames.size)
      val stride: Int = java.lang.Short.SIZE / java.lang.Byte.SIZE

      for (i <- frames.indices) {
        val bank = frames(i).data
        buffers(i) = new Array[Short](bank.length / stride)
        for (j <- buffers(i).indices) {
          buffers(i)(j) = ((bank(stride * j) & 0xFF) << java.lang.Byte.SIZE).toShort
          buffers(i)(j) |= (bank(stride * j + 1) & 0xFF).toShort
        }
      }

      new DataBufferUShort(buffers, frames.head.data.length / stride)
    }
}

object SaneImage {
  private val singletonFrameTypes = Set(FrameType.GRAY, FrameType.RGB)
  private val redGreenBlueFrameTypes = Set(FrameType.RED, FrameType.GREEN, FrameType.BLUE)

  class Builder {
    private var frames = List[Frame]()
    private var frameTypes = Set[FrameType]()
    private val depthPerPixel: WriteOnce[Integer] = new WriteOnce[Integer]
    private val width: WriteOnce[Integer] = new WriteOnce[Integer]
    private val height: WriteOnce[Integer] = new WriteOnce[Integer]
    private val bytesPerLine: WriteOnce[Integer] = new WriteOnce[Integer]

    def addFrame(frame: Frame) {
      Preconditions.checkArgument(!frameTypes.contains(frame.`type`), "Image already contains a frame of this type": Object)
      Preconditions.checkArgument(frameTypes.isEmpty || !singletonFrameTypes.contains(frame.`type`), "The frame type is singleton but this image " + "contains another frame": Object)
      Preconditions.checkArgument(frames.isEmpty || frames.head.data.length == frame.data.length, "new frame has an inconsistent size": Object)
      setPixelDepth(frame.pixelDepth)
      setBytesPerLine(frame.bytesPerLine)
      setWidth(frame.width)
      setHeight(frame.height)
      frameTypes = frameTypes + frame.`type`
      frames = frames :+ frame
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
      Preconditions.checkState(frames.nonEmpty, "no frames": Object)
      Preconditions.checkState(depthPerPixel() != null, "setPixelDepth must be called": Object)
      Preconditions.checkState(width() != null, "setWidth must be called": Object)
      Preconditions.checkState(height() != null, "setHeight must be called": Object)
      Preconditions.checkState(bytesPerLine() != null, "setBytesPerLine must be called": Object)

      // does the image contains a single instance of a singleton frame?
      if (frames.size == 1 && singletonFrameTypes.contains(frames.head.`type`))
        new SaneImage(frames, depthPerPixel(), width(), height(), bytesPerLine())

      // otherwise, does it contain a red, green and blue frame?
      else if (frames.size == redGreenBlueFrameTypes.size && frameTypes.forall(redGreenBlueFrameTypes.contains))
        new SaneImage(frames, depthPerPixel(), width(), height(), bytesPerLine())
      else
        throw new IllegalStateException("Image is not fully constructed. Frame types present: " + frameTypes)
    }
  }

}
