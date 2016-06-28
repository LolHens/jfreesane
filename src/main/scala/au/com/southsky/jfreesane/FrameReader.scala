package au.com.southsky.jfreesane

import java.io._
import java.util.logging.{Level, Logger}

import com.google.common.base.MoreObjects
import com.google.common.io.ByteStreams
import com.google.common.primitives.UnsignedInteger

object FrameReader {
  private val log: Logger = Logger.getLogger(classOf[FrameReader].getName)
}

/**
  * Represents a reader of {@link Frame frames}.
  */
class FrameReader(val device: SaneDevice, val parameters: SaneParameters, val underlyingStream: InputStream, val bigEndian: Boolean, val listener: ScanListener) {
  @throws[IOException]
  @throws[SaneException]
  def readFrame: Frame = {
    FrameReader.log.log(Level.FINE, "Reading frame: {0}", this)
    var bigArray: ByteArrayOutputStream = null
    val imageSize: Int = parameters.getBytesPerLine * parameters.getLineCount

    // For hand-held scanners where the line count is not known, report an image
    // size of -1 to the user.
    val reportedImageSize: Int =
      if (parameters.getLineCount == -1) -1
      else imageSize

    if (parameters.getLineCount > 0)
      bigArray = new ByteArrayOutputStream(imageSize)
    else
      bigArray = new ByteArrayOutputStream(256)

    var bytesRead: Int = 0
    var totalBytesRead: Int = 0

    while ( {
      bytesRead = readRecord(bigArray)
      bytesRead >= 0
    }) {
      {
        totalBytesRead += bytesRead
        listener.recordRead(device, totalBytesRead, reportedImageSize)
      }
    }
    if (imageSize > 0 && bigArray.size < imageSize) {
      val difference: Int = imageSize - bigArray.size
      FrameReader.log.log(Level.WARNING, "truncated read (got {0}, expected {1} bytes)", Array(bigArray.size, imageSize))
      bigArray.write(new Array[Byte](difference))
      FrameReader.log.log(Level.WARNING, "padded image with {0} null bytes", difference)
    }

    // Now, if necessary, put the bytes in the correct order according
    // to the stream's endianness
    val outputArray: Array[Byte] = bigArray.toByteArray
    if (parameters.getDepthPerPixel == 16 && !bigEndian) {
      if (outputArray.length % 2 != 0)
        throw new IOException("expected a multiple of 2 frame length")

      var i: Int = 0
      while (i < outputArray.length) {
        val swap: Byte = outputArray(i)
        outputArray(i) = outputArray(i + 1)
        outputArray(i + 1) = swap

        i += 2
      }
    }
    if (parameters.getLineCount <= 0) {
      // register the real height
      parameters.setLineCount(outputArray.length / parameters.getBytesPerLine)
      FrameReader.log.log(Level.FINE, "Detected new frame line count: {0}", parameters.getLineCount)
    }

    new Frame(parameters, outputArray)
  }

  @throws[IOException]
  @throws[SaneException]
  private def readRecord(destination: OutputStream): Int = {
    val inputStream: DataInputStream = new DataInputStream(underlyingStream)
    val length: Int = inputStream.readInt

    if (length == 0xffffffff) {
      FrameReader.log.fine("Reached end of records")

      // Hack: saned may actually write a status record here, even
      // though the sane specification says that no more bytes should
      // be read in an end-of-records situation
      val status: Int = inputStream.read
      if (status != -1) {
        val saneStatus: SaneStatus = SaneStatus.fromWireValue(status)

        // An EOF condition is expected: that is what SANE told us!
        if (saneStatus != null && (saneStatus ne SaneStatus.STATUS_EOF))
          throw new SaneException(saneStatus)
      }
      return -1
    }

    if (UnsignedInteger.fromIntBits(length).longValue > Integer.MAX_VALUE)
      throw new IllegalStateException("TODO: support massive records")

    val bytesRead: Int = ByteStreams.copy(ByteStreams.limit(inputStream, length), destination).toInt
    FrameReader.log.log(Level.FINE, "Read a record of {0} bytes", bytesRead)

    bytesRead
  }

  override def toString: String = MoreObjects.toStringHelper(classOf[FrameReader]).add("isBigEndian", bigEndian).add("parameters", parameters).toString
}