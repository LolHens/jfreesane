package au.com.southsky.jfreesane

import java.io._
import java.util.logging.{Level, Logger}

import au.com.southsky.jfreesane.device.SaneDevice
import au.com.southsky.jfreesane.enums.SaneStatus
import com.google.common.base.MoreObjects
import com.google.common.io.ByteStreams
import com.google.common.primitives.UnsignedInteger

/**
  * Represents a reader of {@link Frame frames}.
  */
class FrameReader(val device: SaneDevice,
                  val parameters: SaneParameters,
                  val underlyingStream: InputStream,
                  val bigEndian: Boolean,
                  val listener: ScanListener) {

  @throws[IOException]
  @throws[SaneException]
  def readFrame: Frame = {
    @throws[IOException]
    @throws[SaneException]
    def readRecord(destination: OutputStream): Int = {
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
            throw SaneException(saneStatus)
        }
        return -1
      }

      if (UnsignedInteger.fromIntBits(length).longValue > Integer.MAX_VALUE)
        throw new IllegalStateException("TODO: support massive records")

      val bytesRead: Int = ByteStreams.copy(ByteStreams.limit(inputStream, length), destination).toInt
      FrameReader.log.log(Level.FINE, s"Read a record of $bytesRead bytes")

      bytesRead
    }

    FrameReader.log.log(Level.FINE, s"Reading frame: ${this}")

    // For hand-held scanners where the line count is not known, report an image
    // size of -1 to the user.
    val imageSizeBytes: Option[Int] =
    if (parameters.lineCount == -1)
      None
    else
      Some(parameters.bytesPerLine * parameters.lineCount)

    val bigArray = new ByteArrayOutputStream(imageSizeBytes.getOrElse(256))

    var bytesRead: Int = 0
    var totalBytesRead: Int = 0

    while ( {
      bytesRead = readRecord(bigArray)
      bytesRead >= 0
    }) {
      {
        totalBytesRead += bytesRead
        listener.recordRead(device, totalBytesRead, imageSizeBytes)
      }
    }

    // Pad image if necessary
    imageSizeBytes match {
      case Some(imageSize) if bigArray.size < imageSize =>
        val difference = imageSize - bigArray.size
        FrameReader.log.log(Level.WARNING, s"truncated read (got ${bigArray.size}, expected $imageSize bytes)")

        bigArray.write(new Array[Byte](difference))

        FrameReader.log.log(Level.WARNING, s"padded image with $difference null bytes")

      case None =>
    }

    // Now, if necessary, put the bytes in the correct order according to the stream's endianness
    val outputArray: Array[Byte] = bigArray.toByteArray
    if (parameters.depthPerPixel == 16 && !bigEndian) {
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
    if (parameters.lineCount <= 0) {
      // register the real height
      parameters.lineCount = outputArray.length / parameters.bytesPerLine
      FrameReader.log.log(Level.FINE, s"Detected new frame line count: ${parameters.lineCount}")
    }

    new Frame(parameters, outputArray)
  }

  override def toString: String = MoreObjects.toStringHelper(classOf[FrameReader]).add("isBigEndian", bigEndian).add("parameters", parameters).toString
}

object FrameReader {
  private val log: Logger = Logger.getLogger(classOf[FrameReader].getName)
}
