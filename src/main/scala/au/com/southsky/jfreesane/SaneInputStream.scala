package au.com.southsky.jfreesane

import java.io.{IOException, InputStream}
import java.util.logging.{Level, Logger}

import com.google.common.base.Charsets
import com.google.common.io.ByteStreams

import scala.collection.mutable.ListBuffer

/**
  * Wraps an {@link InputStream} to provide some methods for deserializing SANE-related types.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneInputStream(val saneSession: SaneSession,
                      var wrappedStream: InputStream) extends InputStream {
  private var currentGroup: OptionGroup = null

  @throws[IOException]
  def read: Int = wrappedStream.read

  @throws[IOException]
  override def read(b: Array[Byte]): Int = wrappedStream.read(b)

  @throws[IOException]
  override def read(b: Array[Byte], off: Int, len: Int): Int = wrappedStream.read(b, off, len)

  @throws[IOException]
  @throws[SaneException]
  def readDeviceList: List[SaneDevice] = {
    // Status first
    readStatus match {
      case SaneStatus.STATUS_GOOD =>
        // now we're reading an array, decode the length of the array (which includes the null if the array is non-empty)
        val length: Int = readWord.integerValue - 1
        if (length > 0) {
          val result: List[SaneDevice] =
            (0 until length).map(_ =>
              readSaneDevicePointer match {
                case null =>
                  throw new IllegalStateException("null pointer encountered when not expected")

                case device =>
                  device
              }
            ).toList

          // read past a trailing byte in the response that I haven't figured out yet...
          readWord

          result
        } else
          Nil

      case status =>
        throw new SaneException(status)
    }
  }

  /**
    * Reads a single {@link SaneDevice} definition pointed to by the pointer at the current location
    * in the stream. Returns {@code null} if the pointer is a null pointer.
    */
  @throws[IOException]
  private def readSaneDevicePointer: SaneDevice = {
    if (!readPointer) {
      // TODO(sjr): why is there always a null pointer here?
      // null;
      readSaneDevice
    } else {
      // now we assume that there's a sane device ready to parse
      readSaneDevice
    }
  }

  /**
    * Reads a single pointer and returns {@code true} if it was non-null.
    */
  @throws[IOException]
  private def readPointer: Boolean = readWord.integerValue != 0

  @throws[IOException]
  private def readSaneDevice: SaneDevice = {
    val deviceName: String = readString
    val deviceVendor: String = readString
    val deviceModel: String = readString
    val deviceType: String = readString

    new SaneDevice(this.saneSession, deviceName, deviceVendor, deviceModel, deviceType)
  }

  @throws[IOException]
  def readString: String = {
    // read the length
    val length: Int = readWord.integerValue

    if (length != 0) {
      // now read all the bytes
      val input: Array[Byte] = new Array[Byte](length)
      if (ByteStreams.read(this, input, 0, length) != length)
        throw new IllegalStateException("truncated input while reading string")

      // skip the null terminator
      new String(input, 0, input.length - 1, Charsets.ISO_8859_1)
    } else
      ""
  }

  @throws[IOException]
  def readSaneParameters: SaneParameters = {
    val frame = readWord.integerValue
    val lastFrame = readWord.integerValue == 1
    val bytesPerLine = readWord.integerValue
    val pixelsPerLine = readWord.integerValue
    val lines = readWord.integerValue
    val depth = readWord.integerValue

    new SaneParameters(frame, lastFrame, bytesPerLine, pixelsPerLine, lines, depth)
  }

  @throws[IOException]
  def readStatus: SaneStatus = SaneStatus.fromWireValue(readWord.integerValue)

  @throws[IOException]
  def readWord: SaneWord = SaneWord.fromStream(this)

  @throws[IOException]
  def readOptionDescriptor: SaneOptionDescriptor = {
    // discard pointer
    readWord

    val optionName: String = readString
    val optionTitle: String = readString
    val optionDescription: String = readString

    // TODO: range check here
    val valueType: OptionValueType = OptionValueType(readWord)

    if (valueType eq OptionValueType.GROUP)
    // a new group applies!
      currentGroup = new OptionGroup(optionTitle)

    // TODO: range check here
    val units: SaneOption.OptionUnits = SaneOption.OptionUnits(readWord)

    val size: Int = readWord.integerValue

    // constraint type

    val capabilityWord: Int = readWord.integerValue

    // TODO: range check here
    val constraintType = OptionValueConstraintType(readWord)

    // decode the constraint

    var stringConstraints = ListBuffer[String]()
    var valueConstraints = ListBuffer[SaneWord]()
    var rangeConstraint: Option[RangeConstraint] = None

    constraintType match {
      case OptionValueConstraintType.NO_CONSTRAINT =>
      // inputStream.readWord(); // discard empty list

      case OptionValueConstraintType.STRING_LIST_CONSTRAINT =>
        val n = readWord.integerValue
        for (i <- 0 until n) {
          val stringConstraint = readString

          // the last element is a null terminator, don't add that
          if (i < n - 1)
            stringConstraints += stringConstraint
        }

      case OptionValueConstraintType.VALUE_LIST_CONSTRAINT =>
        val n = readWord.integerValue
        for (i <- 0 until n) {
          // first element is list length, don't add that
          val value = readWord

          if (i != 0) {
            valueConstraints += value
          }
        }

      case OptionValueConstraintType.RANGE_CONSTRAINT =>
        // discard pointer to range
        readWord

        val min: SaneWord = readWord
        val max: SaneWord = readWord
        val quantization: SaneWord = readWord

        valueType match {
          case OptionValueType.INT | OptionValueType.FIXED =>
            rangeConstraint = Some(new RangeConstraint(min, max, quantization))

          case _ =>
            SaneInputStream.logger.log(Level.WARNING, "Ignoring invalid option type/constraint combination: " + "value_type={0},constraint_type={1} for option {2}. " + "Option will be treated by jfreesane as unconstrained", Array[AnyRef](valueType, constraintType, optionName))
        }
      case _ =>
        throw new IllegalStateException("Unknown constraint type")
    }

    new SaneOptionDescriptor(
      optionName,
      optionTitle,
      optionDescription,
      currentGroup,
      valueType,
      units,
      size,
      OptionCapability.enumSet(capabilityWord),
      constraintType,
      rangeConstraint,
      stringConstraints.toList,
      valueConstraints.toList)
  }
}

object SaneInputStream {
  private val logger: Logger = Logger.getLogger(classOf[SaneInputStream].getName)
}