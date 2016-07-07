package au.com.southsky.jfreesane

import java.io.{IOException, InputStream}
import java.util
import java.util.logging.{Level, Logger}

import com.google.common.base.Charsets
import com.google.common.collect.{ImmutableList, Lists}
import com.google.common.io.ByteStreams

import scala.collection.JavaConversions._

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
  def readDeviceList: util.List[SaneDevice] = {
    // Status first
    val status: SaneStatus = readStatus

    if (!(SaneStatus.STATUS_GOOD == status))
      throw new SaneException(status)

    // now we're reading an array, decode the length of the array (which
    // includes the null if the array is non-empty)
    val length: Int = readWord.integerValue - 1
    if (length <= 0)
      return ImmutableList.of()

    val result: ImmutableList.Builder[SaneDevice] = ImmutableList.builder()

    for (i <- 0 until length) {
      val device = readSaneDevicePointer
      if (device == null)
        throw new IllegalStateException("null pointer encountered when not expected")

      result.add(device)
    }

    // read past a trailing byte in the response that I haven't figured
    // out yet...
    readWord

    result.build
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

    if (length == 0)
      return ""

    // now read all the bytes
    val input: Array[Byte] = new Array[Byte](length)
    if (ByteStreams.read(this, input, 0, length) != length)
      throw new IllegalStateException("truncated input while reading string")

    // skip the null terminator
    new String(input, 0, input.length - 1, Charsets.ISO_8859_1)
  }

  @throws[IOException]
  def readSaneParameters: SaneParameters = {
    val frame: Int = readWord.integerValue
    val lastFrame: Boolean = readWord.integerValue == 1
    val bytesPerLine: Int = readWord.integerValue
    val pixelsPerLine: Int = readWord.integerValue
    val lines: Int = readWord.integerValue
    val depth: Int = readWord.integerValue

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
    val typeInt: Int = readWord.integerValue
    // TODO: range check here
    val valueType: OptionValueType = SaneEnums.valueOf(classOf[OptionValueType], typeInt)

    if (valueType eq OptionValueType.GROUP)
    // a new group applies!
      currentGroup = new OptionGroup(optionTitle)

    val unitsInt: Int = readWord.integerValue
    // TODO: range check here
    val units: SaneOption.OptionUnits = SaneEnums.valueOf(classOf[SaneOption.OptionUnits], unitsInt)

    val size: Int = readWord.integerValue

    // constraint type

    val capabilityWord: Int = readWord.integerValue
    val constraintTypeInt: Int = readWord.integerValue
    // TODO: range check here
    val constraintType: OptionValueConstraintType = SaneEnums.valueOf(classOf[OptionValueConstraintType], constraintTypeInt)

    // decode the constraint

    var stringConstraints: util.List[String] = null
    var valueConstraints: util.List[SaneWord] = null
    var rangeConstraint: RangeConstraint = null

    constraintType match {
      case OptionValueConstraintType.NO_CONSTRAINT =>
      // inputStream.readWord(); // discard empty list

      case OptionValueConstraintType.STRING_LIST_CONSTRAINT =>
        stringConstraints = Lists.newArrayList()

        val n = readWord.integerValue
        for (i <- 0 until n) {
          val stringConstraint = readString

          // the last element is a null terminator, don't add that
          if (i < n - 1) {
            stringConstraints.add(stringConstraint)
          }
        }

      case OptionValueConstraintType.VALUE_LIST_CONSTRAINT =>
        valueConstraints = Lists.newArrayList()

        val n = readWord.integerValue
        for (i <- 0 until n) {
          // first element is list length, don't add that
          val value = readWord

          if (i != 0) {
            valueConstraints.add(value)
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
            rangeConstraint = new RangeConstraint(min, max, quantization)

          case _ =>
            SaneInputStream.logger.log(Level.WARNING, "Ignoring invalid option type/constraint combination: " + "value_type={0},constraint_type={1} for option {2}. " + "Option will be treated by jfreesane as unconstrained", Array[AnyRef](valueType, constraintType, optionName))
        }
      case _ =>
        throw new IllegalStateException("Unknown constraint type")
    }

    new SaneOptionDescriptor(optionName, optionTitle, optionDescription, currentGroup, valueType, units, size, SaneEnums.enumSet(classOf[OptionCapability], capabilityWord), constraintType, rangeConstraint, stringConstraints.toList, valueConstraints.toList)
  }
}

object SaneInputStream {
  private val logger: Logger = Logger.getLogger(classOf[SaneInputStream].getName)
}