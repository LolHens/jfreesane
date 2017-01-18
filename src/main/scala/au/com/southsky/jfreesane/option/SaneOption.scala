package au.com.southsky.jfreesane.option

import java.io.IOException
import java.nio.charset.Charset
import java.util.logging.Logger

import au.com.southsky.jfreesane._
import au.com.southsky.jfreesane.device.SaneDevice
import au.com.southsky.jfreesane.enums._
import com.google.common.base.{Charsets, Preconditions, Strings}
import com.google.common.io.ByteStreams

/**
  * This class represents a SANE device option. An option may be active or inactive (see
  * {@link #isActive}). Active options may be read (see {@link #isReadable}) and modified (see
  * {@link #isWriteable}).
  *
  * <p>
  * Options have a type (see {@link #getType}), in order to read or write an option's value, you must
  * call the getter or setter method corresponding to the option's type. For example, for an option
  * of type {@link OptionValueType#STRING}, you will call {@link #setStringValue} or
  * {@link #getStringValue}.
  *
  * <p>
  * Options may have constraints that impose restrictions on the range of values the option may take.
  * Constraints have a type which may be obtained using {@link #getConstraintType}. You may read the
  * actual constraints by calling the constraint getter method corresponding to the constraint type.
  * For example, an option of type {@link OptionValueType#INT} may have a constraint of type
  * {@link OptionValueConstraintType#VALUE_LIST_CONSTRAINT}, which you may obtain by calling
  * {@link #getIntegerValueListConstraint}.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneOption private[jfreesane](val device: SaneDevice,
                                    optionNumber: Int,
                                    descriptor: SaneOptionDescriptor) {

  descriptor.group match {
    case Some(group) if valueType == OptionValueType.GROUP =>
      group += this

    case _ =>
  }

  def name: String = descriptor.name

  def title: String = descriptor.title

  def description: String = descriptor.description

  def group: OptionGroup = descriptor.group.orNull

  def `type`: OptionValueType = descriptor.valueType

  def units: SaneOption.OptionUnits = descriptor.units

  def size: Int = descriptor.size

  def valueCount: Int = descriptor.valueType match {
    case OptionValueType.BOOLEAN | OptionValueType.STRING => 1
    case OptionValueType.INT | OptionValueType.FIXED => size / SaneWord.sizeBytes
    case OptionValueType.BUTTON | OptionValueType.GROUP =>
      throw new IllegalStateException("Option type '" + descriptor.valueType + "' has no value count")

    case _ =>
      throw new IllegalStateException("Option type '" + descriptor.valueType + "' unknown")

  }

  def isConstrained = OptionValueConstraintType.NO_CONSTRAINT != descriptor.constraintType

  def constraintType: OptionValueConstraintType = descriptor.constraintType

  def rangeConstraints: RangeConstraint = descriptor.rangeConstraints.orNull

  def stringConstraints: List[String] = descriptor.stringConstraints

  def wordConstraints: List[SaneWord] = descriptor.wordConstraints

  def integerValueListConstraint: List[Int] = descriptor.wordConstraints.map(_.intValue)

  def fixedValueListConstraint: List[Double] = descriptor.wordConstraints.map(_.fixedPrecisionValue)

  override def toString = s"Option: ${descriptor.name}, ${descriptor.title}, value type: ${descriptor.valueType}, units: ${descriptor.units}"

  private def valueType: OptionValueType = descriptor.valueType

  /**
    * Reads the current boolean value option. This option must be of type
    * {@link OptionValueType#BOOLEAN}.
    *
    * @throws IOException
    * if a problem occurred while talking to SANE
    */
  @throws[IOException]
  @throws[SaneException]
  def booleanValue: Boolean = {
    Preconditions.checkState(valueType == OptionValueType.BOOLEAN, "option is not a boolean": Object)
    Preconditions.checkState(valueCount == 1, "option is a boolean array, not boolean": Object)

    val result: SaneOption.ControlOptionResult = readOption
    SaneWord.fromBytes(result.value).intValue != 0
  }

  /**
    * Reads the current Integer value option. We do not cache value from previous get or set
    * operations so each get involves a round trip to the server.
    *
    * TODO: consider caching the returned value for "fast read" later
    *
    * @return the value of the option
    * @throws IOException
    * if a problem occurred while talking to SANE
    */
  @throws[IOException]
  @throws[SaneException]
  def integerValue: Int = {
    // check for type agreement
    Preconditions.checkState(valueType == OptionValueType.INT, "option is not an integer": Object)
    Preconditions.checkState(valueCount == 1, "option is an integer array, not integer": Object)

    // Send RCP corresponding to:
    // SANE_Status sane_control_option (SANE_Handle h, SANE_Int n, SANE_Action a, void *v, SANE_Int * i);

    val result = readOption
    Preconditions.checkState(result.`type` == OptionValueType.INT)
    Preconditions.checkState(result.valueSize == SaneWord.sizeBytes, s"unexpected value size ${result.valueSize}, expecting ${SaneWord.sizeBytes}": Object)

    // TODO: handle resource authorisation
    // TODO: check status -- may have to reload options!!
    SaneWord.fromBytes(result.value).intValue // the value
  }

  @throws[IOException]
  @throws[SaneException]
  def integerArrayValue: List[Int] = {
    val result: SaneOption.ControlOptionResult = readOption
    Preconditions.checkState(result.`type` eq OptionValueType.INT)

    (0 until result.valueSize by SaneWord.sizeBytes)
      .map(i => SaneWord.fromBytes(result.value, i).intValue)
      .toList
  }

  /**
    * Returns the value of this option interpreted as a LATIN-1 (SANE's default encoding)
    * encoded string.
    *
    * @throws IOException if a problem occurs reading the value from the SANE backend
    */
  @throws[IOException]
  @throws[SaneException]
  def stringValue: String = stringValue(Charsets.ISO_8859_1)

  @throws[IOException]
  @throws[SaneException]
  def stringValue(encoding: Charset): String = {
    Preconditions.checkState(valueType == OptionValueType.STRING, "option is not a string": Object)

    val bytes = readOption.value.takeWhile(_ != 0)
    new String(bytes, 0, bytes.length, encoding)
  }

  @throws[IOException]
  @throws[SaneException]
  def fixedValue: Double = {
    Preconditions.checkState(valueType eq OptionValueType.FIXED, "option is not of fixed precision type": Object)

    val result = readOption
    SaneWord.fromBytes(result.value).fixedPrecisionValue
  }

  @throws[IOException]
  @throws[SaneException]
  def fixedArrayValue: List[Double] = {
    val result = readOption
    Preconditions.checkState(result.`type` == OptionValueType.FIXED)

    (0 until result.valueSize by SaneWord.sizeBytes)
      .map(i => SaneWord.fromBytes(result.value, i).fixedPrecisionValue)
      .toList
  }

  @throws[IOException]
  @throws[SaneException]
  private def readOption: SaneOption.ControlOptionResult = {
    // check that this option is readable
    Preconditions.checkState(isReadable, "option is not readable": Object)
    Preconditions.checkState(isActive, "option is not active": Object)

    val out: SaneOutputStream = device.session.outputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.handle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneOption.OptionAction.GET_VALUE)

    out.write(valueType)
    out.write(SaneWord.forInt(size))

    val elementCount: Int = valueType match {
      case OptionValueType.BOOLEAN | OptionValueType.FIXED | OptionValueType.INT =>
        size / SaneWord.sizeBytes
      case OptionValueType.STRING =>
        size
      case _ =>
        throw new IllegalStateException("Unsupported type " + valueType)
    }

    out.write(SaneWord.forInt(elementCount))

    for (i <- 0 until size)
      out.write(0) // why do we need to provide a value buffer in an RPC call ???

    //read result
    SaneOption.ControlOptionResult.fromSession(device.session)
  }

  /**
    * Sets the value of the current option to the supplied boolean value. Option value must be of
    * boolean type. SANE may ignore your preference, so if you need to ensure the value has been set
    * correctly, you should examine the return value of this method.
    *
    * @return the value that the option now has according to SANE
    */
  @throws[IOException]
  @throws[SaneException]
  def booleanValue_=(value: Boolean): Boolean = {
    val result = writeOption(SaneWord.forInt(if (value) 1 else 0))
    Preconditions.checkState(result.`type` == OptionValueType.BOOLEAN)

    SaneWord.fromBytes(result.value).intValue != 0
  }

  @throws[IOException]
  @throws[SaneException]
  def setButtonValue = writeButtonOption

  /**
    * Sets the value of the current option to the supplied fixed-precision value. Option value must
    * be of fixed-precision type.
    */
  @throws[IOException]
  @throws[SaneException]
  def fixedValue_=(value: Double): Double = {
    Preconditions.checkArgument(value >= -32768 && value <= 32767.9999, s"value $value is out of range": Object)
    val wordValue: SaneWord = SaneWord.forFixedPrecision(value)
    val result = writeOption(wordValue)
    Preconditions.checkState(result.`type` == OptionValueType.FIXED, s"setFixedValue is not appropriate for option of type ${result.`type`}": Object)

    SaneWord.fromBytes(result.value).fixedPrecisionValue
  }

  /**
    * Sets the value of the current option to the supplied list of fixed-precision values. Option
    * value must be of fixed-precision type and {@link #getValueCount} must be more than 1.
    */
  @throws[IOException]
  @throws[SaneException]
  def fixedValue_=(value: List[Double]): List[Double] = {
    val wordValues: List[SaneWord] = value.map { input =>
      Preconditions.checkArgument(input >= -32768 && input <= 32767.9999, "value " + input + " is out of range": Object)
      SaneWord.forFixedPrecision(input)
    }

    val result = writeWordListOption(wordValues)

    // capacity = result.valueSize / SaneWord.sizeBytes
    (0 until result.valueSize by SaneWord.sizeBytes)
      .map(i => SaneWord.fromBytes(result.value, i).fixedPrecisionValue)
      .toList
  }

  @throws[IOException]
  @throws[SaneException]
  def stringValue_=(newValue: String): String = {
    // check for type agreement
    Preconditions.checkState(valueType eq OptionValueType.STRING)
    Preconditions.checkState(valueCount == 1)
    Preconditions.checkState(isWriteable)

    // new value must be STRICTLY less than size(), as SANE includes the
    // trailing null
    // that we will add later in its size
    Preconditions.checkState(newValue.length < size, ("string value '" + newValue + "' (length=" + newValue.length + ") exceeds maximum size of " + (size - 1) + " byte(s) for option " + name): Object)

    val result: SaneOption.ControlOptionResult = writeOption(newValue)
    Preconditions.checkState(result.`type` eq OptionValueType.STRING)

    // TODO(sjr): maybe this should go somewhere common?
    val optionValueFromServer: String = new String(result.value, 0, result.valueSize - 1, Charsets.ISO_8859_1)

    Preconditions.checkState(result.info.contains(SaneOption.OptionWriteInfo.INEXACT) ^ newValue == optionValueFromServer, "new option value does not match when it should": Object)

    optionValueFromServer
  }

  /**
    * Set the value of the current option to the supplied value. Option value must be of integer type
    *
    * TODO: consider caching the returned value for "fast read" later
    *
    * @param newValue
    * for the option
    * @return the value actually set
    * @throws IOException
    */
  @throws[IOException]
  @throws[SaneException]
  def integerValue_=(newValue: Int): Int = {
    Preconditions.checkState(valueCount == 1, "option is an array": Object)

    // check that this option is readable
    Preconditions.checkState(isWriteable)

    // Send RPC corresponding to:
    // SANE_Status sane_control_option (SANE_Handle h, SANE_Int n, SANE_Action a, void *v, SANE_Int * i);

    val result: SaneOption.ControlOptionResult = writeOption(List(newValue))
    Preconditions.checkState(result.`type` eq OptionValueType.INT)
    Preconditions.checkState(result.valueSize == SaneWord.sizeBytes)

    SaneWord.fromBytes(result.value).intValue
  }

  @throws[IOException]
  @throws[SaneException]
  def integerValue_=(newValue: List[Int]): List[Int] = {
    val result: SaneOption.ControlOptionResult = writeOption(newValue)

    //capacity = result.valueSize / SaneWord.sizeBytes

    (0 until result.valueSize by SaneWord.sizeBytes)
      .map(i => SaneWord.fromBytes(result.value, i).intValue)
      .toList
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeWordListOption(value: List[SaneWord]): SaneOption.ControlOptionResult = {
    Preconditions.checkState(isWriteable, "option is not writeable": Object)
    Preconditions.checkState(isActive, "option is not active": Object)

    val out: SaneOutputStream = device.session.outputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.handle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneWord.forInt(SaneOption.OptionAction.SET_VALUE.wireValue))
    out.write(valueType)

    out.write(SaneWord.forInt(value.size * SaneWord.sizeBytes))

    // Write the pointer to the words
    out.write(SaneWord.forInt(value.size))

    for (element <- value)
    // and the words themselves
      out.write(element)

    val result: SaneOption.ControlOptionResult = handleWriteResponse
    if (result.info.contains(SaneOption.OptionWriteInfo.RELOAD_OPTIONS) || result.info.contains(SaneOption.OptionWriteInfo.RELOAD_PARAMETERS)) {
      device.invalidateOptions
      device.listOptions
    }

    result
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(value: String): SaneOption.ControlOptionResult = {
    Preconditions.checkState(valueType eq OptionValueType.STRING)
    val out: SaneOutputStream = device.session.outputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(SaneWord.forInt(device.getHandle.handle.intValue))
    out.write(SaneWord.forInt(this.optionNumber))
    out.write(SaneWord.forInt(SaneOption.OptionAction.SET_VALUE.wireValue))
    out.write(valueType)

    // even if the string is empty, we still write out at least 1 byte (null terminator)
    out.write(SaneWord.forInt(value.length + 1))

    // write(String) takes care of writing the size for us
    out.write(value)

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(word: SaneWord): SaneOption.ControlOptionResult = writeWordListOption(List(word))

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(value: List[Int]): SaneOption.ControlOptionResult = {
    Preconditions.checkState(isActive, "option %s is not active", name)
    Preconditions.checkState(isWriteable, "option %s is not writeable", name)
    Preconditions.checkState(valueType == OptionValueType.INT, "option %s is %s-typed, you must use the corresponding methods to set the value", List(name, valueType): _*)

    val out: SaneOutputStream = device.session.outputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.handle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneOption.OptionAction.SET_VALUE)
    out.write(valueType)
    out.write(SaneWord.forInt(size))
    out.write(SaneWord.forInt(value.size))

    for (element <- value)
      out.write(SaneWord.forInt(element))

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeButtonOption: SaneOption.ControlOptionResult = {
    Preconditions.checkState(valueType eq OptionValueType.BUTTON)

    val out: SaneOutputStream = device.session.outputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.handle)
    out.write(SaneWord.forInt(this.optionNumber))
    out.write(SaneOption.OptionAction.SET_VALUE)
    out.write(valueType)
    out.write(SaneWord.forInt(0))
    out.write(SaneWord.forInt(0)) // only one value follows

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def handleWriteResponse: SaneOption.ControlOptionResult = {
    val result: SaneOption.ControlOptionResult = SaneOption.ControlOptionResult.fromSession(device.session)

    if (result.info.contains(SaneOption.OptionWriteInfo.RELOAD_OPTIONS))
      device.invalidateOptions

    result
  }

  def isActive: Boolean = !descriptor.optionCapabilities.contains(OptionCapability.INACTIVE)

  def isReadable: Boolean = descriptor.optionCapabilities.contains(OptionCapability.SOFT_DETECT)

  def isWriteable: Boolean = descriptor.optionCapabilities.contains(OptionCapability.SOFT_SELECT)

  def isHardSelectable: Boolean = descriptor.optionCapabilities.contains(OptionCapability.HARD_SELECT)
}

object SaneOption {
  private val logger: Logger = Logger.getLogger(classOf[SaneOption].getName)

  sealed class OptionAction(val actionNo: Int) extends SaneEnum[OptionAction](actionNo)

  object OptionAction extends SaneEnumObject[OptionAction] {

    object GET_VALUE extends OptionAction(0)

    object SET_VALUE extends OptionAction(1)

    object SET_AUTO extends OptionAction(2)

    override def values: Set[OptionAction] = Set(
      GET_VALUE,
      SET_VALUE,
      SET_AUTO
    )
  }

  /**
    * Instances of this enum are returned by {@link SaneOption#getUnits} indicating what units, if
    * any, the value has.
    */
  sealed class OptionUnits(wireValue: Int) extends SaneEnum[OptionUnits](wireValue)

  object OptionUnits extends SaneEnumObject[OptionUnits] {

    /**
      * The option has no units.
      */
    object UNIT_NONE extends OptionUnits(0)

    /**
      * The option unit is pixels.
      */
    object UNIT_PIXEL extends OptionUnits(1)

    /**
      * The option unit is bits.
      */
    object UNIT_BIT extends OptionUnits(2)

    /**
      * The option unit is millimeters.
      */
    object UNIT_MM extends OptionUnits(3)

    /**
      * The option unit is dots per inch.
      */
    object UNIT_DPI extends OptionUnits(4)

    /**
      * The option unit is a percentage.
      */
    object UNIT_PERCENT extends OptionUnits(5)

    /**
      * The option unit is microseconds.
      */
    object UNIT_MICROSECOND extends OptionUnits(6)

    override def values: Set[OptionUnits] = Set(
      UNIT_NONE,
      UNIT_PIXEL,
      UNIT_BIT,
      UNIT_MM,
      UNIT_DPI,
      UNIT_PERCENT,
      UNIT_MICROSECOND
    )
  }

  /**
    * Represents the information that the SANE daemon returns about the effect of modifying an
    * option.
    */
  sealed class OptionWriteInfo(wireValue: Int) extends SaneEnum[OptionWriteInfo](wireValue)

  object OptionWriteInfo extends SaneEnumObject[OptionWriteInfo] {

    /**
      * The value passed to SANE was accepted, but the SANE daemon has chosen a different
      * value than the one specified.
      */
    object INEXACT extends OptionWriteInfo(1)

    /**
      * Setting the option may have resulted in changes to other options and the client should
      * re-read options whose values it needs.
      */
    object RELOAD_OPTIONS extends OptionWriteInfo(2)

    /**
      * Setting the option may have caused a parameter set by the user to have changed.
      */
    object RELOAD_PARAMETERS extends OptionWriteInfo(4)

    override def values: Set[OptionWriteInfo] = Set(
      INEXACT,
      RELOAD_OPTIONS,
      RELOAD_PARAMETERS
    )
  }

  @throws[IOException]
  def optionsFor(device: SaneDevice): List[SaneOption] = {
    Preconditions.checkState(device.isOpen, "you must open() the device first": Object)
    var options = List[SaneOption]()
    val session = device.session
    val inputStream = session.inputStream
    val outputStream = session.outputStream

    // send SANE_NET_GET_OPTION_DESCRIPTORS
    outputStream.write(SaneRpcCode.SANE_NET_GET_OPTION_DESCRIPTORS)

    // select device
    outputStream.write(device.getHandle.handle)

    // first word of response is number of option entries
    val length = inputStream.readWord.intValue - 1
    if (length <= 0)
      Nil
    else {
      for (i <- 0 to length) {
        val option = SaneOption.fromStream(inputStream, device, i)

        if (option.valueType == OptionValueType.GROUP) {
          device.addOptionGroup(option.group)
        } else {
          // http://code.google.com/p/jfreesane/issues/detail?id=1
          // The first option always has an empty name. Sometimes we see options after the first option
          // that have empty names. Elsewhere we assume that option names are unique, so this option is
          // omitted
          if (i > 0 && Strings.isNullOrEmpty(option.name)) {
            logger.fine(s"ignoring null or empty option with id $i: $option")
          } else
            options = options ++ (option match {
              case option if option.isWriteable && option.isHardSelectable =>
                None // This option is invalid, it can't be both hardware and software selectable.
              case option if option.isWriteable && !option.isReadable =>
                None // Can't have a write-only option.
              case option if !(option.isWriteable || option.isReadable || option.isHardSelectable) =>
                None // Useless option, skip it.
              case option =>
                Some(option)
            })
        }
      }

      options
    }
  }

  @throws[IOException]
  private def fromStream(inputStream: SaneInputStream, device: SaneDevice, optionNumber: Int) =
    new SaneOption(device, optionNumber, inputStream.readOptionDescriptor)

  class ControlOptionResult(val status: Int,
                            _info: Int,
                            val `type`: OptionValueType,
                            val valueSize: Int,
                            val value: Array[Byte],
                            val resource: String) {

    val info: Set[SaneOption.OptionWriteInfo] =
      SaneOption.OptionWriteInfo.enumSet(_info)
  }

  object ControlOptionResult {
    @throws[IOException]
    @throws[SaneException]
    def fromSession(session: SaneSession): SaneOption.ControlOptionResult = {
      val stream = session.inputStream

      // Expected record format:
      // SANE_Status status
      // SANE_Word info
      // SANE_Word value_type
      // SANE_Word value_size
      // void *value
      // SANE_String *resource
      // See http://sane-project.org/html/doc017.html#s5.2.6
      var status: SaneWord = stream.readWord

      if (status.intValue != 0)
        throw SaneException(status)

      var info: Int = stream.readWord.intValue
      var `type`: OptionValueType = OptionValueType(stream.readWord.intValue)
      var valueSize: Int = stream.readWord.intValue

      // read the pointer
      var pointer = stream.readWord.intValue
      var value: Array[Byte] =
        if (pointer == 0)
          null
        else {
          val value = new Array[Byte](valueSize)

          if (ByteStreams.read(stream, value, 0, valueSize) != valueSize)
            throw new IOException("truncated read while getting value")
          else
            value
        }

      val resource: String = stream.readString
      if (!resource.isEmpty) {
        session.authorize(resource)
        status = stream.readWord

        if (status.intValue != 0)
          throw SaneException(status)

        info = stream.readWord.intValue

        `type` = OptionValueType(stream.readWord.intValue)

        valueSize = stream.readWord.intValue

        // read the pointer
        pointer = stream.readWord.intValue
        value =
          if (pointer == 0)
            null
          else {
            val value = new Array[Byte](valueSize)

            if (stream.read(value) != valueSize)
              throw new IOException("truncated read while getting value")
            else
              value
          }
      }

      new SaneOption.ControlOptionResult(status.intValue, info, `type`, valueSize, value, resource)
    }
  }

}