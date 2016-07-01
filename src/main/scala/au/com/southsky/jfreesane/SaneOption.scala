package au.com.southsky.jfreesane

import java.io.IOException
import java.nio.charset.Charset
import java.util
import java.util.logging.Logger

import com.google.common.base.{Charsets, Function, Preconditions, Strings}
import com.google.common.collect.{ImmutableList, Lists, Sets}
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
class SaneOption private[jfreesane](val device: SaneDevice, val optionNumber: Int, val descriptor: SaneOptionDescriptor) {
  if (descriptor.getGroup != null && (getValueType ne OptionValueType.GROUP)) {
    descriptor.getGroup.addOption(this)
  }

  def getDevice: SaneDevice = device

  def getName: String = descriptor.getName

  def getTitle: String = descriptor.getTitle

  def getDescription: String = descriptor.getDescription

  def getGroup: OptionGroup = descriptor.getGroup

  def getType: OptionValueType = descriptor.getValueType

  def getUnits: SaneOption.OptionUnits = descriptor.getUnits

  def getSize: Int = descriptor.getSize

  def getValueCount: Int = {
    descriptor.getValueType match {
      case OptionValueType.BOOLEAN | OptionValueType.STRING =>
        return 1
      case OptionValueType.INT | OptionValueType.FIXED =>
        return getSize / SaneWord.SIZE_IN_BYTES
      case OptionValueType.BUTTON | OptionValueType.GROUP =>
        throw new IllegalStateException("Option type '" + descriptor.getValueType + "' has no value count")
      case _ =>
        throw new IllegalStateException("Option type '" + descriptor.getValueType + "' unknown")
    }
  }

  def isConstrained: Boolean = !(OptionValueConstraintType.NO_CONSTRAINT == descriptor.getConstraintType)

  def getConstraintType: OptionValueConstraintType = descriptor.getConstraintType

  def getRangeConstraints: RangeConstraint = descriptor.getRangeConstraints

  def getStringConstraints: util.List[String] = descriptor.getStringConstraints

  def getWordConstraints: util.List[SaneWord] = descriptor.getWordConstraints

  def getIntegerValueListConstraint: util.List[Integer] = Lists.transform(descriptor.getWordConstraints, SaneWord.TO_INTEGER_FUNCTION)

  def getFixedValueListConstraint: util.List[java.lang.Double] = Lists.transform(descriptor.getWordConstraints, SaneWord.TO_FIXED_FUNCTION)

  override def toString: String = String.format("Option: %s, %s, value type: %s, units: %s", descriptor.getName, descriptor.getTitle, descriptor.getValueType, descriptor.getUnits)

  private def getValueType: OptionValueType = descriptor.getValueType

  /**
    * Reads the current boolean value option. This option must be of type
    * {@link OptionValueType#BOOLEAN}.
    *
    * @throws IOException
    * if a problem occurred while talking to SANE
    */
  @throws[IOException]
  @throws[SaneException]
  def getBooleanValue: Boolean = {
    Preconditions.checkState(getValueType eq OptionValueType.BOOLEAN, "option is not a boolean": Object)
    Preconditions.checkState(getValueCount == 1, "option is a boolean array, not boolean": Object)

    val result: SaneOption.ControlOptionResult = readOption
    SaneWord.fromBytes(result.getValue).integerValue != 0
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
  def getIntegerValue: Int = {
    // check for type agreement
    Preconditions.checkState(getValueType eq OptionValueType.INT, "option is not an integer": Object)
    Preconditions.checkState(getValueCount == 1, "option is an integer array, not integer": Object)

    // Send RCP corresponding to:
    // SANE_Status sane_control_option (SANE_Handle h, SANE_Int n, SANE_Action a, void *v, SANE_Int * i);

    val result: SaneOption.ControlOptionResult = readOption
    Preconditions.checkState(result.getType eq OptionValueType.INT)
    Preconditions.checkState(result.getValueSize == SaneWord.SIZE_IN_BYTES, ("unexpected value size " + result.getValueSize + ", expecting " + SaneWord.SIZE_IN_BYTES): Object)

    // TODO: handle resource authorisation
    // TODO: check status -- may have to reload options!!
    SaneWord.fromBytes(result.getValue).integerValue // the value
  }

  @throws[IOException]
  @throws[SaneException]
  def getIntegerArrayValue: util.List[Int] = {
    val result: SaneOption.ControlOptionResult = readOption
    Preconditions.checkState(result.getType eq OptionValueType.INT)

    val values = Lists.newArrayList[Int]()
    for (i <- 0 until result.getValueSize by SaneWord.SIZE_IN_BYTES)
      values.add(SaneWord.fromBytes(result.getValue, i).integerValue)

    values
  }

  /**
    * Returns the value of this option interpreted as a LATIN-1 (SANE's default encoding)
    * encoded string.
    *
    * @throws IOException if a problem occurs reading the value from the SANE backend
    */
  @throws[IOException]
  @throws[SaneException]
  def getStringValue: String = getStringValue(Charsets.ISO_8859_1)

  @throws[IOException]
  @throws[SaneException]
  def getStringValue(encoding: Charset): String = {
    Preconditions.checkState(getValueType eq OptionValueType.STRING, "option is not a string": Object)

    val result: SaneOption.ControlOptionResult = readOption
    val value: Array[Byte] = result.getValue.takeWhile(_ != 0)

    new String(value, 0, value.length, encoding)
  }

  @throws[IOException]
  @throws[SaneException]
  def getFixedValue: Double = {
    Preconditions.checkState(getValueType eq OptionValueType.FIXED, "option is not of fixed precision type": Object)

    val result: SaneOption.ControlOptionResult = readOption
    SaneWord.fromBytes(result.getValue).fixedPrecisionValue
  }

  @throws[IOException]
  @throws[SaneException]
  def getFixedArrayValue: util.List[Double] = {
    val result: SaneOption.ControlOptionResult = readOption
    Preconditions.checkState(result.getType eq OptionValueType.FIXED)

    val values = Lists.newArrayList[Double]()
    for (i <- 0 until result.getValueSize by SaneWord.SIZE_IN_BYTES)
      values.add(SaneWord.fromBytes(result.getValue, i).fixedPrecisionValue)

    values
  }

  @throws[IOException]
  @throws[SaneException]
  private def readOption: SaneOption.ControlOptionResult = {
    // check that this option is readable
    Preconditions.checkState(isReadable, "option is not readable": Object)
    Preconditions.checkState(isActive, "option is not active": Object)

    val out: SaneOutputStream = device.getSession.getOutputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.getHandle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneOption.OptionAction.GET_VALUE)

    out.write(getValueType)
    out.write(SaneWord.forInt(getSize))

    val elementCount: Int = getValueType match {
      case OptionValueType.BOOLEAN | OptionValueType.FIXED | OptionValueType.INT =>
        getSize / SaneWord.SIZE_IN_BYTES
      case OptionValueType.STRING =>
        getSize
      case _ =>
        throw new IllegalStateException("Unsupported type " + getValueType)
    }

    out.write(SaneWord.forInt(elementCount))

    for (i <- 0 until getSize)
      out.write(0); // why do we need to provide a value buffer in an RPC call ???

    //read result
    SaneOption.ControlOptionResult.fromSession(device.getSession)
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
  def setBooleanValue(value: Boolean): Boolean = {
    val result: SaneOption.ControlOptionResult = writeOption(SaneWord.forInt(if (value) 1 else 0))
    Preconditions.checkState(result.getType eq OptionValueType.BOOLEAN)

    SaneWord.fromBytes(result.getValue).integerValue != 0
  }

  @throws[IOException]
  @throws[SaneException]
  def setButtonValue {
    writeButtonOption
  }

  /**
    * Sets the value of the current option to the supplied fixed-precision value. Option value must
    * be of fixed-precision type.
    */
  @throws[IOException]
  @throws[SaneException]
  def setFixedValue(value: Double): Double = {
    Preconditions.checkArgument(value >= -32768 && value <= 32767.9999, ("value " + value + " is out of range"): Object)
    val wordValue: SaneWord = SaneWord.forFixedPrecision(value)
    val result: SaneOption.ControlOptionResult = writeOption(wordValue)
    Preconditions.checkState(result.getType eq OptionValueType.FIXED, ("setFixedValue is not appropriate for option of type " + result.getType): Object)

    SaneWord.fromBytes(result.getValue).fixedPrecisionValue
  }

  /**
    * Sets the value of the current option to the supplied list of fixed-precision values. Option
    * value must be of fixed-precision type and {@link #getValueCount} must be more than 1.
    */
  @throws[IOException]
  @throws[SaneException]
  def setFixedValue(value: util.List[Double]): util.List[Double] = {
    val wordValues: util.List[SaneWord] = Lists.transform(value, new Function[Double, SaneWord]() {
      def apply(input: Double): SaneWord = {
        Preconditions.checkArgument(input >= -32768 && input <= 32767.9999, "value " + input + " is out of range": Object)
        SaneWord.forFixedPrecision(input)
      }
    })

    val result = writeWordListOption(wordValues)

    val newValues: util.List[Double] = Lists.newArrayListWithCapacity(result.getValueSize / SaneWord.SIZE_IN_BYTES)
    for (i <- 0 until result.getValueSize by SaneWord.SIZE_IN_BYTES)
      newValues.add(SaneWord.fromBytes(result.getValue, i).fixedPrecisionValue)

    newValues
  }

  @throws[IOException]
  @throws[SaneException]
  def setStringValue(newValue: String): String = {
    // check for type agreement
    Preconditions.checkState(getValueType eq OptionValueType.STRING)
    Preconditions.checkState(getValueCount == 1)
    Preconditions.checkState(isWriteable)

    // new value must be STRICTLY less than size(), as SANE includes the
    // trailing null
    // that we will add later in its size
    Preconditions.checkState(newValue.length < getSize, ("string value '" + newValue + "' (length=" + newValue.length + ") exceeds maximum size of " + (getSize - 1) + " byte(s) for option " + getName): Object)

    val result: SaneOption.ControlOptionResult = writeOption(newValue)
    Preconditions.checkState(result.getType eq OptionValueType.STRING)

    // TODO(sjr): maybe this should go somewhere common?
    val optionValueFromServer: String = new String(result.getValue, 0, result.getValueSize - 1, Charsets.ISO_8859_1)

    Preconditions.checkState(result.getInfo.contains(SaneOption.OptionWriteInfo.INEXACT) ^ newValue == optionValueFromServer, "new option value does not match when it should": Object)

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
  def setIntegerValue(newValue: Int): Int = {
    Preconditions.checkState(getValueCount == 1, "option is an array": Object)

    // check that this option is readable
    Preconditions.checkState(isWriteable)

    // Send RPC corresponding to:
    // SANE_Status sane_control_option (SANE_Handle h, SANE_Int n, SANE_Action a, void *v, SANE_Int * i);

    val result: SaneOption.ControlOptionResult = writeOption(ImmutableList.of[Integer](newValue))
    Preconditions.checkState(result.getType eq OptionValueType.INT)
    Preconditions.checkState(result.getValueSize == SaneWord.SIZE_IN_BYTES)

    SaneWord.fromBytes(result.getValue).integerValue
  }

  @throws[IOException]
  @throws[SaneException]
  def setIntegerValue(newValue: util.List[Integer]): util.List[Integer] = {
    val result: SaneOption.ControlOptionResult = writeOption(newValue)

    val newValues: util.List[Integer] = Lists.newArrayListWithCapacity(result.getValueSize / SaneWord.SIZE_IN_BYTES)
    var i: Int = 0
    for (i <- 0 until result.getValueSize by SaneWord.SIZE_IN_BYTES)
      newValues.add(SaneWord.fromBytes(result.getValue, i).integerValue)

    newValues
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeWordListOption(value: util.List[SaneWord]): SaneOption.ControlOptionResult = {
    Preconditions.checkState(isWriteable, "option is not writeable": Object)
    Preconditions.checkState(isActive, "option is not active": Object)

    val out: SaneOutputStream = device.getSession.getOutputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.getHandle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneWord.forInt(SaneOption.OptionAction.SET_VALUE.getWireValue))
    out.write(getValueType)

    out.write(SaneWord.forInt(value.size * SaneWord.SIZE_IN_BYTES))

    // Write the pointer to the words
    out.write(SaneWord.forInt(value.size))


    import scala.collection.JavaConversions._
    for (element <- value)
    // and the words themselves
      out.write(element)

    val result: SaneOption.ControlOptionResult = handleWriteResponse
    if (result.getInfo.contains(SaneOption.OptionWriteInfo.RELOAD_OPTIONS) || result.getInfo.contains(SaneOption.OptionWriteInfo.RELOAD_PARAMETERS)) {
      device.invalidateOptions
      device.listOptions
    }

    result
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(value: String): SaneOption.ControlOptionResult = {
    Preconditions.checkState(getValueType eq OptionValueType.STRING)
    val out: SaneOutputStream = device.getSession.getOutputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(SaneWord.forInt(device.getHandle.getHandle.integerValue))
    out.write(SaneWord.forInt(this.optionNumber))
    out.write(SaneWord.forInt(SaneOption.OptionAction.SET_VALUE.getWireValue))
    out.write(getValueType)

    // even if the string is empty, we still write out at least 1 byte (null terminator)
    out.write(SaneWord.forInt(value.length + 1))

    // write(String) takes care of writing the size for us
    out.write(value)

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(word: SaneWord): SaneOption.ControlOptionResult = writeWordListOption(ImmutableList.of(word))

  @throws[IOException]
  @throws[SaneException]
  private def writeOption(value: util.List[Integer]): SaneOption.ControlOptionResult = {
    Preconditions.checkState(isActive, "option %s is not active", getName)
    Preconditions.checkState(isWriteable, "option %s is not writeable", getName)
    Preconditions.checkState(getValueType eq OptionValueType.INT, "option %s is %s-typed, you must use the corresponding methods to set the value", getName, getValueType)

    val out: SaneOutputStream = device.getSession.getOutputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.getHandle)
    out.write(SaneWord.forInt(optionNumber))
    out.write(SaneOption.OptionAction.SET_VALUE)
    out.write(getValueType)
    out.write(SaneWord.forInt(getSize))
    out.write(SaneWord.forInt(value.size))

    import scala.collection.JavaConversions._
    for (element <- value)
      out.write(SaneWord.forInt(element))

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def writeButtonOption: SaneOption.ControlOptionResult = {
    Preconditions.checkState(getValueType eq OptionValueType.BUTTON)

    val out: SaneOutputStream = device.getSession.getOutputStream
    out.write(SaneRpcCode.SANE_NET_CONTROL_OPTION)
    out.write(device.getHandle.getHandle)
    out.write(SaneWord.forInt(this.optionNumber))
    out.write(SaneOption.OptionAction.SET_VALUE)
    out.write(getValueType)
    out.write(SaneWord.forInt(0))
    out.write(SaneWord.forInt(0)) // only one value follows

    handleWriteResponse
  }

  @throws[IOException]
  @throws[SaneException]
  private def handleWriteResponse: SaneOption.ControlOptionResult = {
    val result: SaneOption.ControlOptionResult = SaneOption.ControlOptionResult.fromSession(device.getSession)

    if (result.getInfo.contains(SaneOption.OptionWriteInfo.RELOAD_OPTIONS))
      device.invalidateOptions

    result
  }

  def isActive: Boolean = !descriptor.getOptionCapabilities.contains(OptionCapability.INACTIVE)

  def isReadable: Boolean = descriptor.getOptionCapabilities.contains(OptionCapability.SOFT_DETECT)

  def isWriteable: Boolean = descriptor.getOptionCapabilities.contains(OptionCapability.SOFT_SELECT)
}

object SaneOption {
  private val logger: Logger = Logger.getLogger(classOf[SaneOption].getName)

  sealed class OptionAction(name: String, ordinal: Int, val actionNo: Int) extends Enum[OptionAction](name, ordinal) with SaneEnum {
    override def getWireValue: Int = actionNo
  }

  object OptionAction {

    object GET_VALUE extends OptionAction("GET_VALUE", 0, 0)

    object SET_VALUE extends OptionAction("SET_VALUE", 1, 1)

    object SET_AUTO extends OptionAction("SET_AUTO", 2, 2)

  }

  /**
    * Instances of this enum are returned by {@link SaneOption#getUnits} indicating what units, if
    * any, the value has.
    */
  sealed class OptionUnits(name: String, ordinal: Int, val wireValue: Int) extends Enum[OptionUnits](name, ordinal) with SaneEnum {
    override def getWireValue: Int = wireValue
  }

  object OptionUnits {

    /**
      * The option has no units.
      */
    object UNIT_NONE extends OptionUnits("UNIT_NONE", 0, 0)

    /**
      * The option unit is pixels.
      */
    object UNIT_PIXEL extends OptionUnits("UNIT_PIXEL", 1, 1)

    /**
      * The option unit is bits.
      */
    object UNIT_BIT extends OptionUnits("UNIT_BIT", 2, 2)

    /**
      * The option unit is millimeters.
      */
    object UNIT_MM extends OptionUnits("UNIT_MM", 3, 3)

    /**
      * The option unit is dots per inch.
      */
    object UNIT_DPI extends OptionUnits("UNIT_DPI", 4, 4)

    /**
      * The option unit is a percentage.
      */
    object UNIT_PERCENT extends OptionUnits("UNIT_PERCENT", 5, 5)

    /**
      * The option unit is microseconds.
      */
    object UNIT_MICROSECOND extends OptionUnits("UNIT_MICROSECOND", 6, 6)

  }

  /**
    * Represents the information that the SANE daemon returns about the effect of modifying an
    * option.
    */
  sealed class OptionWriteInfo(name: String, ordinal: Int, val wireValue: Int) extends Enum[OptionWriteInfo](name, ordinal) with SaneEnum {
    override def getWireValue: Int = wireValue
  }

  object OptionWriteInfo {

    /**
      * The value passed to SANE was accepted, but the SANE daemon has chosen a different
      * value than the one specified.
      */
    object INEXACT extends OptionWriteInfo("INEXACT", 0, 1)

    /**
      * Setting the option may have resulted in changes to other options and the client should
      * re-read options whose values it needs.
      */
    object RELOAD_OPTIONS extends OptionWriteInfo("RELOAD_OPTIONS", 1, 2)

    /**
      * Setting the option may have caused a parameter set by the user to have changed.
      */
    object RELOAD_PARAMETERS extends OptionWriteInfo("RELOAD_PARAMETERS", 2, 4)

  }

  @throws[IOException]
  def optionsFor(device: SaneDevice): util.List[SaneOption] = {
    Preconditions.checkState(device.isOpen, "you must open() the device first": Object)
    val options: util.List[SaneOption] = Lists.newArrayList()
    val session: SaneSession = device.getSession
    val inputStream: SaneInputStream = session.getInputStream
    val outputStream: SaneOutputStream = session.getOutputStream

    // send SANE_NET_GET_OPTION_DESCRIPTORS
    outputStream.write(SaneRpcCode.SANE_NET_GET_OPTION_DESCRIPTORS)

    // select device
    outputStream.write(device.getHandle.getHandle)

    // first word of response is number of option entries
    val length: Int = inputStream.readWord.integerValue - 1
    if (length <= 0)
      return ImmutableList.of()

    for (i <- 0 to length) {
      val option = SaneOption.fromStream(inputStream, device, i)

      if (option.getValueType == OptionValueType.GROUP) {
        device.addOptionGroup(option.getGroup)
      } else {
        // http://code.google.com/p/jfreesane/issues/detail?id=1
        // The first option always has an empty name. Sometimes we see options after the first option
        // that have empty names. Elsewhere we assume that option names are unique, so this option is
        // omitted
        if (i > 0 && Strings.isNullOrEmpty(option.getName)) {
          logger.fine(s"ignoring null or empty option with id $i: $option")
        } else
          options.add(option)
      }
    }

    options
  }

  @throws[IOException]
  private def fromStream(inputStream: SaneInputStream, device: SaneDevice, optionNumber: Int) = new SaneOption(device, optionNumber, inputStream.readOptionDescriptor)

  class ControlOptionResult private(val status: Int,
                                    val _info: Int,
                                    val `type`: OptionValueType,
                                    val valueSize: Int,
                                    val value: Array[Byte],
                                    val resource: String) {
    val info = SaneEnums.enumSet[SaneOption.OptionWriteInfo](classOf[SaneOption.OptionWriteInfo], _info)

    def getStatus: Int = status

    def getInfo: util.Set[SaneOption.OptionWriteInfo] = Sets.immutableEnumSet(info)

    def getType: OptionValueType = `type`

    def getValueSize: Int = valueSize

    def getValue: Array[Byte] = value

    def getResource: String = resource
  }

  object ControlOptionResult {
    @throws[IOException]
    @throws[SaneException]
    def fromSession(session: SaneSession): SaneOption.ControlOptionResult = {
      val stream: SaneInputStream = session.getInputStream

      var status: SaneWord = stream.readWord

      if (status.integerValue != 0)
        throw SaneException.fromStatusWord(status)

      var info: Int = stream.readWord.integerValue

      var `type`: OptionValueType = SaneEnums.valueOf(classOf[OptionValueType], stream.readWord.integerValue)

      var valueSize: Int = stream.readWord.integerValue

      // read the pointer
      var pointer: Int = stream.readWord.integerValue
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

        if (status.integerValue != 0)
          throw SaneException.fromStatusWord(status)

        info = stream.readWord.integerValue

        `type` = SaneEnums.valueOf(classOf[OptionValueType], stream.readWord.integerValue)

        valueSize = stream.readWord.integerValue

        // read the pointer
        pointer = stream.readWord.integerValue
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

      new SaneOption.ControlOptionResult(status.integerValue, info, `type`, valueSize, value, resource)
    }
  }

}