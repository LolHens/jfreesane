package au.com.southsky.jfreesane.enums

/**
  * Instances of this enum are returned by {@link SaneOption#getType} that indicate the type of value
  * that the option has.
  */
sealed class OptionValueType(typeNo: Int) extends SaneEnum[OptionValueType](typeNo)

object OptionValueType extends SaneEnumObject[OptionValueType] {

  /**
    * The option's value is a boolean and can be written with {@link SaneOption#setBooleanValue} and
    * read by {@link SaneOption#getBooleanValue}.
    */
  object BOOLEAN extends OptionValueType(0)

  /**
    * The option's value is an integer and can be written with {@link SaneOption#setIntegerValue} and
    * read by {@link SaneOption#getIntegerValue}.
    */
  object INT extends OptionValueType(1)

  /**
    * The option's value is of SANE's fixed-precision type and can be written with
    * {@link SaneOption#setFixedValue} and read by {@link SaneOption#getFixedValue}.
    */
  object FIXED extends OptionValueType(2)

  /**
    * The option's value is a string and can be written with {@link SaneOption#setStringValue} and
    * read by {@link SaneOption#getStringValue}.
    */
  object STRING extends OptionValueType(3)

  object BUTTON extends OptionValueType(4)

  object GROUP extends OptionValueType(5)

  override def values: Set[OptionValueType] = Set(
    BOOLEAN,
    INT,
    FIXED,
    STRING,
    BUTTON,
    GROUP
  )
}