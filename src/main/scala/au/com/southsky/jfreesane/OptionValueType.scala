package au.com.southsky.jfreesane

/**
  * Instances of this enum are returned by {@link SaneOption#getType} that indicate the type of value
  * that the option has.
  */
sealed class OptionValueType(name: String, typeNo: Int) extends SaneEnum2[OptionValueType](name, typeNo)

object OptionValueType {

  /**
    * The option's value is a boolean and can be written with {@link SaneOption#setBooleanValue} and
    * read by {@link SaneOption#getBooleanValue}.
    */
  object BOOLEAN extends OptionValueType("BOOLEAN", 0)

  /**
    * The option's value is an integer and can be written with {@link SaneOption#setIntegerValue} and
    * read by {@link SaneOption#getIntegerValue}.
    */
  object INT extends OptionValueType("INT", 1)

  /**
    * The option's value is of SANE's fixed-precision type and can be written with
    * {@link SaneOption#setFixedValue} and read by {@link SaneOption#getFixedValue}.
    */
  object FIXED extends OptionValueType("FIXED", 2)

  /**
    * The option's value is a string and can be written with {@link SaneOption#setStringValue} and
    * read by {@link SaneOption#getStringValue}.
    */
  object STRING extends OptionValueType("STRING", 3)

  object BUTTON extends OptionValueType("BUTTON", 4)

  object GROUP extends OptionValueType("GROUP", 5)

  val tmp_button = BUTTON

  val tmp_int = INT

  val tmp_string = STRING

  val tmp_fixed = FIXED
}