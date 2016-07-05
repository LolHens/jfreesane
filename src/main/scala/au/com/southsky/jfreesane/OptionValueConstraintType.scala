package au.com.southsky.jfreesane

/**
  * Represents the types of constraints that a {@link SaneOption} may be subjected to.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class OptionValueConstraintType(name: String, wireValue: Int, _description: String) extends SaneEnum2[OptionValueConstraintType](name, wireValue) {
  /**
    * Returns the description of the option as provided by the SANE backend.
    */
  def description: String = _description
}

object OptionValueConstraintType {

  /**
    * The option has no constraints on its value.
    */
  object NO_CONSTRAINT extends OptionValueConstraintType("NO_CONSTRAINT", 0, "No constraint")

  /**
    * The option's value is constrained to some range of values.
    */
  object RANGE_CONSTRAINT extends OptionValueConstraintType("RANGE_CONSTRAINT", 1, "")

  /**
    * The option's value is constrained to some list of values.
    */
  object VALUE_LIST_CONSTRAINT extends OptionValueConstraintType("VALUE_LIST_CONSTRAINT", 2, "")

  /**
    * The option's value type is {@link OptionValueType#STRING} and its value is constrained to some
    * list of string values.
    */
  object STRING_LIST_CONSTRAINT extends OptionValueConstraintType("STRING_LIST_CONSTRAINT", 3, "")

  val tmp_stringList = STRING_LIST_CONSTRAINT

  val tmp_valueList = VALUE_LIST_CONSTRAINT

  val tmp_range = RANGE_CONSTRAINT
}