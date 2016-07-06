package au.com.southsky.jfreesane

/**
  * Represents the types of constraints that a {@link SaneOption} may be subjected to.
  *
  * Returns the description of the option as provided by the SANE backend.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class OptionValueConstraintType(wireValue: Int,
                                       val description: String) extends SaneEnum[OptionValueConstraintType](wireValue)

object OptionValueConstraintType extends SaneEnumObject[OptionValueConstraintType] {

  /**
    * The option has no constraints on its value.
    */
  object NO_CONSTRAINT extends OptionValueConstraintType(0, "No constraint")

  /**
    * The option's value is constrained to some range of values.
    */
  object RANGE_CONSTRAINT extends OptionValueConstraintType(1, "")

  /**
    * The option's value is constrained to some list of values.
    */
  object VALUE_LIST_CONSTRAINT extends OptionValueConstraintType(2, "")

  /**
    * The option's value type is {@link OptionValueType#STRING} and its value is constrained to some
    * list of string values.
    */
  object STRING_LIST_CONSTRAINT extends OptionValueConstraintType(3, "")

  override def values: Set[OptionValueConstraintType] = Set(
    NO_CONSTRAINT,
    RANGE_CONSTRAINT,
    VALUE_LIST_CONSTRAINT,
    STRING_LIST_CONSTRAINT
  )
}