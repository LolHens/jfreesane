package au.com.southsky.jfreesane

/**
  * Describes a SANE option.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneOptionDescriptor(val name: String,
                           val title: String,
                           val description: String,
                           val group: OptionGroup,
                           val valueType: OptionValueType,
                           val units: SaneOption.OptionUnits,
                           val size: Int,
                           val optionCapabilities: Set[OptionCapability],
                           val constraintType: OptionValueConstraintType,
                           val rangeConstraints: Option[RangeConstraint],
                           val stringConstraints: List[String],
                           // TODO: wrong level of abstraction
                           val wordConstraints: List[SaneWord])
