package au.com.southsky.jfreesane.option

import au.com.southsky.jfreesane.enums.{OptionCapability, OptionValueConstraintType, OptionValueType}
import au.com.southsky.jfreesane.{RangeConstraint, SaneWord}

/**
  * Describes a SANE option.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneOptionDescriptor(val name: String,
                           val title: String,
                           val description: String,
                           val group: Option[OptionGroup],
                           val valueType: OptionValueType,
                           val units: SaneOption.OptionUnits,
                           val size: Int,
                           val optionCapabilities: Set[OptionCapability],
                           val constraintType: OptionValueConstraintType,
                           val rangeConstraints: Option[RangeConstraint],
                           val stringConstraints: List[String],
                           // TODO: wrong level of abstraction
                           val wordConstraints: List[SaneWord])
