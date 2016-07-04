package au.com.southsky.jfreesane

import java.util

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
                           val optionCapabilities: util.Set[OptionCapability],
                           val constraintType: OptionValueConstraintType,
                           val rangeConstraints: RangeConstraint,
                           val stringContraints: util.List[String],
                           // TODO: wrong level of abstraction
                           val wordConstraints: util.List[SaneWord]) {
  def getName: String = name

  def getTitle: String = title

  def getDescription: String = description

  def getGroup: OptionGroup = group

  def getValueType: OptionValueType = valueType

  def getUnits: SaneOption.OptionUnits = units

  def getSize: Int = size

  def getOptionCapabilities: util.Set[OptionCapability] = optionCapabilities

  def getConstraintType: OptionValueConstraintType = constraintType

  def getRangeConstraints: RangeConstraint = rangeConstraints

  def getStringConstraints: util.List[String] = stringContraints

  def getWordConstraints: util.List[SaneWord] = wordConstraints
}
