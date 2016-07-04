// Copyright 2012 Google Inc. All Rights Reserved.
package au.com.southsky.jfreesane

/**
  * Represents a restriction on the acceptable values of an option. A constrained option (see
  * {@link SaneOption#isConstrained}) whose constraint type is
  * {@link OptionValueConstraintType#RANGE_CONSTRAINT} will return an instance of {@code
  * RangeConstraint} from its {@link SaneOption#getRangeConstraints} method.
  *
  * @author James Ring (sjr@jdns.org)
  */
class RangeConstraint private[jfreesane](val min: SaneWord,
                                         val max: SaneWord,
                                         val quantum: SaneWord) {

  def getMinimumInteger: Int = min.integerValue

  def getMaximumInteger: Int = max.integerValue

  def getQuantumInteger: Int = quantum.integerValue

  def getMinimumFixed: Double = min.fixedPrecisionValue

  def getMaximumFixed: Double = max.fixedPrecisionValue

  def getQuantumFixed: Double = quantum.fixedPrecisionValue
}