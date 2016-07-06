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
class RangeConstraint(min: SaneWord,
                      max: SaneWord,
                      quantum: SaneWord) {

  def minInt: Int = min.integerValue

  def maxInt: Int = max.integerValue

  def quantumInt: Int = quantum.integerValue

  def minFixed: Double = min.fixedPrecisionValue

  def maxFixed: Double = max.fixedPrecisionValue

  def quantumFixed: Double = quantum.fixedPrecisionValue
}