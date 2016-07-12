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
class RangeConstraint(_min: SaneWord,
                      _max: SaneWord,
                      _quantization: SaneWord) {

  lazy val minInt = _min.intValue

  lazy val maxInt = _max.intValue

  lazy val quantizationInt = _quantization.intValue

  lazy val minFixed = _min.fixedPrecisionValue

  lazy val maxFixed = _max.fixedPrecisionValue

  lazy val quantizationFixed = _quantization.fixedPrecisionValue
}