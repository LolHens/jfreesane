package au.com.southsky.jfreesane.enums

import au.com.southsky.jfreesane.SaneWord

/**
  * Enumerations that implement this interface may be serialized in the SANE network protocol. You
  * may use {@link SaneEnums#valueOf} to look up instances of this interface by their wire value.
  *
  * Returns the integer used by the SANE network protocol to represent an instance of this enum on
  * the wire.
  *
  * @author James Ring (sjr@jdns.org)
  */
abstract class SaneEnum[T <: SaneEnum[T]](val wireValue: Int)

trait SaneEnumObject[T <: SaneEnum[T]] {
  def values: Set[T]

  /**
    * Returns a set of {@code T} obtained by treating {@code wireValue} as a bit vector whose bits
    * represent the wire values of the enum constants of the given {@code enumType}.
    */
  def enumSet(wireValue: Int): Set[T] = values.filter(value => (wireValue & value.wireValue) != 0)

  /**
    * Returns the result of bitwise-ORing the wire values of the given {@code SaneEnum} set. This
    * method does not check to make sure the result is sensible: the caller must ensure that the set
    * contains members whose wire values can be ORed together in a logically correct fashion.
    */
  def wireValue(values: Set[T]): Int = values.foldLeft(0)((last, value) => last | value.wireValue)

  def apply(wireValue: Int): T = values.find(_.wireValue == wireValue).get

  def apply(value: SaneWord): T = this (value.intValue)
}