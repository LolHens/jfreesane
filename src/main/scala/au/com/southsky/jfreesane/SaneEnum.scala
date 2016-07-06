package au.com.southsky.jfreesane

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
}