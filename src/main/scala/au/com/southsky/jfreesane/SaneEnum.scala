package au.com.southsky.jfreesane

/**
  * Enumerations that implement this interface may be serialized in the SANE network protocol. You
  * may use {@link SaneEnums#valueOf} to look up instances of this interface by their wire value.
  *
  * @author James Ring (sjr@jdns.org)
  */
trait SaneEnum[T <: SaneEnum[T]] {
  /**
    * Returns the integer used by the SANE network protocol to represent an instance of this enum on
    * the wire.
    */
  def getWireValue: Int

  // def values: List[SaneEnum]
}

trait SaneEnumObject {
  def values: Set[AnyRef] // TODO
}