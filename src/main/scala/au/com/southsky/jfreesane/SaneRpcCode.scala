package au.com.southsky.jfreesane

/**
  * Represents the opcodes of SANE RPC API calls.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class SaneRpcCode(wireValue: Int) extends SaneEnum[SaneRpcCode](wireValue)

object SaneRpcCode extends SaneEnumObject[SaneRpcCode] {

  object SANE_NET_INIT extends SaneRpcCode(0)

  object SANE_NET_GET_DEVICES extends SaneRpcCode(1)

  object SANE_NET_OPEN extends SaneRpcCode(2)

  object SANE_NET_CLOSE extends SaneRpcCode(3)

  object SANE_NET_GET_OPTION_DESCRIPTORS extends SaneRpcCode(4)

  object SANE_NET_CONTROL_OPTION extends SaneRpcCode(5)

  object SANE_NET_GET_PARAMETERS extends SaneRpcCode(6)

  object SANE_NET_START extends SaneRpcCode(7)

  object SANE_NET_CANCEL extends SaneRpcCode(8)

  object SANE_NET_AUTHORIZE extends SaneRpcCode(9)

  object SANE_NET_EXIT extends SaneRpcCode(10)

  override def values: Set[SaneRpcCode] = Set(
    SANE_NET_INIT,
    SANE_NET_GET_DEVICES,
    SANE_NET_OPEN,
    SANE_NET_CLOSE,
    SANE_NET_GET_OPTION_DESCRIPTORS,
    SANE_NET_CONTROL_OPTION,
    SANE_NET_GET_PARAMETERS,
    SANE_NET_START,
    SANE_NET_CANCEL,
    SANE_NET_AUTHORIZE,
    SANE_NET_EXIT
  )
}