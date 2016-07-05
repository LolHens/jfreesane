package au.com.southsky.jfreesane

/**
  * Represents the opcodes of SANE RPC API calls.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class SaneRpcCode(name: String, val wireValue: Int) extends SaneEnum2[SaneRpcCode](name, wireValue)

object SaneRpcCode {

  object SANE_NET_INIT extends SaneRpcCode("SANE_NET_INIT", 0)

  object SANE_NET_GET_DEVICES extends SaneRpcCode("SANE_NET_GET_DEVICES", 1)

  object SANE_NET_OPEN extends SaneRpcCode("SANE_NET_OPEN", 2)

  object SANE_NET_CLOSE extends SaneRpcCode("SANE_NET_CLOSE", 3)

  object SANE_NET_GET_OPTION_DESCRIPTORS extends SaneRpcCode("SANE_NET_GET_OPTION_DESCRIPTORS", 4)

  object SANE_NET_CONTROL_OPTION extends SaneRpcCode("SANE_NET_CONTROL_OPTION", 5)

  object SANE_NET_GET_PARAMETERS extends SaneRpcCode("SANE_NET_GET_PARAMETERS", 6)

  object SANE_NET_START extends SaneRpcCode("SANE_NET_START", 7)

  object SANE_NET_CANCEL extends SaneRpcCode("SANE_NET_CANCEL", 8)

  object SANE_NET_AUTHORIZE extends SaneRpcCode("SANE_NET_AUTHORIZE", 9)

  object SANE_NET_EXIT extends SaneRpcCode("SANE_NET_EXIT", 10)

}