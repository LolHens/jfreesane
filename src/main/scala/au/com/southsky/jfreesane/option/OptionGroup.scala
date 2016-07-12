package au.com.southsky.jfreesane.option

import au.com.southsky.jfreesane.enums.OptionValueType
import com.google.common.base.{MoreObjects, Preconditions}

/**
  * Represents a group of options. The SANE backend may group options together. These may be handy
  * if, for example, a JFreeSane user wants to present the options to the user in logical groups.
  *
  * @author James Ring (sjr@jdns.org)
  */
class OptionGroup(val title: String) {
  private var _options = List[SaneOption]()

  /**
    * Returns an immutable copy of the options in this group.
    */
  def options = _options

  /**
    * Adds an option to the group.
    */
  private[jfreesane] def +=(option: SaneOption) = {
    Preconditions.checkState(option.group == this)
    _options = _options :+ option
  }

  def valueType: OptionValueType = OptionValueType.GROUP

  override def toString: String =
    MoreObjects.toStringHelper(this)
      .add("title", title)
      .add("options", _options)
      .toString
}