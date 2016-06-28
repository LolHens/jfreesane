package au.com.southsky.jfreesane

import java.util
import com.google.common.base.MoreObjects
import com.google.common.base.Preconditions
import com.google.common.collect.ImmutableList
import com.google.common.collect.Lists

/**
  * Represents a group of options. The SANE backend may group options together. These may be handy
  * if, for example, a JFreeSane user wants to present the options to the user in logical groups.
  *
  * @author James Ring (sjr@jdns.org)
  */
class OptionGroup(val title: String) {
  private val options: util.List[SaneOption] = Lists.newArrayList()

  def getTitle: String = title

  def getValueType: OptionValueType = OptionValueType.GROUP

  /**
    * Returns an immutable copy of the options in this group.
    */
  def getOptions: util.List[SaneOption] = ImmutableList.copyOf(options.iterator())

  /**
    * Adds an option to the group.
    */
  private[jfreesane] def addOption(option: SaneOption) = {
    Preconditions.checkState(option.getGroup eq this)
    options.add(option)
  }

  override def toString: String = MoreObjects.toStringHelper(this).add("title", title).add("options", options).toString
}