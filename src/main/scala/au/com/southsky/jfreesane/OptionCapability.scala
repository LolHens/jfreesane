package au.com.southsky.jfreesane

/**
  * This enum describes the capabilities possessed by an option. See <a
  * href="http://www.sane-project.org/html/doc011.html#s4.2.9.7">Sane Standard v1.0.4 section
  * 4.2.9.7</a> the official description of these values.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class OptionCapability(val capBit: Int) extends SaneEnum[OptionCapability](capBit)

object OptionCapability extends SaneEnumObject[OptionCapability] {

  /**
    * The option value may be set in software.
    */
  object SOFT_SELECT extends OptionCapability(1)

  /**
    * This option may be selected by the user on the scanner (e.g. by flipping a switch).
    */
  object HARD_SELECT extends OptionCapability(2)

  /**
    * This option may be read in software.
    */
  object SOFT_DETECT extends OptionCapability(4)

  /**
    * The option is not directly supported by the scanner but is emulated by the SANE backend.
    */
  object EMULATED extends OptionCapability(8)

  /**
    * The option value may be automatically set by the SANE backend if desired.
    */
  object AUTOMATIC extends OptionCapability(16)

  /**
    * The option is inactive.
    */
  object INACTIVE extends OptionCapability(32)

  /**
    * The option is intended for advanced users.
    */
  object ADVANCED extends OptionCapability(64)

  override def values: Set[OptionCapability] = Set(
    SOFT_SELECT,
    HARD_SELECT,
    SOFT_DETECT,
    EMULATED,
    AUTOMATIC,
    INACTIVE,
    ADVANCED
  )
}