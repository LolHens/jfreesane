package au.com.southsky.jfreesane

/**
  * This enum describes the capabilities possessed by an option. See <a
  * href="http://www.sane-project.org/html/doc011.html#s4.2.9.7">Sane Standard v1.0.4 section
  * 4.2.9.7</a> the official description of these values.
  *
  * @author James Ring (sjr@jdns.org)
  */
sealed class OptionCapability(name: String, ordinal: Int, capBit: Int) extends SaneEnum2[OptionCapability](name, ordinal, capBit)

object OptionCapability {

  /**
    * The option value may be set in software.
    */
  object SOFT_SELECT extends OptionCapability("SOFT_SELECT", 0, 1)

  /**
    * This option may be selected by the user on the scanner (e.g. by flipping a switch).
    */
  object HARD_SELECT extends OptionCapability("HARD_SELECT", 1, 2)

  /**
    * This option may be read in software.
    */
  object SOFT_DETECT extends OptionCapability("SOFT_DETECT", 2, 4)

  /**
    * The option is not directly supported by the scanner but is emulated by the SANE backend.
    */
  object EMULATED extends OptionCapability("EMULATED", 3, 8)

  /**
    * The option value may be automatically set by the SANE backend if desired.
    */
  object AUTOMATIC extends OptionCapability("AUTOMATIC", 4, 16)

  /**
    * The option is inactive.
    */
  object INACTIVE extends OptionCapability("INACTIVE", 5, 32)

  /**
    * The option is intended for advanced users.
    */
  object ADVANCED extends OptionCapability("ADVANCED", 6, 64)

}