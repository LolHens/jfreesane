package au.com.southsky.jfreesane

/**
  * Created by u016595 on 01.07.2016.
  */
abstract class SaneEnum2[T <: SaneEnum2[T]](name: String, ordinal: Int, wireValue: Int = Integer.MIN_VALUE) extends Enum[T](name, ordinal) with SaneEnum {
  override val getWireValue: Int = if (wireValue == Integer.MIN_VALUE) ordinal else wireValue

  //TODO!!!
}
