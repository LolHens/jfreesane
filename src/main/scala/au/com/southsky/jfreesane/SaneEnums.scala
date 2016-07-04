package au.com.southsky.jfreesane

import java.util

import com.google.common.collect.{ImmutableMap, Lists, Maps, Sets}

/**
  * Utilities for dealing with instances of {@link SaneEnum}.
  *
  * @author James Ring (sjr@jdns.org)
  */
final class SaneEnums private {
}

object SaneEnums {
  private val cachedTypeMaps: util.Map[Class[_], util.Map[Integer, _]] = Maps.newHashMap()

  @SuppressWarnings(Array("unchecked"))
  private def mapForType[T <: Enum[T] with SaneEnum](enumType: Class[T]): util.Map[Integer, T] =
    if (cachedTypeMaps.containsKey(enumType))
      cachedTypeMaps.get(enumType).asInstanceOf[util.Map[Integer, T]]
    else {
      val mapBuilder: ImmutableMap.Builder[Integer, T] = ImmutableMap.builder()

      for (value <- enumType.getEnumConstants)
        mapBuilder.put(value.getWireValue, value)

      val result: util.Map[Integer, T] = mapBuilder.build
      cachedTypeMaps.put(enumType, result)


      result
    }

  /**
    * Returns a set of {@code T} obtained by treating {@code wireValue} as a bit vector whose bits
    * represent the wire values of the enum constants of the given {@code enumType}.
    */
  def enumSet[T <: Enum[T] with SaneEnum](enumType: Class[T], wireValue: Int): util.Set[T] = {
    val enumConstants: Array[T] = enumType.getEnumConstants
    val values: util.List[T] = Lists.newArrayListWithCapacity(enumConstants.length)

    for (value <- enumConstants)
      if ((wireValue & value.getWireValue) != 0)
        values.add(value)

    Sets.immutableEnumSet(values)
  }

  /**
    * Returns the result of bitwise-ORing the wire values of the given {@code SaneEnum} set. This
    * method does not check to make sure the result is sensible: the caller must ensure that the set
    * contains members whose wire values can be ORed together in a logically correct fashion.
    */
  def wireValue[T <: SaneEnum](values: util.Set[T]): Int = {
    var result: Int = 0

    import scala.collection.JavaConversions._
    for (value <- values)
      result |= value.getWireValue

    result
  }

  def valueOf[T <: Enum[T] with SaneEnum](enumType: Class[T], valueType: Int): T = mapForType(enumType).get(valueType)

  def valueOf[T <: Enum[T] with SaneEnum](enumType: Class[T], value: SaneWord): T = valueOf(enumType, value.integerValue)
}
