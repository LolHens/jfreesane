package au.com.southsky.jfreesane

import java.util

import com.google.common.collect.{ImmutableMap, Lists, Maps}

import scala.collection.JavaConversions._

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
  private def mapForType[T <: SaneEnum[T]](enumType: Class[T]): util.Map[Integer, T] =
    if (cachedTypeMaps.containsKey(enumType))
      cachedTypeMaps.get(enumType).asInstanceOf[util.Map[Integer, T]]
    else {
      val mapBuilder: ImmutableMap.Builder[Integer, T] = ImmutableMap.builder()

      for (value <- enumType.getEnumConstants)
        mapBuilder.put(value.wireValue, value)

      val result: util.Map[Integer, T] = mapBuilder.build
      cachedTypeMaps.put(enumType, result)


      result
    }

  /**
    * Returns a set of {@code T} obtained by treating {@code wireValue} as a bit vector whose bits
    * represent the wire values of the enum constants of the given {@code enumType}.
    */
  def enumSet[T <: SaneEnum[T]](enumType: Class[T], wireValue: Int): Set[T] = {
    val enumConstants: Array[T] = enumType.getEnumConstants
    val values: util.List[T] = Lists.newArrayListWithCapacity(enumConstants.length)

    for (value <- enumConstants)
      if ((wireValue & value.wireValue) != 0)
        values.add(value)

    // TODO
    values.toSet
  }

  /**
    * Returns the result of bitwise-ORing the wire values of the given {@code SaneEnum} set. This
    * method does not check to make sure the result is sensible: the caller must ensure that the set
    * contains members whose wire values can be ORed together in a logically correct fashion.
    */
  def wireValue[T <: SaneEnum[T]](values: util.Set[T]): Int = {
    var result: Int = 0

    import scala.collection.JavaConversions._
    for (value <- values)
      result |= value.wireValue

    result
  }

  def valueOf[T <: SaneEnum[T]](enumType: Class[T], valueType: Int): T = mapForType(enumType).get(valueType)

  def valueOf[T <: SaneEnum[T]](enumType: Class[T], value: SaneWord): T = valueOf(enumType, value.integerValue)
}
