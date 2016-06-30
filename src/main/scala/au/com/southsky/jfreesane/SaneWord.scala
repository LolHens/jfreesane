package au.com.southsky.jfreesane

import java.io._
import java.util.Arrays

import com.google.common.base.{Function, Preconditions}
import com.google.common.io.ByteStreams

/**
  * Represents a SANE word type. JFreeSane chooses to represent the SANE word type as an array of
  * {@link #SIZE_IN_BYTES} bytes.
  *
  * <p>
  * See <a href="http://www.sane-project.org/html/doc011.html#s4.2.1">the SANE specification</a> for
  * a thorough discussion about the SANE word type.
  *
  * @author James Ring (sjr@jdns.org)
  */
final class SaneWord private(val value: Array[Byte]) {
  /**
    * Returns a copy of the underlying byte array representing this {@link SaneWord}. The length of
    * the array is {@link #SIZE_IN_BYTES} bytes.
    */
  def getValue: Array[Byte] = {
    return Arrays.copyOf(value, value.length)
  }

  /**
    * Treats this {@link SaneWord} as an integer and returns the represented value.
    */
  def integerValue: Int = {
    try {
      return new DataInputStream(new ByteArrayInputStream(value)).readInt
    }
    catch {
      case e: IOException => {
        throw new IllegalStateException(e)
      }
    }
  }

  /**
    * Returns the value of this {@link SaneWord} treated as a SANE fixed precision value.
    */
  def fixedPrecisionValue: Double = {
    return integerValue.toDouble / SaneWord.PRECISION
  }

  override def toString: String = {
    return Arrays.toString(value)
  }
}

object SaneWord {
  /**
    * The number of bytes used to represent a SANE word.
    */
  val SIZE_IN_BYTES: Int = 4

  private val PRECISION: Int = 1 << 16

  /**
    * A function that, when applied to a {@link SaneWord} instance, returns the integer value of that
    * SANE word.
    *
    * @see SaneWord#integerValue
    */
  val TO_INTEGER_FUNCTION: Function[SaneWord, Integer] = new Function[SaneWord, Integer]() {
    def apply(word: SaneWord): Integer = {
      return word.integerValue
    }
  }

  /**
    * A function that, when applied to a {@link SaneWord} instance, returns the SANE fixed precision
    * value of that SANE word.
    *
    * @see SaneWord#fixedPrecisionValue
    */
  val TO_FIXED_FUNCTION: Function[SaneWord, Double] = new Function[SaneWord, Double]() {
    def apply(word: SaneWord): Double = {
      return word.fixedPrecisionValue
    }
  }

  /**
    * Returns a new {@code SaneWord} by consuming {@link #SIZE_IN_BYTES} bytes from the given
    * {@link InputStream}.
    */
  @throws[IOException]
  def fromStream(input: InputStream): SaneWord = {
    val newValue: Array[Byte] = new Array[Byte](SIZE_IN_BYTES)
    if (ByteStreams.read(input, newValue, 0, newValue.length) != newValue.length) {
      throw new IOException("input stream was truncated while reading a word")
    }
    return new SaneWord(newValue)
  }

  /**
    * Returns a new {@code SaneWord} representing the given integer value.
    */
  def forInt(value: Int): SaneWord = {
    val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream(SIZE_IN_BYTES)
    val stream: DataOutputStream = new DataOutputStream(byteStream)
    try {
      stream.writeInt(value)
    }
    catch {
      case e: IOException => {
        throw new IllegalArgumentException(e)
      }
    }
    return new SaneWord(byteStream.toByteArray)
  }

  /**
    * Returns a new {@code SaneWord} representing the given SANE version.
    *
    * @param major
    * the SANE major version
    * @param minor
    * the SANE minor version
    * @param build
    * the SANE build identifier
    */
  def forSaneVersion(major: Int, minor: Int, build: Int): SaneWord = {
    var result: Int = (major & 0xff) << 24
    result |= (minor & 0xff) << 16
    result |= (build & 0xffff) << 0
    return forInt(result)
  }

  /**
    * Creates a new {@link SaneWord} from a copy of the given byte array. The array must be of length
    * {@link #SIZE_IN_BYTES}, anything else will cause a runtime exception to be thrown.
    */
  def fromBytes(byteValue: Array[Byte]): SaneWord = {
    return fromBytes(byteValue, 0)
  }

  /**
    * Creates a new {@link SaneWord} from a copy of the given bytes within the array.
    * {@code offset + SIZE_IN_BYTES} must be a valid index (i.e. there must be enough bytes in the
    * array at the given offset), otherwise a runtime exception is thrown.
    */
  def fromBytes(byteValue: Array[Byte], offset: Int): SaneWord = {
    Preconditions.checkArgument(offset >= 0, "offset must be positive or zero": Object)
    Preconditions.checkArgument(offset + SIZE_IN_BYTES <= byteValue.length)
    return new SaneWord(Arrays.copyOfRange(byteValue, offset, offset + SIZE_IN_BYTES))
  }

  /**
    * Creates a new {@link SaneWord} from the given double. If {@code value} cannot be exactly
    * represented in SANE's fixed precision scheme, then
    * {@code SaneWord.forFixedPrecision(someValue).fixedPrecisionValue()} will not necessarily yield
    * {@code someValue}.
    */
  def forFixedPrecision(value: Double): SaneWord = {
    return SaneWord.forInt((value * PRECISION).toInt)
  }
}