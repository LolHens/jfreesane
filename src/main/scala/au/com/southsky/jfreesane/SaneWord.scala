package au.com.southsky.jfreesane

import java.io._
import java.util.Arrays

import com.google.common.base.Preconditions
import com.google.common.io.ByteStreams

/**
  * Represents a SANE word type. JFreeSane chooses to represent the SANE word type as an array of
  * {@link #SIZE_IN_BYTES} bytes.
  *
  * <p>
  * See <a href="http://www.sane-project.org/html/doc011.html#s4.2.1">the SANE specification</a> for
  * a thorough discussion about the SANE word type.
  *
  * Returns a copy of the underlying byte array representing this {@link SaneWord}. The length of
  * the array is {@link #SIZE_IN_BYTES} bytes.
  *
  * @author James Ring (sjr@jdns.org)
  */
final class SaneWord(val bytes: Array[Byte]) {
  /**
    * Treats this {@link SaneWord} as an integer and returns the represented value.
    */
  def intValue: Int = try {
    new DataInputStream(new ByteArrayInputStream(bytes)).readInt
  } catch {
    case e: IOException =>
      throw new IllegalStateException(e)
  }

  /**
    * Returns the value of this {@link SaneWord} treated as a SANE fixed precision value.
    */
  def fixedPrecisionValue: Double = intValue.toDouble / SaneWord.precision

  override def toString: String = Arrays.toString(bytes)
}

object SaneWord {
  /**
    * The number of bytes used to represent a SANE word.
    */
  val sizeBytes: Int = 4

  private val precision: Int = 1 << 16

  /**
    * Returns a new {@code SaneWord} by consuming {@link #SIZE_IN_BYTES} bytes from the given
    * {@link InputStream}.
    */
  @throws[IOException]
  def fromStream(input: InputStream): SaneWord = {
    val newValue = new Array[Byte](sizeBytes)

    if (ByteStreams.read(input, newValue, 0, newValue.length) != newValue.length)
      throw new IOException("input stream was truncated while reading a word")

    new SaneWord(newValue)
  }

  /**
    * Returns a new {@code SaneWord} representing the given integer value.
    */
  def forInt(value: Int): SaneWord = {
    val byteStream: ByteArrayOutputStream = new ByteArrayOutputStream(sizeBytes)

    new DataOutputStream(byteStream).writeInt(value)

    new SaneWord(byteStream.toByteArray)
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
  def forSaneVersion(major: Int, minor: Int, build: Int): SaneWord =
  forInt(
    (major & 0xff) << 24 |
      (minor & 0xff) << 16 |
      (build & 0xffff) << 0
  )

  /**
    * Creates a new {@link SaneWord} from a copy of the given byte array. The array must be of length
    * {@link #SIZE_IN_BYTES}, anything else will cause a runtime exception to be thrown.
    */
  def fromBytes(byteValue: Array[Byte]): SaneWord = fromBytes(byteValue, 0)

  /**
    * Creates a new {@link SaneWord} from a copy of the given bytes within the array.
    * {@code offset + SIZE_IN_BYTES} must be a valid index (i.e. there must be enough bytes in the
    * array at the given offset), otherwise a runtime exception is thrown.
    */
  def fromBytes(byteValue: Array[Byte], offset: Int): SaneWord = {
    Preconditions.checkArgument(offset >= 0, "offset must be positive or zero": Object)
    Preconditions.checkArgument(offset + sizeBytes <= byteValue.length)

    new SaneWord(Arrays.copyOfRange(byteValue, offset, offset + sizeBytes))
  }

  /**
    * Creates a new {@link SaneWord} from the given double. If {@code value} cannot be exactly
    * represented in SANE's fixed precision scheme, then
    * {@code SaneWord.forFixedPrecision(someValue).fixedPrecisionValue()} will not necessarily yield
    * {@code someValue}.
    */
  def forFixedPrecision(value: Double): SaneWord =
  SaneWord.forInt((value * precision).toInt)
}