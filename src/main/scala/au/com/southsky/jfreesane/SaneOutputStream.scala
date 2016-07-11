package au.com.southsky.jfreesane

import java.io.{IOException, OutputStream}

/**
  * This class wraps a {@link OutputStream} and provides a handful of utilities to serialize
  * SANE-related types to the underlying stream.
  * Creates a new {@code SaneOutputStream} that wraps the given stream.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneOutputStream(val wrappedStream: OutputStream) extends OutputStream {
  @throws[IOException]
  override def close() = wrappedStream.close()

  @throws[IOException]
  override def flush() = wrappedStream.flush()

  @throws[IOException]
  def write(b: Int) = wrappedStream.write(b)

  @throws[IOException]
  override def write(b: Array[Byte]) = wrappedStream.write(b)

  @throws[IOException]
  override def write(b: Array[Byte], off: Int, len: Int) = wrappedStream.write(b, off, len)

  /**
    * Writes the given string to the underlying stream in SANE string format. The format is:
    *
    * <ul>
    * <li>if the string is non-empty, a {@link SaneWord} representing the length of the string plus a
    * null terminator</li>
    * <li>if the string is non-empty, the bytes of the string (see {@link String#toCharArray})</li>
    * <li>unconditionally, a null terminator</li>
    * </ul>
    *
    * @param string
    * @throws IOException
    */
  @throws[IOException]
  def write(string: String): Unit = write(string.toCharArray)

  /**
    * Writes the given char[] to the underlying stream in SANE string format. The format is:
    *
    * <ul>
    * <li>if the char[] is non-empty, a {@link SaneWord} representing the length of the string plus a
    * null terminator</li>
    * <li>if the char[] is non-empty, the bytes of the char[]</li>
    * <li>unconditionally, a null terminator</li>
    * </ul>
    *
    * @param charArray character array to be written to the stream
    * @throws IOException
    */
  @throws[IOException]
  def write(charArray: Array[Char]): Unit = {
    if (charArray.length > 0) {
      val encoded: Array[Byte] = SanePasswordEncoder.encodedLatin1(charArray)
      write(SaneWord.forInt(encoded.length + 1))
      write(encoded)
    }
    write(0)
  }

  /**
    * Writes the bytes of the given {@link SaneWord} to the underlying stream. See
    * {@link SaneWord#getValue}.
    */
  @throws[IOException]
  def write(word: SaneWord): Unit = write(word.bytes)

  /**
    * Writes the wire value of the given {@link SaneEnum} to the underlying stream.
    */
  @throws[IOException]
  def write(someEnum: SaneEnum[_]): Unit = write(SaneWord.forInt(someEnum.wireValue))
}