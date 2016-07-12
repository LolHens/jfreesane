package au.com.southsky.jfreesane

import java.io.{ByteArrayInputStream, IOException, InputStream}

import com.google.common.truth.Truth
import org.junit.Assert.assertEquals
import org.junit.{Assert, Test}

/**
  * This class implements tests for {@link SaneWord}.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneWordTest {
  @Test def testFixedPrecisionValue =
    assertEquals(216.069, SaneWord.forFixedPrecision(216.069).fixedPrecisionValue, 0.0001)

  @Test def fromArrayWithOffset = {
    val array: Array[Byte] = Array[Byte](0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4)
    assertEquals(0, SaneWord.fromBytes(array, 0).intValue)
    assertEquals(1, SaneWord.fromBytes(array, 4).intValue)
    assertEquals(2, SaneWord.fromBytes(array, 8).intValue)
    assertEquals(3, SaneWord.fromBytes(array, 12).intValue)
    assertEquals(4, SaneWord.fromBytes(array, 16).intValue)
  }

  @Test
  @throws[Exception]
  def fromStream = {
    val array: Array[Byte] = Array[Byte](0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4)
    val stream: InputStream = new ByteArrayInputStream(array)
    Truth.assertThat(Integer.valueOf(SaneWord.fromStream(stream).intValue)).comparesEqualTo(0)
    Truth.assertThat(Integer.valueOf(SaneWord.fromStream(stream).intValue)).comparesEqualTo(1)
    Truth.assertThat(Integer.valueOf(SaneWord.fromStream(stream).intValue)).comparesEqualTo(2)
    Truth.assertThat(Integer.valueOf(SaneWord.fromStream(stream).intValue)).comparesEqualTo(3)
    Truth.assertThat(Integer.valueOf(SaneWord.fromStream(stream).intValue)).comparesEqualTo(4)

    try {
      SaneWord.fromStream(stream)
      Assert.fail("fromStream should have thrown IOException but didn't")
    } catch {
      case expected: IOException =>
      // Expected this exception.
    }
  }
}
