/*
 * Copyright 2014 matthias.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package au.com.southsky.jfreesane

import java.nio.CharBuffer
import java.nio.charset.Charset
import java.security.{MessageDigest, NoSuchAlgorithmException}

object SanePasswordEncoder {
  private[jfreesane] val iso8859_1: Charset = Charset.forName("ISO-8859-1")

  def encodedLatin1(charArray: Array[Char]): Array[Byte] =
    iso8859_1.encode(CharBuffer.wrap(charArray)).array

  private def encodeAsHex(input: Array[Byte]): String =
    input.indices.map { i =>
      val hex = Integer.toHexString(0xff & input(i))
      if (hex.length == 1) s"0$hex" else hex
    }.mkString

  def derivePassword(salt: String, password: String): String =
    try {
      val md: MessageDigest = MessageDigest.getInstance("MD5")
      md.update(iso8859_1.encode(salt))
      md.update(iso8859_1.encode(CharBuffer.wrap(password)))
      encodeAsHex(md.digest)
    } catch {
      case ex: NoSuchAlgorithmException =>
        // This is not expected, so convert to RuntimeException
        throw new RuntimeException(ex)
    }
}