package au.com.southsky.jfreesane

import com.google.common.truth.{DefaultSubject, Subject, Truth}

object Truth2 {
  def assertThat(bool: Boolean) = Truth.assertThat(java.lang.Boolean.valueOf(bool))

  def assertThat(int: Int) = Truth.assertThat(java.lang.Integer.valueOf(int))

  def assertThat(any: Any): Subject[DefaultSubject, AnyRef] = Truth.assertThat(any)
}
