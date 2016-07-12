package au.com.southsky.jfreesane.saneutil

/**
  * Created by pierr on 12.07.2016.
  */
class WriteOnce[T] {
  private var _value: Option[T] = None

  def set(value: T) = if (_value.isEmpty || _value.get == value)
    _value = Some(value)
  else
    throw new IllegalArgumentException("Cannot overwrite with a different value")

  def apply(): T = _value.getOrElse(Default[T])

  object Default {
    def apply[A]: A = {
      class Default {
        var value: A = _
      }
      (new Default).value
    }
  }

}
