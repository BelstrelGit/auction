package srv

trait Key[T, K] {
  def key(el: T): K

  def notFound(id: K): Throwable

  def multipleKey(id: K): Throwable
}

object Key {
  def key[T, K](el: T)(implicit k: Key[T, K]): K = k.key(el)

  def notFound[T, K](id: K)(implicit k: Key[T, K]): Throwable = k.notFound(id)

  def multipleKey[T, K](id: K)(implicit k: Key[T, K]): Throwable = k.multipleKey(id)
}