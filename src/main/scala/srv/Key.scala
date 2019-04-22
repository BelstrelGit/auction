package srv

import java.util.UUID

trait Key[T] {
  def key(el: T): UUID

  def notFound(id: UUID): Throwable

  def multipleKey(id: UUID): Throwable
}

object Key {
  def key[T](el: T)(implicit k: Key[T]): UUID = k.key(el)

  def notFound[T](id: UUID)(implicit k: Key[T]): Throwable = k.notFound(id)

  def multipleKey[T](id: UUID)(implicit k: Key[T]): Throwable = k.multipleKey(id)
}