package srv

trait Unsafe[F[_]] {
  def runSync[A](effect: F[A]): A
}

object Unsafe {
  def runSync[F[_], A](effect: F[A])(implicit unsafe: Unsafe[F]): A = unsafe.runSync(effect)
}