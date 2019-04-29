package srv

trait Extractor[F[_], T] {
  def get(implicit ds: DataSource[F]): F[List[T]]
}

object Extractor{
  def get[F[_], T](implicit extractor: Extractor[F,T], ds: DataSource[F]): F[List[T]] = extractor.get
}