package srv

trait Extractor[F[_], T] {
  def get(implicit ds: DataSource[F]): F[List[T]]
}