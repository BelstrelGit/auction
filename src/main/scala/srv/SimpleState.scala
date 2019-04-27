package srv

trait SimpleState[T, K] {

  def all: List[T]

  def add(elem: T): Either[Throwable, SimpleState[T, K]]

  def remove(elem: T): Either[Throwable, SimpleState[T, K]]

  def update(elem: T): SimpleState[T, K]

  def getById(id: K): Option[T]
}

object SimpleState {

  def create[T, K](elems: List[T])(implicit key: Key[T, K]): Either[Throwable, SimpleState[T, K]] =
    makeStateMap(elems).map(MapSimpleState(_))

  private def makeStateMap[T, K](elems: List[T])(implicit key: Key[T, K]): Either[Throwable, Map[K, T]] = {
    val grouped = elems.groupBy(Key.key[T, K]).toList
    val multiple = grouped.collect { case (id, _ :: _ :: Nil) => id }

    if (multiple.isEmpty)
      Right(grouped.map { case (id, list) => id -> list.head }.toMap)
    else
      Left(Key.multipleKey[T, K](multiple.head)) // TODO change error definition to varargs
  }

  private case class MapSimpleState[T, K](state: Map[K, T])(implicit key: Key[T, K]) extends SimpleState[T, K] {

    def all: List[T] = state.values.toList

    def add(elem: T): Either[Throwable, SimpleState[T, K]] = {
      val id = Key.key(elem)
      getById(id) match {
        case None => Right(copy(state + (id -> elem)))
        case Some(_) => Left(Key.multipleKey(id))
      }
    }

    def remove(elem: T): Either[Throwable, SimpleState[T, K]] = {
      val id = Key.key(elem)
      getById(id) match {
        case Some(_) => Right(copy(state - id))
        case None => Left(Key.notFound(id))
      }
    }

    def update(elem: T): SimpleState[T, K] = copy(state + (Key.key(elem) -> elem))

    def getById(id: K): Option[T] = state.get(id)
  }

}


