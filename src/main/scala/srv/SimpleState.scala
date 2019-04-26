package srv

import java.util.UUID

trait SimpleState[T] {

  def all: List[T]

  def add(elem: T): Either[Throwable, SimpleState[T]]

  def remove(elem: T): Either[Throwable, SimpleState[T]]

  def update(elem: T): SimpleState[T]

  def getById(id: UUID): Option[T]
}

object SimpleState {

  def create[T: Key](elems: List[T]): Either[Throwable, SimpleState[T]] = makeStateMap(elems).map(MapSimpleState(_))

  private def makeStateMap[T: Key](elems: List[T]): Either[Throwable, Map[UUID, T]] = {
    val grouped = elems.groupBy(Key.key[T]).toList
    val multiple = grouped.collect { case (id, _ :: _ :: Nil) => id }

    if (multiple.isEmpty)
      Right(grouped.map { case (id, list) => id -> list.head }.toMap)
    else
      Left(Key.multipleKey[T](multiple.head)) // TODO change error definition to varargs
  }

  private case class MapSimpleState[T: Key](state: Map[UUID, T]) extends SimpleState[T] {

    def all: List[T] = state.values.toList

    def add(elem: T): Either[Throwable, SimpleState[T]] = {
      val id = Key.key(elem)
      getById(id) match {
        case None => Right(copy(state + (id -> elem)))
        case Some(_) => Left(Key.multipleKey(id))
      }
    }

    def remove(elem: T): Either[Throwable, SimpleState[T]] = {
      val id = Key.key(elem)
      getById(id) match {
        case Some(_) => Right(copy(state - id))
        case None => Left(Key.notFound(id))
      }
    }

    def update(elem: T): SimpleState[T] = copy(state + (Key.key(elem) -> elem))

    def getById(id: UUID): Option[T] = state.get(id)
  }

}


