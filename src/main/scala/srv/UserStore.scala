package srv

trait UserStore[F[_]] extends SimpleStateStore[F, User, String] {

}
