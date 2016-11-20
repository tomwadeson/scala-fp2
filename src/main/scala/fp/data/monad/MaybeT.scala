package fp.data.monad

import fp.data.Maybe
import fp.data.Maybe.{Just, Nothing}
import fp.typeclasses.Monad
import fp.typeclasses.Monad.ops._

final case class MaybeT[M[_], A](runMaybeT: M[Maybe[A]])(implicit M: Monad[M]) {

  def flatMap[B](f: A => MaybeT[M, B]) = MaybeT[M, B] {
    M.flatMap(runMaybeT) {
      case Just(x) => f(x).runMaybeT
      case Nothing => M.pure(Nothing)
    }
  }

  def map[B](f: A => B): MaybeT[M, B] = MaybeT[M, B] {
    M.map(runMaybeT) {
      _.map(f)
    }
  }
}

