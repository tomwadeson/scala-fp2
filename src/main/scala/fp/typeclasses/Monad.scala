package fp.typeclasses

import simulacrum.{op, typeclass}

import scala.concurrent.Future

@typeclass trait Monad[M[_]] extends Applicative[M] {

  @op(">>=", alias = true)
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  override def ap[A, B](ff: M[A => B])(fa: M[A]): M[B] =
    flatMap(ff)(map(fa))

  override def map[A, B](fa: M[A])(f: A => B): M[B] =
    flatMap(fa)(a => pure(f(a)))
}

object Monad {

  implicit val FutureMonad = new Monad[Future] {
    override def flatMap[A, B](ma: Future[A])(f: (A) => Future[B]): Future[B] = {
      import scala.concurrent.ExecutionContext.Implicits.global
      ma.flatMap(f)
    }

    override def pure[A](a: A): Future[A] = {
      Future.successful(a)
    }
  }
}
