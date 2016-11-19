package fp.typeclasses

import simulacrum.{op, typeclass}

@typeclass trait Monad[M[_]] extends Applicative[M] {

  @op(">>=")
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  override def ap[A, B](ff: M[A => B])(fa: M[A]): M[B] =
    flatMap(ff)(map(fa))

  override def map[A, B](fa: M[A])(f: A => B): M[B] =
    flatMap(fa)(a => pure(f(a)))
}
