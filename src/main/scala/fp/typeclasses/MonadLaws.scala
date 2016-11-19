package fp.typeclasses

import Monad.ops._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._

object MonadLaws {

  def apply[F[_], A, B, C](gen: Gen[A], f: A => F[B], g: B => F[C])
                       (implicit F: Monad[F]): Prop =
    all(
      leftIdentityProperty(gen, f),
      rightIdentityProperty(gen),
      associativityProperty(gen, f, g)
    )

  def leftIdentityProperty[F[_], A, B](gen: Gen[A], f: A => F[B])
                                      (implicit F: Monad[F]): Prop =
    forAll(gen) { v =>
      (F.pure(v) >>= f) == f(v)
    }

  def rightIdentityProperty[F[_], A, B](gen: Gen[A])
                                      (implicit F: Monad[F]): Prop =
    forAll(gen) { v =>
      (F.pure(v) >>= F.pure) == F.pure(v)
    }

  def associativityProperty[F[_], A, B, C](gen: Gen[A], f: A => F[B], g: B => F[C])
                                      (implicit F: Monad[F]): Prop =
    forAll(gen) { v =>
      (F.pure(v) >>= (x => f(x) >>= g)) == (F.pure(v) >>= f >>= g)
    }
}
