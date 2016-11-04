package fp.typeclasses

import Applicative.ops._
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._

object ApplicativeLaws {

  def apply[F[_], A, B, C](gen: Gen[A])
                          (fab: A => B, fbc: B => C)
                          (implicit F: Applicative[F]): Prop =

    all(
      functorRelationProperty(gen)(fab),
      identityProperty(gen),
      homomorphismProperty(gen)(fab),
      interchangeProperty(gen)(fab),
      compositionProperty(gen)(fab, fbc)
    )

  def functorRelationProperty[F[_], A, B](gen: Gen[A])
                                         (f: A => B)
                                         (implicit F: Applicative[F]): Prop =
    forAll(gen) { v =>
      (F.pure(f) <*> F.pure(v)) == (F.pure(v) map f)
    }

  def identityProperty[F[_], A](gen: Gen[A])
                               (implicit F: Applicative[F]): Prop =
    forAll(gen) { v =>
      (F.pure((a: A) => a) <*> F.pure(v)) == F.pure(v)
    }

  def homomorphismProperty[F[_], A, B](gen: Gen[A])
                                      (f: A => B)
                                      (implicit F: Applicative[F]): Prop =
    forAll(gen) { v =>
      (F.pure(f) <*> F.pure(v)) == F.pure(f(v))
    }

  def interchangeProperty[F[_], A, B](gen: Gen[A])
                                     (f: A => B)
                                     (implicit F: Applicative[F]): Prop =
    forAll(gen) { v =>
      (F.pure(f) <*> F.pure(v)) == (F.pure((g: A => B) => g(v)) <*> F.pure(f))
    }

  def compositionProperty[F[_], A, B, C](gen: Gen[A])
                                        (fab: A => B, fbc: B => C)
                                        (implicit F: Applicative[F]): Prop = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose

    forAll(gen) { v =>
      val fa = F.pure(v)
      (F.pure(compose) <*> F.pure(fbc) <*> F.pure(fab) <*> (fa)) == (F.pure(fbc) <*> (F.pure(fab) <*> (fa)))
    }
  }
}
