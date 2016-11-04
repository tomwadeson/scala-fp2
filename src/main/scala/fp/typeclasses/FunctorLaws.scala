package fp.typeclasses

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._
import Functor.ops._

object FunctorLaws {

  def apply[F[_], A, B, C](gen: Gen[F[A]])
                          (f: A => B, g: B => C)
                          (implicit F: Functor[F]): Prop =

    identityProperty(gen) && compositionProperty(gen)(f, g)

  private def compositionProperty[F[_], A, B, C](gen: Gen[F[A]])
                                                (f: (A) => B, g: (B) => C)
                                                (implicit F: Functor[F]): Prop =
    forAll(gen) { v =>
      (v map (g compose f)) == ((v map f) map g)
    }

  private def identityProperty[F[_], A, B, C](gen: Gen[F[A]])
                                             (implicit F: Functor[F]): Prop =
    forAll(gen) { v =>
      (v map identity) == identity(v)
    }
}
