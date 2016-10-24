package fp.typeclasses

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop._

object FunctorLaws {

  def apply[F[_], A, B, C](gen: Gen[F[A]])(f: B => C, g: A => B)(implicit functor: Functor[F]): Prop = {
    // Identity
    forAll(gen) { p => functor.map(p)(identity) == identity(p) } &&
    // Composition
    forAll(gen) { p =>
      functor.map(p)(f compose g) == functor.map(functor.map(p)(g))(f)
    }
  }
}
