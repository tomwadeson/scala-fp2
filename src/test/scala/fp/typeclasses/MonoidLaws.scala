package fp.typeclasses

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}

object MonoidLaws {

  def apply[A](gen: Gen[A])(implicit monoid: Monoid[A]): Prop =
    associativityProperty(gen) && identityProperty(gen)

  private def identityProperty[A](gen: Gen[A])(implicit monoid: Monoid[A]): Prop =
    forAll(gen)((a: A) =>
      monoid.op(a, monoid.zero) == a && monoid.op(monoid.zero, a) == a)

  private def associativityProperty[A](gen: Gen[A])(implicit monoid: Monoid[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      monoid.op(p._1, monoid.op(p._2, p._3)) == monoid.op(monoid.op(p._1, p._2), p._3))
}
