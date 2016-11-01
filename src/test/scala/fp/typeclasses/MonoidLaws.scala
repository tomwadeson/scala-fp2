package fp.typeclasses

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import Monoid.ops._

object MonoidLaws {

  def apply[A](gen: Gen[A])
              (implicit M: Monoid[A]): Prop =
    associativityProperty(gen) && identityProperty(gen)

  private def identityProperty[A](gen: Gen[A])
                                 (implicit M: Monoid[A]): Prop =
    forAll(gen) { v =>
      (v <> M.zero) == v && (M.zero <> v) == v
    }

  private def associativityProperty[A](gen: Gen[A])
                                      (implicit M: Monoid[A]): Prop =
    forAll(gen, gen, gen) { (x, y, z) =>
      (x <> (y <> z)) == ((x <> y) <> z)
    }
}
