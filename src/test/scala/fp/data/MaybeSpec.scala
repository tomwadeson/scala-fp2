package fp.data

import fp.BaseSpec
import fp.data.Maybe.{Just, Nothing}
import fp.typeclasses.{ApplicativeLaws, FunctorLaws, MonadLaws, MonoidLaws}
import org.scalacheck.Gen
import org.scalatest.prop.Checkers._

class MaybeSpec extends BaseSpec {

  import MaybeGenerators._

  val AbsentMaybeInt: Maybe[Int] = Nothing

  "Present Maybe values (Just)" should "be defined and not absent" in {
    forAll(PresentMaybeIntGen) { p =>
      p.isDefined && !p.isAbsent
    }
  }

  it should "wrap an inner value" in {
    forAll(PresentMaybeIntGen) { case p@Just(inner) =>
      p.unsafeGet == inner
    }
  }

  it should "always return its inner value in favour of a supplied default" in {
    forAll(PresentMaybeIntGen, Gen.posNum[Int]) { case (p@Just(inner), randomDefault) =>
      p.getOrDefault(randomDefault) == inner
    }
  }

  it should "always return itself in favour of another Maybe value" in {
    forAll(PresentMaybeIntGen, PresentMaybeIntGen) { case (p, z) =>
      p.orElse(z) == p
    }
  }

  "Absent Maybe values (Nothing)" should "be absent and not defined" in {
    AbsentMaybeInt.isAbsent should be(true)
    AbsentMaybeInt.isDefined should be(false)
  }

  it should "throw an exception upon invocation of `unsafeGet`" in {
    an[UnsupportedOperationException] should be thrownBy AbsentMaybeInt.unsafeGet
  }

  it should "return a default" in {
    forAll(Gen.posNum[Int]) { x =>
      AbsentMaybeInt.getOrDefault(x) should be(x)
    }
  }

  it should "defer to the alternative upon invocation of `getOrElse`" in {
    forAll(PresentMaybeIntGen) { p =>
      AbsentMaybeInt.orElse(p) should be(p)
    }
  }

  "Maybe" should "form a Monoid" in {
    check(MonoidLaws(MaybeIntGen))
  }

  it should "form a Functor" in {
    val f: Int => Int = _ + 1
    val g: Int => Int = _ * 2
    check(FunctorLaws(MaybeIntGen)(f, g))
  }

  it should "form an Applicative" in {
    val f: Int => Int = _ + 1
    val g: Int => Int = _ * 2
    check(ApplicativeLaws[Maybe, Int, Int, Int](Gen.posNum[Int])(f, g))
  }

  it should "form a Monad" in {
    val f: Int => Maybe[Int] = x => Just(x*2)
    val g: Int => Maybe[Int] = x => Just(x+1)
    check(MonadLaws(Gen.posNum[Int], f, g))
  }
}

object MaybeGenerators {
  val PresentMaybeIntGen = presentMaybeGen(Gen.posNum[Int])

  val MaybeIntGen = maybeGen(Gen.posNum[Int])

  def presentMaybeGen[A](implicit inner: Gen[A]): Gen[Maybe[A]] =
    inner.flatMap(Just(_))

  def maybeGen[A](implicit inner: Gen[A]): Gen[Maybe[A]] =
    Gen.frequency(
      (9, presentMaybeGen),
      (1, Nothing)
    )
}
