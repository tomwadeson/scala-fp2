package fp.data

import fp.typeclasses.{Applicative, Functor, Monad, Monoid}

sealed trait Maybe[+A] {
  def isDefined: Boolean

  def isAbsent: Boolean =
    !isDefined

  def getOrDefault[B >: A](default: B): B

  def orElse[B >: A](other: Maybe[B]): Maybe[B]

  def unsafeGet: A
}

object Maybe {

  final case class Just[+A](inner: A) extends Maybe[A] {
    override def isDefined: Boolean =
      true

    override def getOrDefault[B >: A](default: B): B =
      inner

    override def orElse[B >: A](other: Maybe[B]): Maybe[B] =
      this

    override def unsafeGet: A =
      inner
  }

  case object Nothing extends Maybe[Nothing] {
    override def isDefined: Boolean =
      false

    override def getOrDefault[B >: Nothing](default: B): B =
      default

    override def orElse[B >: Nothing](other: Maybe[B]): Maybe[B] =
      other

    override def unsafeGet: Nothing =
      throw new UnsupportedOperationException("`unsafeGet` is unsupported on `Nothing` values")
  }

  implicit def monoidInstance[A]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def zero: Maybe[A] =
      Nothing

    override def op(a1: Maybe[A], a2: Maybe[A]): Maybe[A] =
      a1 orElse a2
  }

  implicit val FunctorInstance = new Functor[Maybe] {
    override def map[A, B](fa: Maybe[A])(f: (A) => B): Maybe[B] = fa match {
      case Just(x) => Just(f(x))
      case Nothing => Nothing
    }
  }

  implicit val ApplicativeInstance = new Applicative[Maybe] {
    override def pure[A](a: A): Maybe[A] =
      Just(a)

    override def ap[A, B](ff: Maybe[(A) => B])(fa: Maybe[A]): Maybe[B] = (ff, fa) match {
      case (Just(f), Just(x)) => Just(f(x))
      case _ => Nothing
    }
  }

  implicit val MonadInstance = new Monad[Maybe] {
    override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
      ma match {
        case Just(x) => f(x)
        case _ => Nothing
      }

    override def pure[A](a: A): Maybe[A] =
      Just(a)
  }
}
