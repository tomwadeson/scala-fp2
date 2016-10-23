package fp.data

import fp.typeclasses.Monoid

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
}
