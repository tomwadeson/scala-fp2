package fp.typeclasses

import simulacrum.{op, typeclass}

@typeclass trait Monoid[A] {

  def zero: A

  @op("<>")
  def op(a1: A, a2: A): A
}
