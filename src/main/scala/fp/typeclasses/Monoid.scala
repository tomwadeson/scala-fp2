package fp.typeclasses

import simulacrum.typeclass

@typeclass trait Monoid[A] {

  def zero: A

  def op(a1: A, a2: A): A
}
