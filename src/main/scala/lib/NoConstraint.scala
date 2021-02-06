package lib

/** Represents no constraints exist for value of `T` */
trait NoConstraint[+T] 

object NoConstraint extends NoConstraint[Nothing] {
  given [T]: NoConstraint[T] = this
}
