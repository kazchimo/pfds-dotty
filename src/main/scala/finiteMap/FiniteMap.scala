package finiteMap

/** Map containing finite number of values */
trait FiniteMap[+K, +V]:
  type Map[+K, +T] <: FiniteMap[K, T]
  type KConst[T] // Constraint for Key
  type VConst[T] // Constraint for Value

  /** Add new key-value pair to Map */
  def bind[KK >: K: KConst, VV >: V: VConst](k: KK, v: VV): Map[KK, VV]

  /** Find a value by key */
  def lookup[KK >: K: KConst](k: KK): V
