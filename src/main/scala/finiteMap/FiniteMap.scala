package finiteMap

/** Map containing finite number of values */
trait FiniteMap[+K, +V, Map[+K, +T] <: FiniteMap[K, T, ?]]:
  type KConst[_] // Constraint for Key
  
  /** Add new key-value pair to Map */
  def bind[KK >: K: KConst, VV >: V](k: KK, v: VV): Map[KK, VV]  
  
  /** Find a value by key */
  def lookup[KK >: K: KConst](k: KK): V
