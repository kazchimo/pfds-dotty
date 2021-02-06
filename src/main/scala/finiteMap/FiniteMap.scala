package finiteMap

/** Map containing finite number of values */
trait FiniteMap[Key, +T, Map[Key, +T] <: FiniteMap[Key, T, ?]]:
  /** Add new key-value pair to Map */
  def bind[S >: T](k: Key, v: S): Map[Key, S]  
  
  /** Find a value by key */
  def loolup(k: Key): T
