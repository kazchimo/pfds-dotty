package finiteMap

enum TreeMap[+K, +V] extends FiniteMap[K, V, TreeMap] {
  case Leaf
  case Node(left: TreeMap[K, V], key: K, value: V, right: TreeMap[K, V])
  
  type KConst[T] = Ordering[T]

  /** Add new key-value pair to Map */
  override def bind[KK >: K, VV >: V](k: KK, v: VV)(using kord: Ordering[KK]): TreeMap[KK, VV] =
    this match
      case Leaf => Node(Leaf, k, v, Leaf)
      case Node(l, key, value, r) => 
        if kord.lt(k, key) then Node(l.bind(k, v), key, value, r)
        else if kord.lt(key, k) then Node(l, key, value, r.bind(k, v))
        else this

  /** Find a value by key */
  override def lookup[KK >: K](k: KK)(using kord: Ordering[KK]): V = this match
    case Leaf => throw Exception("Empty Map")
    case Node(l, key, value, r) => 
      if kord.lt(k, key) then l.lookup(k)
      else if kord.lt(key, k) then r.lookup(k)
      else value
}
