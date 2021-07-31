abstract class IntSet{
  def incl(x: Int):IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}
class NonEmpty(ele: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int) =
    if(x < ele) new NonEmpty(x, left incl(x), right)
    else if(x > ele) new NonEmpty(x, left, right.incl(x))
    else this

  override def contains(x: Int) =
    if(x == ele) true
    else if(x < ele) left.contains(x)
    else  right.contains(x)

  override def toString: String = "{" + left + ele + right + "}"

  override def union(other: IntSet) =
    ((left union right) union  other) incl ele
}


class Empty extends IntSet{
  override def incl(x: Int) = new NonEmpty(x,new Empty, new Empty)
  override def contains(x: Int) = false

  override def toString: String = "."

  override def union(other: IntSet) = other
}

val mm = new NonEmpty(3, new Empty, new Empty)
val t = mm incl(4)
t.toString

val em = new Empty
val em2 = em incl(6)
em2.toString
