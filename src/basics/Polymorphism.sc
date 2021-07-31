trait List[T]{

  def isEmpty : Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  override def isEmpty = false
}

class Nil[T] extends List[T]{
  override def isEmpty = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: Nothing = {
    throw new NoSuchElementException("Nil.tail")
  }
}

def singleTone[T](ele : T) = new Cons[T](ele, new Nil[T])
singleTone[Int](2).head
singleTone[Boolean](true).head
singleTone[Boolean](true).tail

def nth[T](n: Int, list: List[T]): T =  {
    if(list.isEmpty) throw new IndexOutOfBoundsException
    if(n == 0) list.head
    else nth(n-1, list.tail)
}

val list = new Cons(1,new Cons(2, new Cons(3, new Nil[Int])))

nth(2, list)
nth(3, list)
nth(-1, list)

//******************************************
abstract class Nat{
  def isZero: Boolean
  def precedder: Nat
   def successor = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}
object Zero extends Nat {
  override def isZero = true

  override def precedder = throw new Error("zero.precedder")

  override def +(that: Nat) = that

  override def -(that: Nat) = if(that.isZero) this else throw new Error("zero.-")
}

class Succ(n: Nat) extends Nat{
  override def isZero = false

  override def precedder = new Succ(n)

  override def +(that: Nat) = new Succ(n + that)

  override def -(that: Nat) = if(that.isZero) n else (n - that.precedder)
}

// functions as objects
//scala numberic or boolean type can be represented as classes
// function type A => B is actaully class scala.Function1[A,B]
// functions are objects with apply method
// LIKE 2,3,4 so upto 22