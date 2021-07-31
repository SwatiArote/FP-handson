package basics

object highrerOrderListFucntionsDemo extends App {

  def scaleList(list: List[Int]): List[Int] = list match {
    case Nil => list
    case x :: xs =>  x * x :: scaleList(xs)
  }

  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case x :: xs =>  x + sum(xs)
  }

  def scaleListsWithMap(list: List[Int]): List[Int] = {
      list.map(x => x*x)
  }

  def selectPos(list: List[Int]): List[Int] = list match {
    case Nil => list
    case x :: xs =>  if(x>0) x::selectPos(xs) else  selectPos(xs)
  }

  def selectPosWithFilter(list: List[Int]): List[Int] = {
    list.filter(_ >  0)
  }

  def pack[T](list: List[T]): List[List[T]] = list match {
    case Nil => Nil
    case x::xs => list.takeWhile(_==x) :: pack(xs.dropWhile(_ == x)) // here can also use span
  }

  def encode[T](list: List[T]): List[(T,Int)] = {
      pack(list).map(x => (x.head, x.length))
  }

  def concat[T](l1 : List[T], l2: List[T]): List[T] ={
    l1.foldRight(l2)(_::_)
    (  l1 foldRight   l2) (_::_)
 //   (  l1 foldLeft    l2) (_::_) it will give error as :: function not availabe for type T as it will be l1.head::l2
  }



  println(scaleList(List(1,2,3)))
  println(scaleList(List()))
  println(scaleListsWithMap(List(1,2,3)))
  println(selectPos(List(-1,2,-3,4,-5)))
  println(List(-1,2,-3,4,-5).filterNot(_ > 0))
  println(List(-1,2,-3,4,-5).partition(_ > 0)) // filter n filternot combination
  println(List(1,2,-3,4,-5).takeWhile(_ > 0)) // logest prifix of list
  println(List(1,2,-3,4,-5).dropWhile(_ > 0)) // logest prifix of list
  println(List(1,2,-3,4,-5).span(_ > 0)) // rake while n drop while comb
  println(pack(List(1,1,2,3,3,3,4,4,5)))
  println(encode(List(1,1,2,3,3,3,4,4,5)))
  println(encode(List("a","a","b","c","c","c","d","d","e")))
  println(sum(List(1,1,2,3,3,3,4,4,5)))
  println(List(1,1,2,3,3,3,4,4,5).reduceLeft((x, y) => x + y))
  println(List(1,1,2,3,3,3,4,4,5).reduceLeft(_*_))
  println(List(1,1,2,3,3,3,4,4,5).reduceLeft(_*_))
//  println(List().reduceLeft(_*_)) not possible
  //println(List().foldLeft(1)(_*_))
  println(concat(List("a","a","b","c"),List("c","c","d","d","e")))

  // reduceleft can only applied to nonempty list
  // foldfleft can only be applied to nonempty list
}
