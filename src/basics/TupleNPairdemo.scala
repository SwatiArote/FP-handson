package basics

object TupleNPairdemo {

  def mSort(list: List[Int]): List[Int] = {
    val n = list.length /2
    if(n == 0 ) list
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil,ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) =>
          if(x < y) x :: merge(xs1, ys)
          else y:: merge(ys1, xs)
      }
      val (xs, ys) = list.splitAt(n)
      merge(mSort(xs), mSort(ys))
    }
  }

  // Implementaion for geric
  def mergeSort[T](list: List[T], compareFunction: (T,T) => Boolean): List[T] ={
    val n = list.length /2
    if(n == 0 ) list
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil,ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) =>
          if(compareFunction(x,y)) x :: merge(xs1, ys)
          else y:: merge(ys1, xs)
      }
      val (xs, ys) = list.splitAt(n)
      merge(mergeSort(xs,compareFunction), mergeSort(ys,compareFunction))
    }
  }

  // Implementaion for geric with ordering
  def mergeSortWithOrdering[T](list: List[T], ord: Ordering[T]): List[T] ={
    val n = list.length /2
    if(n == 0 ) list
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil,ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) =>
          if(ord.lt(x,y)) x :: merge(xs1, ys)
          else y:: merge(ys1, xs)
      }
      val (xs, ys) = list.splitAt(n)
      merge(mergeSortWithOrdering(xs,ord), mergeSortWithOrdering(ys,ord))
    }
  }

  // with implicit
   def mergeSortImplicit[T](list: List[T]) (implicit ord: Ordering[T]): List[T] ={
    val n = list.length /2
    if(n == 0 ) list
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil,ys1) => ys1
        case (xs1, Nil) => xs1
        case (x::xs1, y::ys1) =>
          if(ord.lt(x,y)) x :: merge(xs1, ys)
          else y:: merge(ys1, xs)
      }
      val (xs, ys) = list.splitAt(n)
      merge(mergeSortImplicit(xs), mergeSortImplicit(ys))
    }
  }

}

object sortDemo extends App{
  println(TupleNPairdemo.mSort(List(56,-2,4,0,1,22)))
  val intCompare = (x: Int, y: Int) => x < y
  println(TupleNPairdemo.mergeSort(List(56,-2,4,0,1,22),intCompare))
  println(TupleNPairdemo.mergeSortWithOrdering(List(56,-2,4,0,1,22),Ordering.Int))
  println(TupleNPairdemo.mergeSortImplicit(List(56,-2,4,0,1,22)))

  println(TupleNPairdemo.mergeSort(List("apple","banana","orange"), (x : String, y: String) => x.compareTo(y) < 0))
  println(TupleNPairdemo.mergeSortWithOrdering(List("apple","banana","orange"), Ordering.String))
  println(TupleNPairdemo.mergeSortImplicit(List("apple","banana","orange")))

}