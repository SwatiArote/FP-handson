def insert(value: Int, list: List[Int]): List[Int] = list match {
  case List() =>  List(value)
  case x :: xs => if(value <= x) value :: xs else x :: insert(value, xs)
}

def isertionSort(list: List[Int]): List[Int]= list match {
  case List() => List()
  case x::xs =>  insert(x, isertionSort(xs))

}
//complexcity propational to N

val list = List(1,2,3,4)
list.length
list.last
list.init
list take(2)
list drop (2)
list(2)
list
val list2 = List(5,6,7,8)
list :: list2
list.reverse
list.updated(0,5)
list.contains(2)
list.indexOf(2)

//last element of list

def last[T](list: List[T]): T = list match {
  case List() => throw new Error("empty.last")
  case List(x) => x
  case x::xs => last(xs)
}
def init[T](list: List[T]): List[T] = list match {
  case List() => throw new Error("empty.last")
  case List(x) => List()
  case x::xs => x :: init(xs)
}

def concat[T](list1: List[T],list2: List[T]): List[T] = list1 match {
  case List() => list2
  case x::xs => x :: concat(xs , list2)
}

def reverse[T](list: List[T]): List[T] = list match {
  case List() => List()
  case x::xs =>  reverse(xs) ++ List(x)
}

def removeAt[T](list: List[T], n : Int): List[T]= {
  (list take n ) ::: (list drop n+1)
}

last(list)
init(list)
concat(list,list2)
list
reverse(list)
removeAt(list,2)