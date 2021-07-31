package basics

object forExpression extends App {

  case class Person(name: String, age: Int)

  val persons = List(Person("a",33), Person("bb", 45), Person("cc", 40))

  persons.filter(x => x.age >=40).map(x => x.name)
  //equivalent to for expression
  for (p <- persons if(p.age>=40) ) yield p.name

  def scalarProcut(l1 :List[Int], l2: List[Int])={
   val result = for{
      (x,y) <- l1.zip(l2)
    }yield (x * y)
    result.sum
  }
}
