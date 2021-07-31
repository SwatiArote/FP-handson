package basics

object TailRecursionDemo extends App {

  // tail recusrion- call same or some other function  recusrivly

  // tail recursive
  def gcd(n1: Int, n2: Int): Int =
    if(n2 == 0) n1 else gcd(n2, n1%n2)

  // not tail recurisve as at end it as n*fnc
  def factorial(n: Int): Int =
    if(n == 0) 1 else  n * factorial(n-1)

  // tail recusrive version
  def factorial2(value: Int): Int ={
     def loop(acc: Int, value: Int):Int = {
       if(value == 0) acc
       else loop(acc * value, value-1)
     }
    loop(1, value)
  }



  println(factorial(4))
  println(factorial2(4))
  println(gcd(14,21))


}
