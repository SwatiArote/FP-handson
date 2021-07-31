package basics

import basics.FixedPoinOfFunctionDemo.squet3

object CurryingDemo extends App {


  def sum3(f: Int => Int) (a: Int, b: Int): Int = {
      if(a  >  b) 0
      else f(a) + sum3(f) (a+1, b)
  }

  // Type of function here is  (Int => Int) : (Int,Int) => Int

  val cube = (x:Int) => x * x * x
  val product = (x:Int) => x * x
  val sameInt = (x:Int) => x

  println(sum3(cube)(2,3))
  println(sum3(sameInt)(2,3))

  def product(f: (Int) => Int) (a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * product(f) (a+1, b)
  }

  def fact(n : Int) =  product(Int => Int)(1, n)

  println(product(product)(3,4))
  println(fact(4))

  def mapReduce(f: Int => Int , combine : (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if(a> b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  def productUsingMapReduce(f: Int => Int)(a: Int, b: Int) : Int = mapReduce(f, (x,y) => x  * y , 1)(a, b)

  println(productUsingMapReduce(x => x * x)(3,4))
  println(FixedPoinOfFunctionDemo.squet3(2))
}

object FixedPoinOfFunctionDemo{

  def isCloseEnough(x: Double, y: Double): Boolean = Math.abs(((x -y )/ x)) / x < 0.001

  def fixedPoint(f: Double => Double)(fiestGuess: Double): Double ={
    def interate(guess: Double): Double ={
      val next = f(guess)
      if(isCloseEnough(guess, next) ) next
      else interate(next)
    }
    interate(1.0)
  }

  // sqaure root expression :
  // y = x * x
  // x = y / x
  def squet3(x: Double): Double = fixedPoint(y => (y + x /y) / 2)(1.0)


}