package basics

object HigherOrderFunction extends App {


  def sum(a: Int, b: Int, f: Int => Int) : Int =
    f(a) + f(b)

  def sumCube(a: Int, b: Int) = sum(a, b, (x: Int) => x * x * x)
  def sumInt(a: Int, b: Int) = sum(a, b, (x: Int) => x )

  println(sumCube(2,3))
  println(sumInt(2,3))

  // Tail recusrion version

  def sum2(f: Int => Int) (a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if(a  >  b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)
  }

  println(sum2( (x: Int) => x)(2,3))
  println(sum2( (x: Int) => x * x * x)(2,3))

}
