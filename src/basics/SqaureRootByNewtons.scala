package basics

object SqaureRootByNewtons extends App {

  def isGoodEnough(guess: Double, x: Double): Boolean = Math.abs(guess * guess - x) / x < 0.001

  def sqrroot(x: Double, guess: Double): Double =
    if(isGoodEnough(guess,x)) guess
    else sqrroot(x, improve(guess,x))

  def improve(guess: Double, x: Double) : Double = ( guess + x / guess) / 2

  def sqrt(x: Double) = sqrroot(x, 1.0)

  println(sqrroot(2,1))
  println(sqrt(2))
  println(sqrt(1e-6)) // too much low num
  println(sqrt(1e60)) // too much high num

  // Using Block we:
  // x will be same in block
  def sqrt2(x: Double) = {
    def isGoodEnough(guess: Double): Boolean = Math.abs(guess * guess - x) / x < 0.001

    def sqrroot(guess: Double): Double =
      if(isGoodEnough(guess)) guess
      else sqrroot(improve(guess))

    def improve(guess: Double) : Double = ( guess + x / guess) / 2

    sqrroot(1)
  }

  println(sqrt2(4))

}
