class Rational(x: Int, y: Int){

  //require(y != 0 , "denom should be non=zero")
  private def gcd(x: Int, y: Int): Int = if(y ==0) x else gcd(y, x % y)
  private val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  // secound contrsuctor for class
  def this(x: Int) = this(x,1)

  override def toString = numer + "/" + denom

  def add(that: Rational) = {
    new Rational(that.denom * numer + that.numer * denom ,
      that.denom * denom)
  }

  def neg = new Rational(- numer, denom)
  // infix operator for negation/ inverse
  def unary_- : Rational = new Rational(- numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if(this.less(that)) that else this

}

  val r1 = new Rational(1,2)
  r1.numer
  r1.denom
  r1.toString
  val r2 = new Rational(1,2)
  val r3 = new Rational(1,0)
  r1.add(r2).toString
  r1.neg
  r1.sub(r2)
  r1.less(r2)
  r1
  -r1




