package basics

object PolynomialWithMaps extends App {

  class Poly(val terms0: Map[Int, Double]){
    val terms = terms0.withDefaultValue(0.0)
    def +(other: Poly) = new Poly(terms ++ other.terms map adjsut)

    def adjsut(term: (Int, Double)) : (Int, Double) ={
      val (exp, coff) = term
      terms.get(exp) match {
        case Some(coff1) => exp -> (coff + coff1)
        case None =>   exp -> coff
      }

    }
    override def toString: String = {
      terms.toList.sorted.reverse.map(x=> x._2+"^X"+x._1).mkString("+")
    }
  }

  val p1 = new Poly(Map(1 -> 1.2, 2-> 1.2))
  val p2 = new Poly(Map(1 -> 2.2, 2-> 5.2))
  println(p1)
  println(p2)
  println(p2.terms(3))
  println(p1+(p2))
}
