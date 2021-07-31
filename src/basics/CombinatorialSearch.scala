package basics

object CombinatorialSearch extends App {

    //sequcne of pair whose sum is prime number
    def isPrime(n: Int): Boolean ={
      (2 until n ).forall(x => n % x !=0)
    }

    def seqOfPrimeAdd(n: Int) ={
       (1 until n).map(i => (1 until i).map(j => (i,j))).flatten.filter(pair => isPrime(pair._1+pair._2))
    }

    //equiavlent
    def sequnceOfPrime(n: Int): Unit ={
      for {
        i <- 1 until n
        j <- 1 until i
        if(isPrime(i+j))
      } yield (i,j)
    }

    println(seqOfPrimeAdd(7))

}
