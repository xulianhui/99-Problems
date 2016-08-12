class PInt(n:Int) {
  //31
  def isPrime:Boolean = {
    Range(2, Math.sqrt(n).toInt + 1) map { md => 
      if (n % md == 0) return false
    }
    true
  }
  //32
  def gcd(a:Int, b:Int):Int = if(b == 0) a else gcd(b, a % b)
  //33 
  def isCoprimeTo(v:Int):Boolean = {
    gcd(n, v) == 1
  }
  //34
  def totient:Int = (1 to n) filter (n isCoprimeTo(_)) length
  //35
  def primeFactors:List[Int] = {
    List.range(2, n) filter (_ isPrime) flatMap {
      case x if (n % x == 0) => List(x)/*.fill(n / x)*/
      case _ => Nil   
    }
  }
  //36
  def getC (n:Int, x:Int):Int = {
    n match {
      case c if (c % x == 0) => 1 + getC(n / x, x)
      case _ => 0
    }
  }
  def primeFactorMultiplicity:List[(Int, Int)]= { // Map[Int,Int] 
    this.primeFactors flatMap { x:Int => 
      List(Tuple2(x, getC(n, x)))
    } // toMap
  }
  //37
  def phi:Int = {
    this.primeFactorMultiplicity.foldLeft(1) {
      (r, e) => e match {
        case (p, m) => r * (p - 1) * Math.pow(p, m-1).toInt
      }  
    }
  }
  //38
  def comp:Unit = {
    val a = System.currentTimeMillis
    n.totient
    println("totient:" + (System.currentTimeMillis - a) + " ms.")

    val b = System.currentTimeMillis
    n.phi
    println("phi    :" + (System.currentTimeMillis - b) + "ms.")
  }
  //39
  def listPrimesRange(r:Int):List[Int] = {
    List.range(2, r) filter (_ isPrime) dropWhile (_ < n)
  }
  //40
  def goldbach :(Int, Int) = {
    val c = List.range(2, n).filter(_ isPrime).find (p => (n - p).isPrime)
    // println(n)
    // println(c) 
    c match {
      case None => {
        throw new IllegalArgumentException
      }
      case Some(p) => (p, n - p)
    }
  }
}
object PInt {
  //41 
  def printGoldbachList(r:Range, t: Int = 1):Unit = {
    r filter {c => 
      if (c % 2 == 1 || c <= 2) false 
      else {
        val an = c.goldbach
        if (an._1 < t) false
        else {
          println(an)
          true
        }
      }
    }
  }
}

implicit def covInt2PInt(n:Int):PInt = new PInt(n)

PInt printGoldbachList(1 to 2000, 50)