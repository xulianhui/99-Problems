/*//46
def and(a:Boolean, b:Boolean):Boolean = a & b
def or(a:Boolean, b:Boolean):Boolean = a | b
def not(a:Boolean):Boolean = !a

def table(f:(Boolean, Boolean) => Boolean):Unit = {
  printf("A\tB\tresult\n")
  Seq(true, false) map { a => 
    Seq(true, false) map { b => 
      printf(a + "\t" + b + "\t" + f(a, b) + "\n")
    }
  }
}
//47
class Logic(v:Boolean) {
  def and(e:Boolean):Boolean = v & e
  def or(e:Boolean):Boolean = v | e
  def not:Boolean = !v
}

object Logic {
  implicit def boolean2Logic(e:Boolean):Logic = new Logic(e)
}

val pq = collection.mutable.PriorityQueue(1, 2, 4, 6, 5, 3)()

//50
def huffman[T](ls:List[(T, Int)]):List[T, Int] = {
  val pq = collection.mutable.PriorityQueue[Tuple2[T, Int]]()(Ordering.by[(T, Int), Int](_._2))
  ls map { e =>
    pq += Tuple2(e._1, - e._2)
  }
  // List(("a", 1))
}

*/
class Logic(a:Boolean) {
  def and(b:Boolean) : Boolean = {
    (a, b) match {
      case (true, true) => true
      case _ => false
    }
  }

  def or(b:Boolean) : Boolean = {
    (a, b) match {
      case (false, false) => false
      case _ => true
    }
  }

  def xor(b:Boolean) : Boolean = {
    (a, b) match {
      case (true, false) => true
      case (false, true) => true
      case _ => false
    }
  }

  def equ(b:Boolean) : Boolean = {
    (a, b) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }
  }

  def nand(b:Boolean) : Boolean = {
    Logic.not(Logic.and(a, b))
  }

  def nor(b:Boolean) : Boolean = {
    Logic.not(Logic.or(a, b))
  }
  
  def impl(b:Boolean) : Boolean = {
    Logic.or(Logic.not(a), b)
  }
}

object Logic {
  //46
  def and(a:Boolean, b:Boolean) : Boolean = {
    (a, b) match {
      case (true, true) => true
      case _ => false
    }
  }

  def or(a:Boolean, b:Boolean) : Boolean = {
    (a, b) match {
      case (false, false) => false
      case _ => true
    }
  }

  def not(a:Boolean) : Boolean = {
    a match {
      case true => false
      case false => true
    }
  }

  def xor(a:Boolean, b:Boolean) : Boolean = {
    (a, b) match {
      case (true, false) => true
      case (false, true) => true
      case _ => false
    }
  }

  def equ(a:Boolean, b:Boolean) : Boolean = {
    (a, b) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }
  }

  def nand(a:Boolean, b:Boolean) : Boolean = {
    not(and(a, b))
  }

  def nor(a:Boolean, b:Boolean) : Boolean = {
    not(or(a, b))
  }
  
  def impl(a:Boolean, b:Boolean) : Boolean = {
    or(not(a), b)
  }
  //47
  def table2(f:(Boolean, Boolean) => Boolean):Unit = {
    println("a\tb\tf(a, b)")  
    Seq(true, false) map { a => 
      Seq(true, false) map { b => 
        println(s"${a}\t${b}\t${f(a, b)}")
      }
    }
  }
  //48
  def table3(f:(Boolean, Boolean, Boolean) => Boolean):Unit = {
    println("a\tb\tc\tf(a, b)")  
    Seq(true, false) map { a => 
      Seq(true, false) map { b => 
        Seq(true, false) map { c => 
          println(s"${a}\t${b}\t${c}\t${f(a, b, c)}")
        }
      }
    }
  }
  //49
  def gray(n:Int):List[String] = {
    def C(c:Int) : String = {
      def CEx(c:Int, n:Int) : String = n match {
        case 1 => if (c == 1) "1" else "0"
        case s @ _ => CEx(c >> 1, n-1) + (if (((c & 1) ^ ((c >> 1) & 1)) == 1) "1" else "0")
      }
      CEx(c, n)
    }
    ((0 until 1 << n) toList) map { C(_)}
  }
  //50
  
}