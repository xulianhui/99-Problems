//90
def eightQueen():List[List[Int]] = {
  
  def valid(li:List[Int], l:Int, r:Int):Boolean = li match {
    case Nil => true
    case head :: tail => (head != l) && (head != r) && valid(tail, l - 1, r + 1)
  }

  def check(li:List[Int]):Boolean = li match {
    case Nil => true
    case head :: tail => valid(tail, head - 1, head + 1)
  }

  def eightQueenEx(li:List[Int], se:List[Int]):List[List[Int]] = {
    if (check(se) == false) Nil 
    else if (li.isEmpty) List(se)
    else {
      li flatMap { e => 
        eightQueenEx( li diff List(e), e :: se)
      }
    }
  }

  eightQueenEx((1 to 8).toList, Nil)
}

//91 
def knightTour(n:Int):List[(Int, Int)] = {
  
  val dir = List((1, 2), (1, -2), (2, 1), (2, -1), (-1, -2), (-1, 2), (-2, -1), (-2, 1))

  def knightTourEx(pos:List[(Int, Int)], tou:List[(Int, Int)]) : List[(Int, Int)] = {
    println(tou.head)
    def valid (p:(Int, Int)) :Boolean = {
      if (p._1 < 1 || p._1 > n || p._2 < 1 || p._2 > n || !(pos exists (_ == p))) false
      else true
    }

    dir flatMap { e =>

      val now = tou.head
      val nex = (now._1 + e._1, now._2 + e._2)

      if (valid (nex)) {
        knightTourEx(pos diff List(nex), nex :: tou)
      } else Nil
    }
  }
  val pos = ((1 to 10 ) toList) flatMap { a=> 
              ((1 to 10 ) toList) map { b=>
                (a, b)
              }
            }

  knightTourEx(pos diff List(1, 1), List((1, 1)))
}

((1 to 10 ) toList) flatMap { a=> 
  ((1 to 10 ) toList) map { b=>
    (a, b)
  }
}
