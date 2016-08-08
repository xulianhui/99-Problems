//1
def last [T](list : List[T]): T = list.last
//2
def penultimate [T](list : List[T]):T = list(list.length - 2)
//3
def nth [T](index:Int, list : List[T]):T = {
  if (index + 1 < list.length)
    list(index)
  else
    throw new Error()
}
//4
def length[T](list:List[T]):Int = list.length
//5
def reverse[T](list:List[T]):List[T] = list.reverse
//6
def isPalindrome[T](list:List[T]):Boolean = list equals list.reverse
//7
def flatten(list:List[Any]):List[Any] = {/*{List[Any].()}*/
  list flatMap {
    case ls:List[_] => flatten(ls)
    case e => List(e)
  }
}
//8
def compress[T](list:List[T]):List[T] = {
  (list.toSet).toList
}
//9
def pack[T](list:List[T]):List[List[T]] = {
  if (list == Nil) Nil
  else {
    val (pac, nex) = list span (_ == list.head)
    pac :: pack(nex)
  }
}
//10
def encode[T](list:List[T]):List[(Int, T)] = {
  pack(list) map (e => (e.length, e.head))
}

//11
def encodeModified[T](list:List[T]):List[Any] = {
  if (list == Nil) Nil
  else {
    val (pac, nex) = list span (_ == list.head)
    pac.length match {
      case 1 => pac.head :: encodeModified(nex)
      case _ => (pac.length, pac.head) :: encodeModified(nex)
    }
  }
}
//12
def decode[T](list:List[(Int, T)]):List[T] = {
  list match {
    case Nil => Nil
    case head :: tail => {
      List.fill(head._1)(head._2) ::: decode(tail)
    }
  }
}
//13
def encode[T](list:List[T]):List[(Int, T)] = {
  if (list == Nil) Nil
  else {
    val (pac, nex) = list span (_ == list.head)
    (pac.length, pac.head) :: encode(nex)
  }
}
//14
def duplicate[T](list:List[T]):List[T] = {
  list flatMap {
    case Nil => Nil
    case ls => List.fill(2)(ls)
  }
}
//15
def duplicateN[T](n:Int, list:List[T]):List[T] = {
  list flatMap {
    case Nil => Nil
    case ls => List.fill(n)(ls)
  }
}
//16
def drop[T](n:Int, list:List[T]):List[T] = {
  def dr[T](c:Int, ls:List[T]):List[T] = {
    (c, ls) match {
      case (_, Nil) => Nil
      case (1, _ :: tail) => dr(n, tail)
      case (_, he :: tail) => he :: dr(c - 1, tail)
    }
  }
  dr(n, list)
}
//17 
def split[T](n:Int, list:List[T]):(List[T],List[T]) = {
  (list.dropRight(list.length-n), list.drop(n))
}
//18
def slice[T](l:Int, r:Int, list:List[T]):List[T] = {
  list.dropRight(list.length-r).drop(l)
}
//19 
def rotate[T](n:Int, list:List[T]):List[T] = {
  val ro = if (n < 0) n + list.length else n
  list.drop(ro) ::: list.dropRight(list.length-ro)
}
//20
def removeAt[T](n:Int, list:List[T]):(List[T], T) = {
  list.splitAt(n) match {
    case (pre, e::tail) => (pre:::tail, e)
    case _ => throw new Error
  }
}
//21
def insertAt[T](ne:T, p:Int, list:List[T]):List[T] = {
  list.dropRight(list.length-p) ::: (ne :: list.drop(p))
}
//22
def range(l:Int, r:Int):List[Int] = {
  Range(l, r+1).toList
}
//23
def randomSelect[T](n:Int, list:List[T]):List[T] = {
  val rnd = new util.Random
  if (n == 0) Nil
  else {
    val (ls, e) = removeAt(rnd.nextInt(list.length), list)
    e :: randomSelect(n-1, ls)
  }
}
//24
def lotto(n:Int, m:Int):List[Int] = {
  randomSelect(n,range(1, m))
}
//25
def randomPermute[T](list:List[T]):List[T] = {
  randomSelect(list.length, list)
}
//26
/*
def combinations[A, B](n:Int, list:List[A]):List[List[A]] = {
  var res:List[List[A]] = Nil
  def cmb(n:Int, list:List[A], re:List[A]):Unit = {
    if (n > list.length) Nil
    if (n == 0) {
      res = re :: res
    }
    else {
      val tail = list.tail
      cmb(n-1, tail, list.head :: re)
      cmb(n, tail, re)
    }
  }
  cmb(n, list, Nil)
  res
}
*/
object P26 {
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = 
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
}

flatMapSublists(List(1, 2, 3, 4))(sl => {println(sl), sl})

