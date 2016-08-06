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

