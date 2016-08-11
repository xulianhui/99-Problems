class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  def nodeCount:Int = children.foldLeft(1)((a, e) => a + e.nodeCount)           // 69
  def toStr:String = children match {                                           // 70
    case Nil => s"$value^"
    case _ => children.foldLeft(s"$value")((a, e) => a + e.toStr) + "^"
  }

  def internalPathLengthEx(de:Int):Int = {
    children.foldLeft(de) ((s, e) => s + e.internalPathLengthEx(de + 1))
  }

  def internalPathLength:Int = {                                                // 71
    internalPathLengthEx(0)
  }

  def postorder:List[T] = {                                                     // 72
    children.foldRight (List(value))((e, s) => e.postorder ::: s)
  }
  def lispyTree:String = {                                                      // 73
    children.length match {
      case 0 => value.toString
      case _ => s"($value ${children.map(_.lispyTree).mkString(" ")})"
    }
  }
}

object MTree {
  
  def apply[T](value: T) = new MTree(value, List())
  
  def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  
  implicit def fromStr(mt:String):MTree[Char] = {                               // 70
    
    def find(idx:Int, nes:Int):Int = {
      if (nes == 0) idx
      else find(idx + 1, if (mt(idx) == '^') nes - 1 else nes + 1)
    }

    def splitStr(idx:Int):List[String] = {
      // println(s"$mt  $idx") 
      if (idx >= mt.length - 1) Nil
      else {
        val nex = find(idx + 1, 1)
        mt.substring(idx, nex - 1) :: splitStr(nex)
      }
    } 
    
    MTree(mt(0), splitStr(1).map(s => fromStr(s)))
  }

  def fromLisp(lisp:String):MTree[Char] = {                                     //73
    val li = lisp split (" ") mkString

    def find(idx:Int, nes:Int):Int = {
      println(s"$idx $nes")
      if (nes == 0 && li(idx) != '(') idx
       else li(idx) match {
        case '(' => find(idx + 1, nes + 1)
        case ')' => if (nes == 1) idx else find(idx + 1, nes - 1)
        case _ => find(idx + 1, nes)
      }
    }

    def splitLi(idx:Int):List[String] = {
      if (idx >= li.length - 1) Nil
      else {
        val nex = find(idx, 0)
        li.substring(idx, nex+1) :: splitLi(nex+1)
      }
    }

    def fromLispEx(idx:Int):MTree[Char] = {
      if (li(idx) == '(') {
        MTree(li(1), splitLi(2) map {fromLisp(_)})
      } else {
        MTree(li(0))
      }
    }
    fromLispEx(0)
  }
}

import MTree._

// "afg^^c^bd^e^^^" lispyTree

// "afg^p^d^^c^bd^e^^^" lispyTree

// (a(fg)c(bde))
// (a(fg)(bde))