sealed abstract class Tree[+T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  def leafCount:Int
  def leafList:List[T]
  def internalList:List[T]
  def atLevel(le:Int):List[T]
  def layoutBinaryTree(le:Int = 1, hr:Int = 0):Int
  // def toString:String
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
    case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case _          => false
  }

  def addValue[U >: T <% Ordered[U]](x: U) =
    if (x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))

  def isSymmetric: Boolean = left.isMirrorOf(right)


  def leafCount:Int = {
    (left, right) match {
      case (End, End) => 1
      case (_, _) => left.leafCount + right.leafCount
    }
  }

  def leafList:List[T] = {
    (left, right) match {
      case (End, End) => List(value)
      case _ => left.leafList ::: right.leafList
    }
  }

  def internalList:List[T] = {
    (left, right) match {
       case (End, End) => Nil
       case _ => List(value) ::: left.internalList ::: right.internalList
    }
  }

  def atLevel(le:Int):List[T] = {
    le match {
      case 1 => {
        List(value)
      }
      case _ => {
        left.atLevel(le-1) ::: right.atLevel(le-1)
      }
    }
  }

  def layoutBinaryTree(le:Int = 1, hr:Int = 0):Int = {
    val lc:Int = left.layoutBinaryTree(le+1, hr)
    val rc:Int = right.layoutBinaryTree(le+1, hr + lc + 1)
    println(f"(x:${(hr+lc+1)} y:${le})")
    lc + rc + 1
  }
  override def toString:String = {
    (left, right) match {
      case (End, End) => s"$value"
      case _ => s"$value(${left.toString},${right.toString})"
    }
  }
}

case object End extends Tree[Nothing] {
  override def toString:String = ""
  def layoutBinaryTree(le:Int = 1, hr:Int = 0) = 0
  def atLevel(le:Int) = Nil
  def internalList = Nil
  def leafList = Nil
  def leafCount:Int = 0
  def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
  def isSymmetric: Boolean = true
  def addValue[U <% Ordered[U]](x: U) = Node(x)
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
  def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = 
    l.foldLeft(End: Tree[T])((r, e) => r.addValue(e))
}

object Tree {
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }
  def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = 
    l.foldLeft(End: Tree[T])((r, e) => r.addValue(e))
}

Node('a', Node('b', Node('c'), Node('d')), Node('c',Node('c'), Node('d'))).toString
