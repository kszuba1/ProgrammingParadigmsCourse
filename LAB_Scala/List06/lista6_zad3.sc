//Krzysztof Szuba lista 6
//zadanie 3

sealed trait BTree[A]
case class Leaf[A](elem: A) extends BTree[A]
case class NodeP[A](child: BTree[A]) extends BTree[A]
case class NodeLP[A](left: BTree[A], right: BTree[A]) extends BTree[A]

sealed trait PS[A]
case class P[A](str: String, elem: A) extends PS[A]
case class S[A](str: String) extends PS[A]

def postOrder[A](tree: BTree[A]): List[PS[A]] = {
  tree match {
    case Leaf(elem) => P("Dana", elem) :: Nil

    case NodeP(child) => (child) match {
      case Leaf(_) => postOrder(child) :+ S("Wezel (element)")
      case _ => postOrder(child) :+ S("Wezel (prawo)")
    }

    case NodeLP(left, right) => (left, right) match {
      case (Leaf(_), Leaf(_)) => postOrder(left) ++ postOrder(right) :+ S("Wezel (element, element)")
      case (Leaf(_), _) => postOrder(left) ++ postOrder(right) :+ S("Wezel (element, prawo)")
      case (_, Leaf(_)) => postOrder(left) ++ postOrder(right) :+ S("Wezel (lewo, element)")
      case (_, _) => postOrder(left) ++ postOrder(right) :+ S("Wezel (lewo, prawo)")
    }
  }
}
val tree = NodeLP(NodeLP(NodeLP(Leaf(2), Leaf(1)), NodeP(Leaf(4))), Leaf(3))
postOrder(tree)


