//Krzysztof Szuba lista 8
//zadanie 1

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

def sumList(tree:BT[List[Int]]):BT[Int]= {
  tree match {
    case Empty => Empty
    case Node(elem, left, right) => Node((elem foldLeft (0)) ((sumAccum, hd) => sumAccum + hd), sumList(left), sumList(right))
  }
}
val tree=Node(List(1,2,3),Node(List(1,2,3),Empty,Node(List(17,3),Empty,Empty)),Empty)
sumList(tree)

//zadanie 2
sealed trait Graph[+A]
case class Element[+A](value: A,connectedElems: List[Graph[A]]) extends Graph[A]

def matchingVal[A](elem:Graph[A]):A = {
  elem match{
    case Element(value,_) => value
  }
}
def matchingList[A](elem:Graph[A])= {
  elem match{
    case Element(_,list) => list
  }
}
def isListUniq[A](list:List[A])= {
  def helper(list2: List[A], newList: List[A]): Boolean = {
    list2 match {
      case Nil => if list.size != newList.size then false else true
      case h :: tl => if newList.contains(h) then helper(list2.tail, newList) else helper(list2.tail, h :: newList)
    }
  }

  helper(list, Nil)
}

def isUniq[A](graph: Graph[A])= {

  var graphList: List[A] = Nil

  def helper(graph2: Graph[A]): Unit = {
    var graphVal = matchingVal(graph2)
    val childList = matchingList(graph2)
    graphList = graphVal :: graphList
    for (i <- 0 to childList.size - 1) {
      helper(childList(i))
    }
  }

  helper(graph)
  isListUniq(graphList)
}





val graph=Element(8,List(Element(3,List(Element(4,Nil),Element(5,List(Element(7,Nil))))),Element(9,Nil),Element(1,Nil)))
val graph2=Element(8,List(Element(3,List(Element(4,Nil),Element(5,List(Element(7,Nil))))),Element(9,Nil),Element(3,Nil)))
isUniq(graph)
isUniq(graph2)









def toBinaryTree[A](graph:Graph[A])=
  def helper(graph2: Graph[A]):BT[A]=
    graph2 match {
      case Element(value,Nil) => Node(value,Empty,Empty)
      case Element(value,List(element)) => Node(value,helper(element),Empty)
      case Element(value,list) => Node(value,helper(list(0)),helper(list(1)))
    }
  helper(graph)
val graph3=Element(8,List(Element(3,List(Element(4,Nil),Element(5,List(Element(7,Nil))))),Element(9,Nil),Element(1,Nil)))
toBinaryTree(graph3)


