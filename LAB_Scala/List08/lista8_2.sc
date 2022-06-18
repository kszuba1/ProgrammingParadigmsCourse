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

def isUniq[A](graph:Graph[A])=

  def isUniqHelper(elems:List[Graph[A]],elemsList:List[A],boolList:List[Boolean]):List[Boolean]=

    def matchingA(elem:Graph[A]):A = {
      elem match{
        case Element(value,_) => value
      }
    }
    print("1",matchingA(elems.head),elems.size)
    if elems.tail!=Nil then isUniqHelper(elems.tail,matchingA(elems.head)::elemsList,boolList)
    else boolList



  def isUniqHelper2(graph:Graph[A],elemList:List[A],boolList:List[Boolean]):List[Boolean]=

    graph match {
      case Element(value,Nil) => if elemList.contains(value) then true::boolList else false::boolList
      case Element(value,list) => if elemList.contains(value) then true::boolList else isUniqHelper(list,value::elemList,false::boolList)
    }
  if isUniqHelper2(graph,Nil,Nil).contains(true) then false else true


val graph=Element(5,List(Element(4,List(Element(7,Nil))),Element(7,Nil)))
isUniq(graph)

//zadanie 3






