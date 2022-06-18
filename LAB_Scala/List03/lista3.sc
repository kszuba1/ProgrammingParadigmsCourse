//Krzysztof Szuba zadanie 2 lista 3

def changeList[A](list:List[A]):List[A]={
  def changeListHelper[A](list:List[A],i:Int,n:Int,list2:List[A]):List[A]={
    (list,n) match {
      case (Nil,_) => list2
      case (_,0) => changeListHelper(list.tail, i-1, i-1,list2)
      case (_,_) => changeListHelper(list,i,n-1,list.head :: list2)
    }
  }
  changeListHelper(list.reverse,list.length,list.length,List())
}
changeList(List(1,2,3,4))