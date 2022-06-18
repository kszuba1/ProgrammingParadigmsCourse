import sun.security.util.Length

//Krzysztof Szuba lista 5
// zadanie 1
def uniqList[A](list:List[A]):List[A]={
  if list.tail==List() then list
  else if list.head==list.tail.head then uniqList(list.tail)
  else list.head :: uniqList(list.tail)
}
uniqList(List(1,2,2,3,3,3,4))

//zadanie 2
def subList(list: List[Int],elem: Int,length: Int)= {
  def subListHelper(accumList: List[Int], i: Int, newList: List[Int], bool: Boolean): List[Int] = {
    (accumList,i,bool) match {
      case (_,0,_) => newList.reverse
      case (Nil,_,true) => newList.reverse
      case (Nil,_,false) => throw new RuntimeException ("Brak danego elementu")
      case (_,_,true) =>  subListHelper (accumList.tail, i - 1, accumList.head :: newList, bool)
      case (_,_,_) => if elem == accumList.head
                      then subListHelper (accumList.tail, i - 1, accumList.head :: newList, true)
                      else subListHelper (accumList.tail, i, newList, bool)
    }
  }
  if list==Nil then throw new RuntimeException("Lista pusta!")
  else subListHelper(list, length, List(), false)

}
subList(List(1,2,3,4,5,6,7,8),3,3)
subList(List(1,2,3,4,5,6),5,9)
subList(List(1,2,3,4),5,1)


