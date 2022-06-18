
//zadanie 1
def sqrInts(x:(Int,Int)):(Int, Int)={
  ((x._1)*(x._1),(x._2)*(x._2))
}
sqrInts(2,3)==(4,9)

def checkFloats(a:Double, b:Double)={
  if a==b then true else false
}
checkFloats(5.3,5.3)==true
checkFloats(5.3,-5.3)==false

def mulList(list: List[Int],x:Int): List[Int]={
  if list==Nil then Nil
  else (list.head * x) :: mulList(list.tail, x)
}
mulList(List(2,3,4),2)==List(4,6,8)
mulList(List(),3)==Nil
mulList(List(0,3,-4),-5)==List(0,-15,20)

//Zadanie 2
def checkList(list: List[Int]):Boolean={
  if list == Nil then throw new RuntimeException("Lista pusta!")
  def checkListHelper(elem: Int,list: List[Int]):Boolean={
    if elem<list.head then false
    else if list.tail!=Nil then checkListHelper(list.head, list.tail)
    else true
  }
   checkListHelper(list.head,list)
}

checkList(List(1,2,3,4))==false
checkList(List(1,1))==true
checkList(List(1,0,0))==true
checkList(List(1,1,0,-1,0))==false
checkList(List(1))==true
checkList(List())