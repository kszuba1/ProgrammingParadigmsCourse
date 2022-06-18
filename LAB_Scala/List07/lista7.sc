//Krzysztof Szuba - lista 7
//zadanie 1


//zadanie 2
def comp(list:List[Double => Double])=
  def compHelper(newFunc:Double => Double,list2:List[Double => Double]):Double => Double= {
    if list2==Nil then newFunc
    else compHelper(newFunc compose list2.head,list2.tail)
  }
  compHelper(list.head,list.tail)

def compFold(list:List[Double => Double]):Double=>Double=
  (list foldLeft((x:Double)=>x))((func,hd)=>(func compose hd))

val lista=(lista:List[Int])=>(lista foldLeft(0))((sumAccum,hd)=>sumAccum+hd)
lista(List(1,2,3,4))


