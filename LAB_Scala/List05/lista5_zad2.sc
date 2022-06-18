//Krzysztof Szuba - lista 5
//zadanie 2
type Samochod = (String,String,Int)
type Samochody = List[Samochod]

val lista:Samochody = List(("Opel", "astra", 1999), ("Renault", "megane", 2004), ("Opel", "corsa", 2009), ("Nissan", "micra", 2004), ("Opel", "corsa", 2009), ("Nissan", "micra", 2003))
def matching(elem: Samochod) = {
  val (marka, _, _) = elem
  marka
}
def cars(list:Samochody)= {
  def helper(list: Samochody, newList: List[(String, Int)],marki:List[String]): List[(String, Int)] = {
    val (marka, _, _) = list.head
    if marki.length>0 && marki.contains(marka) then {
      if list.tail!=Nil then helper(list.tail, newList, marki) else newList }
    else {
      val para = (list foldLeft(marka, 0)) ((x, h) => if matching(h) == x._1 then (marka, x._2 + 1) else (marka, x._2))

      if list.tail != Nil then helper(list.tail, para :: newList,marka::marki) else newList
    }
  }
  helper(list,List(),List())
}
cars(lista)



