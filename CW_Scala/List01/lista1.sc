//Krzysztof Szuba

//Zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] = {
  if xss == Nil then Nil
  else xss.head ++ flatten1(xss.tail)
}
//Testy
flatten1 (List(List(5,6), List(1,2,3))) == List(5, 6, 1, 2, 3)
flatten1 (List(List("Ala", "ma", "kota"))) == List("Ala", "ma", "kota")
flatten1(Nil) == List()
flatten1(List())==Nil

//Zadanie 2
def count[A](x: A, xs: List[A]): Int ={
  if xs == Nil then 0
  else (if x==xs.head then 1 else 0) + count(x,xs.tail)
}
//Testy
count("a",List("a","l","a"))==2
count("a",List("ab","l","ab"))==0
count("a",List())==0
count(-5,List(5,3,-5,0))==1

//Zadanie 3
def replicate[A](x: A, n: Int): List[A]={
  if n<=0 then Nil
  else x :: replicate(x,n-1)
}
//Testy
replicate("la",3)==List("la","la","la")
replicate("la",0)==Nil
replicate(5,1)==List(5)

//Zadanie 4

//metoda
def sqrList(xs: List[Int]): List[Int]={
  if xs==Nil then Nil
  else (xs.head * xs.head) :: sqrList(xs.tail)
}

//funkcja
val sqrList2: List[Int] => List[Int] = (xs: List[Int]) =>
  if xs==Nil then Nil
  else xs.head * xs.head :: sqrList2(xs.tail)
//Testy
sqrList(List(1,2,3,-4))==List(1,4,9,16)
sqrList(List())==Nil
sqrList(List(0,-20,13,-7))==List(0,400,169,49)

sqrList2(List(1,2,3,-4))==List(1,4,9,16)
sqrList2(List())==Nil
sqrList2(List(0,-20,13,-7))==List(0,400,169,49)

//Zadanie 5
def  palindrome[A](xs: List[A]): Boolean ={
  xs==xs.reverse
}
//Testy
palindrome(List("a","l","a"))==true
palindrome(List("a","l","ab"))==false
palindrome(Nil)==true

//Zadanie 6
def  listLength[A](xs: List[A]): Int={
  if xs==Nil then 0
  else 1 + listLength(xs.tail)
}
//Testy
listLength(List())==0
listLength(List(1,2,3))==3


