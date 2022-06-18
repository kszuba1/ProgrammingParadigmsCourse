//Krzysztof Szuba - lista 6
//zadanie 1
def whileLoop(cond: => Boolean)(expr: => Unit): Unit =
  if cond then {
    expr
    whileLoop(cond)(expr)
  }

var x = 0
whileLoop(x <= 9)({println(x); x += 1})

//zadanie 2

//a swap
def swap[A](tab: Array[A])(i: Int)(j: Int): Unit =
  val aux = tab(i);
  tab(i) = tab(j);
  tab(j) = aux
//b partition
def choose_pivot[A](tab: Array[A])(m: Int)(n: Int): A = tab((m + n) / 2)

def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) =
  var i = l;
  var j = r;
  var pivot = choose_pivot(tab)(l)(r)
  while i <= j do
    while tab(i) < pivot do i += 1
    while pivot < tab(j) do j -= 1
    if i <= j
    then {
      swap(tab)(i)(j);
      i += 1;
      j -= 1
    }
  (i, j)

//c quick
def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
  if l < r then
    val (i, j) = partition(tab)(l)(r)
    if j - 1 < r - i
    then {
      val _ = quick(tab)(l)(j);
      quick(tab)(i)(r)
    }
    else {
      val _ = quick(tab)(i)(r);
      quick(tab)(l)(j)
    }
  else ()

//d quicksort
def quicksort(tab: Array[Int]) = quick(tab)(0)(tab.length - 1)

//testy
val t1 = Array(4, 8, 1, 12, 7, 3, 1, 9)
quicksort(t1)
t1













