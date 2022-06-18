//Krzysztof Szuba lista 5
//zadanie 1
def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] =
  def lrepHelper(i: Int,llist: LazyList[A]): LazyList[A] =
    (i, llist) match
      case (_, LazyList()) => LazyList()
      case (0, _#:: tl) => lrepHelper(k,tl)
      case (_, hd #:: _ ) => hd #:: (lrepHelper(i - 1,llist))
  lrepHelper(k,lxs)

lrepeat(3)(LazyList('a','b','c','d')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()

//zadanie 2

val lfib: LazyList[Int] =
  def lfibHelper(a: Int,b: Int): LazyList[Int] =
    a #:: lfibHelper(b,a + b)
  lfibHelper(0,1)

lfib.take(15).toList == List(0,1,1,2,3,5,8,13,21,34,55,89,144,233,377)
lfib.take(5).toList == List(0,1,1,2,3)
lfib.take(0).toList == List()

//zadanie 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]


def lBreadth[A](ltree: lBT[A]) =
  def lBreadthHelper(xs: List[lBT[A]]): LazyList[A] =
    xs match
      case LEmpty :: t => LazyList()
      case LNode(x, lLT, lRT) :: t => x #:: (lBreadth_iter(t ::: List(lLT(), lRT())))
  lBreadth_iter(List(ltree))

def lTree(n: Int): lBT[Int] = LNode(n, ( () => lTree(2 * n)), (() => lTree(2 * n + 1)))

lBreadth(lTree(1)).take(20).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
lBreadth(LEmpty).take(20).toList == List()