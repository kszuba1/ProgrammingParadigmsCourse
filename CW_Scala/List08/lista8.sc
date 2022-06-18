//Krzysztof Szuba lista 8
//zadanie 1
def repLList[A](llist: LazyList[A])(f:Int=>Int)=
  def helper(llist2: LazyList[A],n:Int,ind:Int): LazyList[A] =
    (ind, llist2) match
      case (_, LazyList()) => LazyList()
      case (0, _#:: tl) => helper(llist2,n+1,f(n+1))
      case (_, hd #:: _ ) => hd #:: helper(llist2,n,ind-1)
  helper(llist,1,f(1))

repLList(LazyList('a','b'))((i:Int)=>i*i).toList
//lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
//lrepeat(3)(LazyList()).take(15).toList == List()
//zadanie 1






