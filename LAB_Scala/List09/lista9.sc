//Krzysztof Szuba lista 9
//zadanie 1
def repLList[A](llist: LazyList[A],f:Int=>Int)=
  def helper(llist2: LazyList[A],n:Int,ind:Int): LazyList[A] =
    (ind, llist2) match
      case (_, LazyList()) => LazyList()
      case (0, _#:: tl) => helper(tl,n+1,f(n+1))
      case (_, hd #:: _ ) => hd #:: helper(llist2,n,ind-1)
  helper(llist,1,f(1))

repLList(LazyList('a','b','c'),(i:Int)=>i*i).toList
