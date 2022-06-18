(* Krzysztof Szuba - lista 5 *)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons(k, lazy (lfrom(k+1)));;

let rec toLazyList = function
    [] -> LNil
  | x :: xs -> LCons(x,lazy (toLazyList xs))
;;

let rec ltake  = function
  | (0,_) -> []
  | (_,LNil) -> []
  | (n, LCons(x,lazy xf)) -> x::ltake(n-1,xf)
;;
(* zadanie 1 *)                      
let lrepeat k llist =
  let nRepeats elem n = elem in
  let rec lrepHelper i llist =
    match (i,llist) with
    | (_, LNil) -> LNil
    | (0, LCons(_, lazy  tl)) -> lrepHelper k ( tl)
    | (_, LCons(hd, _)) -> LCons (hd, lazy (lrepHelper(i-1)llist))
  in lrepHelper k llist;;

ltake(15, (lrepeat 3 (toLazyList [1;2;3;4;5])));;

(* zadanie 2 *)
let lfib =
  let rec lfibHelper a b =
    LCons(a, lazy (lfibHelper b (a+b)))
  in lfibHelper 0 1;;
ltake(0, lfib);;
ltake(5, lfib);;
ltake(15, lfib);;

(* zadanie 3 *)

type 'a lBT = LEmpty | LNode of 'a *(unit ->'a lBT)*(unit->'a lBT);;

let lBreadth ltree =
  let rec lBreadthHelper  = function
      [] -> LNil
    | LEmpty::tl -> lBreadthHelper tl
    | LNode(x,left,right)::tl -> LCons(x,lazy(lBreadthHelper(tl@[left();right()])))
  in lBreadthHelper[ltree];;

let rec lTree n=LNode(n,(function() -> lTree (2*n)), (function () -> lTree (2* n+1)));;

ltake(10,lBreadth(lTree 1));;





                                              
                                              
                                              


