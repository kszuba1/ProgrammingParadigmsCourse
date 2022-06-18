type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;
let rec lfrom k = LCons(k, lazy (lfrom(k+1)));;
let rec toLazyList = function
    [] -> LNil
  | x :: xs -> LCons(x, lazy (toLazyList xs))
;;
let rec ltake = function
  | (0,_) -> []
  | (_,LNil) -> []
  | (n, LCons(x,lazy xf)) -> x::ltake(n-1,xf)
;;

(* zadanie 1*)
let repLList llist func  =  
  let rec helper llist2 n i=
    match (i,llist2) with
    | (_,LNil) -> LNil
    | (0,LCons(_,lazy tl)) -> helper tl (n+1) (func (n+1))
    | (_,LCons(hd,_)) -> LCons (hd, lazy (helper llist2 n (i-1))) 
  in helper llist 1 (func 1)
;;
ltake(10,(repLList(toLazyList [1;3]) (fun i -> i+2)));;
ltake(100,(repLList(toLazyList[-3.4;5.5;-17.5])(fun i -> i*i*i)));;


(* zadanie 2 *)

type slowo = int * int;;
type slownik = slowo list;;

let addDictionary word (dict:slownik) =
  let rec helper newDict oldDict =
    match oldDict with
    | [] -> []  
    | (value,f)::tl -> if word>value then helper ((value,f)::newDict) tl
                       else if word=value then (List.rev ((value,(f+1))::newDict)) @ tl
                       else (List.rev ((value,f)::((word,1)::newDict))) @ tl
  in helper [] dict
;;
let dictionary:slownik=[(1,3);(2,1);(5,7);(6,2)];;
addDictionary 3 dictionary;;
addDictionary 5 dictionary;;

let add2 word (dict:slownik) =
  let rec helper newDict oldDict =
    match oldDict with
    | [] -> []
    | (word,f)::tl -> List.rev ((word,(f+1))::newDict) @ tl
    | (value,f) ::tl -> if word>value then helper ((value,f)::newDict) tl
                        else (List.rev ((value,f)::((word,1)::newDict))) @ tl
  in helper [] dict;;
                              
                          
                                            
                                         
                                          
