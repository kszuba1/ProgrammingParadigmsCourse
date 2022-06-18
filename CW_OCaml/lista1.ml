(* Krzysztof Szuba*)

(* Zadanie1 *)
let rec flatten1 list =
  if list = [] then [] 
  else List.hd list @ flatten1(List.tl list)
;;
(* Zadanie1 - testy *)
flatten1 [[5;6];[1;2;3]];;
flatten1 [];;
flatten1 [[2;-5];[8;4];[1;2;-3];[]];;

(* Zadanie2 *)
let rec count(x,list) =
  if list = [] then 0
  else 
  let temp = if x = List.hd list then 1 else 0 in
  temp + count(x,List.tl list)
;;
(* testy *)
count('a',['a';'l';'a']);;
count('a',[]);;
count(5,[-4;5;8;-5;5;5]);;

(* zadanie3 *)
let rec replicate(x,n)=
  if n<=0 then []
  else x :: replicate(x,n-1)
  ;;
(* testy *)
replicate("la",3);;
replicate("la",0);;
replicate(-3,5);;
replicate(0,-5);;

(* Zadanie4 *)
let rec sqrList(list)=
  if list = [] then []
  else (List.hd list)*(List.hd list) :: sqrList(List.tl list)
;;
(* testy *)
sqrList[1;2;3;4];;
sqrList[];;
sqrList[0;-2;13];;

(* Zadanie5 *)
let rec palindrome(list)=
  if list=[] then false
  else if list=List.rev list then true else false
;;
(* testy *)
palindrome['a';'l';'a'];;
palindrome['b';'c';'a'];;
palindrome[];;

(* Zadanie6 *)
let rec listLength(list)=
  if list=[] then 0
  else 1 + listLength(List.tl list)
;;
(* testy *)
listLength['a';'b';'c';'d'];;
listLength[];;
