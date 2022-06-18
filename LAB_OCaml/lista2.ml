(* Krzysztof Szuba *)

(* Zadanie 1 *)
(* a *)
let sqrInts(x,y)=
  x*x, y*y
;;
sqrInts(2,3);;
sqrInts(-10,0);;

(* b *)
let checkFloats((a:float), (b:float))=
  if a=b then true else false
;;
checkFloats(4.1,5.2);;
checkFloats(-5.5,-5.5);;

(* c *)
let rec mulList(list,x)=
  if list=[] then []
  else (List.hd list * x) :: mulList(List.tl list, x)
;;
mulList([2;3;4],2);;
mulList([],5);;
mulList([-5;0;-13],-1);;

(* zadanie 3 *)
let rec split(x,list)=
  if list=[] then []
  else if x<=0 || x>List.length list then raise(Failure "zly parametr x")
  else let rec splitHelper(x,list,i)=
         if i=List.length list then []
         else if i=x then []
         else List.nth list i :: splitHelper(x,list,i+1)
       in splitHelper(-1,list,x) @ splitHelper(x,list,0)    
;;
split(5,[1;2;3;4;5]);;
split(0,[1;2;3;4;5]);;
split(1,[1;2]);;
split(2,[1;2]);;
split(1,[1;2;3;4;5]);;
split(3,[1;2;3;4;5]);;
