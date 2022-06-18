(* Krzysztof Szuba - lista 4 *)
(* zadanie 1 *)

let rec findMinMax(list)=
  if list=[] then raise (Failure "Lista pusta")
  else 
  let rec findMinMaxHelper(list,min,max)=
    match list with
      [] -> (min,max)
    | _  -> if List.hd list <= min then findMinMaxHelper(List.tl list,List.hd list,max)
            else if List.hd list > max then findMinMaxHelper(List.tl list,min,List.hd list)
            else findMinMaxHelper(List.tl list,min,max)
  in findMinMaxHelper(List.tl list, List.hd list, List.hd list)
;;
findMinMax [1;2;3;4;5];;
findMinMax [0;5;-10;13;9;2];;
findMinMax [7];;
findMinMax [];;
      

let find(list)=
  if list=[] then raise (Failure "Lista pusta")
   else 
  List.fold_left(fun(min,max) h -> ((if h<=min then h else min), if h>max then h else max))(List.hd list, List.hd list) list
;;
find [7;1;2;3;4;0];;


(* zadanie 2 *)
let findDer f a b dx krok =
  let rec findDerHelper (elem,list) =
    if elem>b then List.rev list else findDerHelper ((elem +. krok),(elem,(f(elem+.dx)-.f(elem))/.dx)::list)
  in findDerHelper (a,[])
;;
findDer (fun x->2.*.x*.x) 1. 10. 0.000001 2. ;;


       


