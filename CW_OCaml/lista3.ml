(* Krzysztof Szuba - lista 3 *)

(* Zadanie 3 *)
let sumProd xs =
  List.fold_left(fun(s,p) h -> (h+s,h*p))(0,1) xs;;
