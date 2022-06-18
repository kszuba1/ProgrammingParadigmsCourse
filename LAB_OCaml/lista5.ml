(* Krzysztof Szuba - lista 5 *)
(* zadanie 3 *)
let rec (+) vec1 vec2=
  let rec helper vec1accum vec2accum leng1 leng2 newVec=
    match leng1 with
    | leng2 -> if vec1accum=[] then List.rev newVec else helper (List.tl vec1accum) (List.tl vec2accum) leng1 leng2 ((List.hd vec1accum +. List.hd vec2accum)::newVec)
    | _ -> if leng1>leng2 then helper vec1accum (List.rev (0.::List.rev vec2accum)) leng1 (leng2+.1.) newVec
           else helper (List.rev (0.::(List.rev vec1accum))) vec2accum (leng1+.1.) leng2 newVec
  in helper vec1 vec2 (float_of_int(List.length vec1)) (float_of_int(List.length vec2)) []
;;
[1.]+[2.;3.];;



let rec plus vec1 vec2 =
  let rec helper vec1accum vec2accum leng1 leng2 newVec=
     if leng1 > leng2 then helper vec1accum (List.rev (0.::(List.rev vec2accum))) leng1 (leng2+.1.) newVec
     else if leng2>leng1 then helper (List.rev (0.::(List.rev vec1accum))) vec2accum (leng1+.1.) leng2 newVec
     else if leng1=leng2 && vec1accum=[] then List.rev newVec
     else helper (List.tl vec1accum) (List.tl vec2accum) leng1 leng2 ((List.hd vec1accum +. List.hd vec2accum)::newVec)
     in helper vec1 vec2 (float_of_int(List.length vec1)) (float_of_int(List.length vec2)) []
                   
;;
plus[1.][2.;3.];;
plus[1.;2.][-1.;-1.]

            

 
