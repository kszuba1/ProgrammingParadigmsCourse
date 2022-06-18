(* Krzysztof Szuba lista 3 *)
(* zadanie 1 *)

let rec piCount(y:float)=
  let rec piCountHelper(accum,sum)=
  if (abs_float((2./.(sqrt(0.5+.0.5*.accum)*.sum))-.(2./.sum))<y) then 2./.(sqrt(0.5+.0.5*.accum)*.sum)  else
      piCountHelper(sqrt(0.5+.0.5*.accum),sum*.sqrt(0.5+.0.5*.accum)) in 
      piCountHelper(sqrt(0.5),sqrt(0.5))
;;
piCount(1.);;
piCount(0.1);;
piCount(0.01);;
piCount(0.001);;






