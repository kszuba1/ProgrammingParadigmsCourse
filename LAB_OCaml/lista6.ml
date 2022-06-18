type litera = Zawiera | Nie_zawiera;;
type dane_slow = Puste | Krotkie of litera | Srednie of litera | Dlugie of litera;;
let rec showDet list x =
  if list=[] then [] else 
  let leng = String.length (List.hd list) in
  let cont = String.contains (List.hd list) x in
   if leng=0 then Puste :: showDet (List.tl list) x
  else if leng<=10 then (if cont then Krotkie Zawiera :: showDet (List.tl list) x else Krotkie Nie_zawiera :: showDet (List.tl list) x)
  else if leng<=20 then (if cont then Srednie Zawiera :: showDet (List.tl list) x else Srednie Zawiera :: showDet (List.tl list) x)
  else if cont then Dlugie Zawiera :: showDet (List.tl list) x else Dlugie Nie_zawiera :: showDet (List.tl list) x
;; 
showDet["";"osa";"przescieradlo";"piedziesieciogroszowka"] 'r';;
