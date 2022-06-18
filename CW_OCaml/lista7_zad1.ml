(* Krzysztof Szuba lista 7 *)

module type QUEUE_FUN =
  sig
    type 'a t
    exception Empty of string
    val empty: unit -> 'a t
    val enqueue: 'a * 'a t -> 'a t
    val dequeue: 'a t -> 'a t
    val first: 'a t -> 'a
    val isEmpty: 'a t -> bool
  end;;

(* zad 1a *)

module SingleListQueue : QUEUE_FUN =
  struct
    type 'a t = 'a list
    exception Empty of string
    let empty() = []
    let enqueue (e,q) = q @ [e]
                      
    let dequeue q =
      match q with
        [] -> []
      | _::tl -> tl

    let first q =
      match q with
        [] -> raise (Empty "module SingleListQueue: first")
      | h::_ -> h

    let isEmpty q = q = []

  end;;
(* testy *)
SingleListQueue.(first(enqueue(3,enqueue(5,dequeue(enqueue(1,empty()))))));;
SingleListQueue.(isEmpty(enqueue(7,empty())));;
SingleListQueue.(isEmpty(dequeue(enqueue(7,empty()))));;
SingleListQueue.(first(dequeue(enqueue(3,empty()))));;

(* zad 2b *)

module DoubleListQueue : QUEUE_FUN =
  struct
    type 'a t = 'a list * 'a list
    exception Empty of string
    let empty() = ([],[])
                
    let enqueue(e,q) =
      match q with
        ([],[]) -> ([e],[])
      | (xl,yl) -> (xl,e::yl)

    let dequeue q =
      match q with
        ([],[])  -> ([],[])
      | ([e],yl) -> (List.rev yl,[])
      | (e::xtl,yl) -> (xtl,yl)

    let first q =
      match q with
        ([],[]) -> raise (Empty "module DoubleListQueue: first")
      | (h::_,_) -> h

    let isEmpty q = q = ([],[])

  end;;
(* testy *)
DoubleListQueue.(first(enqueue(3,enqueue(-5,dequeue(dequeue(enqueue(-3,enqueue(1,empty()))))))));;
DoubleListQueue.(isEmpty(dequeue(enqueue(7,empty()))));;
DoubleListQueue.(first(empty()));;


                      
        

                     
      
        

      
                    
                         




      
                   
      
