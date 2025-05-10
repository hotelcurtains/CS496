(* 
    Mutable state in OCaml
    Quiz 6 - 2 April 202
    Mutable fields in records: Binary Trees
    Name 1: Daniel Detore
    Name 2: Anthony Santilli
*)

type 'a node = { mutable data: 'a;
                 mutable left: 'a node option;
                 mutable right: 'a node option}

type 'a bt = { mutable root: 'a node option ;
               mutable size: int}

let t1 : int bt =
  { root = None;
    size = 0}

(*     
       33
      /  \
     22  44
         /
        37
*)

let t2 : int bt =
  { root = Some { data = 33;
                  left = Some {data=22; left=None; right=None} ;
                  right = Some {data=44;
                                left = Some {data=37; left=None;
                                             right=None};
                                right = None}
                };
    size = 4
  }


let rec height2 x =
  match x with
  | Some x -> max (1 + height2 x.left) (1 + height2 x.right)
  | None -> 0

(** [height t] returns the height of [t]. 
    Eg. [height t2 ==> 3]
*)
let rec height : 'a bt -> int =
  fun t ->
  height2 t.root


let rec mem2 x e =
  match x with
  | Some x -> (x.data == e) || mem2 x.left e || mem2 x.right e
  | None -> false

(** [mem e t] returns a boolean indicating whether [e] is in [t]. 
    Eg. [mem 23 t2 ==> false ]
    Eg. [mem 33 t2 ==> true ]
*)
let mem : 'a -> 'a bt -> bool =
  fun e t  ->
  mem2 t.root e




let rec preorder2 (x : 'a node option) =
  match x with
  | Some x -> x.data :: preorder2 x.left @ preorder2 x.right
  | None -> []

(** [preorder t] returns a list with the preorder traversal of [t]. 
    Eg. [preorder t2 ==> [33; 22; 44; 37] ]
*)
let preorder : 'a bt -> 'a list =
  fun t ->
  preorder2 t.root



(** [map f t] maps [f] to every element of [t].
*)
let map : ('a -> 'a) -> 'a bt -> unit =
  fun f t ->
  failwith "implement"

(** [mirror t] updates [t] to its mirror image (left and right
    children swapped).
*)
let mirror : 'a bt -> unit =
  fun t ->
  failwith "implement"

(** [add e t] adds [e] to [t]. 
    Precondition: [t] is a bst and [e] is not in [t].
*)
let add : 'a -> 'a bt -> unit =
  fun e t ->
  failwith "implement"


