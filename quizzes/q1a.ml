(* Quiz 1 

   Student name 1: Daniel Detore 
   Student name 2: Anthony Santilli

*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive. If your
    solution is not recursive, you can leave the "let rec" all the same.
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t when List.mem h l2 -> sub t l2
  | h::t -> h::sub t l2

let rec sub' l1 l2 = 
  List.filter (fun x -> not (List.mem x l2)) l1
    
(** [outgoing_nodes g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing_nodes ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []
  | (j,k)::t when j=n -> k::outgoing_nodes t n
  | h::t -> outgoing_nodes t n

let rec outgoing_nodes'' g n =
  List.map snd (List.filter (fun (src,tgt) -> src = n) g)

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)

let rec rem_dups l =
  match l with
  | [] -> []
  | h::t when mem h t -> rem_dups t
  | h::t -> h:: rem_dups t
let rec nodes g =
  let rec nodes_helper g =
    match g with
    | [] -> []
    | (src,tgt)::t -> src::tgt::nodes_helper t
  in
  rem_dups @@ (nodes_helper g)

(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
    Eg. degree ex => 2
*)

let rec maxl l =
  match l with
  | [] -> failwith "max: empty list"
  | [x] -> x
  | h::t -> max h (maxl t)
let rec degree g =
  maxl @@ List.map (fun n -> List.length @@ outgoing_nodes g n) (nodes g)

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  List.filter (fun (src,tgt) -> not (src = n || tgt = n)) g
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith "implement"

