(**
@author Daniel Detore
I pledge my honor that I have abided by the Stevens Honor System.
@date 2/23/2025
*)

(* Our encoding of binary decision trees. *)
type dTree =
| Leaf of int
| Node of char * dTree * dTree

(* Left tree from Figure 1. *)
let tLeft = Node('w', 
  Node('x', Leaf(2), Leaf(5)),
  Leaf(8)
)

(* Right tree from Figure 1. *)
let tRight = Node('w', 
  Node('x', Leaf(2), Leaf(5)),
  Node('y', Leaf(7), Leaf(5))
)

(**
[height t] is dTree [t]'s height. A leaf's height is 1.
*)
let rec height t =
  match t with
  | Leaf(_) -> 1
  | Node(_,lt,rt) -> 1 + max (height lt) (height rt)

(**
[size t] is dTree [t]'s size. Each node and leaf have a size of 1.
*)
let rec size t =
  match t with
  | Leaf(_) -> 1
  | Node(_,lt,rt) -> 1 + size lt + size rt

(**
[paths t] is a list with all the paths to dTree [t]'s leaves. A path is a list 
of 0s and 1s ("left"s and "right"s, respectively) that lead to a leaf.
When applied to a leaf, it returns [[[]]].
*)
let rec paths t =
  match t with
  | Leaf(_) -> [[]]
  | Node(_,lt,rt) -> (List.map (fun x -> 0::x) (paths lt)) @ 
                     (List.map (fun x -> 1::x) (paths rt))

(**
[is_perfect t] is true if dTree [t] is perfect, i.e. all of its leaves
have the same depth, and false otherwise. A leaf is perfect.
*)
let rec is_perfect t =
  match t with
  | Leaf(_) -> true
  | Node(_,lt,rt) -> is_perfect lt && is_perfect rt && height lt = height rt

(**
[map f g t] is a new dTree resulting from dTree [t] where all inner nodes 
have [f : char -> char] applied and all leaves have [g : int -> int] applied.
*)
let rec map f g t =
  match t with
  | Leaf(x) -> Leaf(g x)
  | Node(d,lt,rt) -> Node(f d, map f g lt, map f g rt)


(**
[list_to_tree l] is a perfect dTree where each inner level n is filled with the
nth element of char list [l] and all leaves are 0.
*)
let rec list_to_tree l =
  match l with
  | [] -> Leaf(0)
  | h::t -> Node(h, list_to_tree t, list_to_tree t)

(**
[replace_leaf_at t f] returns a dTree resulting from [t] after replacing each
leaf addressed in the first element of a pair in [f] with the pair's second 
element.
*)
let rec replace_leaf_at t f =
  (* dTree -> (int list * int) -> dTree *)
  let rec replace_one t f =
    match t, f with
    | Leaf(_), ([], n) -> Leaf(n)
    | Node(d,lt,rt), (h::t1, n) when h = 0 -> Node(d, replace_one lt (t1, n), rt)
    | Node(d,lt,rt), (h::t1, n) when h = 1 -> Node(d, lt, replace_one rt (t1, n))
    | _, _ -> failwith "bad input"
  in
  match f with
  | [] -> t
  | h::tail -> replace_one t h