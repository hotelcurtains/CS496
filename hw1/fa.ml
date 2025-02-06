(*
Daniel Detore
I pledge my honor that I have abided by the Stevens Honor System.
2/9/2025
*)


(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)
(**
   Code stub for assignment 1
*)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1")
               ; ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]
         }

let a4 = {states = ["q0";"q1";"q2";"q3"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1")
              ; ("q1",'c',"q2");  ("q0",'a',"q3")];
         final= ["q2";"q3"]
        }

let empty = {states = ["q0";"q1";"q2";"q3";"q4"];
            start = "q0";
            tf = [("q0",'a',"q1"); ("q1",'b',"q1")
                ; ("q1",'c',"q2");  ("q3",'a',"q4")];
            final= ["q4"]
            }

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]



(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

(* 
Applies the transition function f to the symbol sym,
assuming that the current state is st and the FA is deterministic. 
apply_transition_function : tf -> state -> symbol -> state option
*)
let rec apply_transition_function f st sym =
  match f with
  | [] -> None
  | (qi,s,qf)::t when qi = st && s = sym -> Some qf
  | _::t -> apply_transition_function t st sym

(* 
Check whether a word is accepted or recognized by a FA.
accept : fa -> input -> bool
*)
let rec accept fa word = 
  (* state option -> input -> state option *)
  let rec run qi word =
    match word with
    | [] -> qi
    | h::t -> 
      match apply_transition_function fa.tf qi h with
      | Some qf -> run qf t
      | None -> qi
  in 
  List.mem (run fa.start word) fa.final

(* 
Returns the list of all the states that are successors of the given state
with the given symbol.
next : tf -> state -> symbol -> state list
*)
let rec next tf q l =
  match tf with
  | [] -> []
  | (qi,s,qf)::t when qi = q && l = s -> qf::next t q l
  | _::t -> next t q l

(*
Checks whether the given automaton is deterministic or not.
is_deterministic : fa -> bool
*)
let is_deterministic fa =
  let rec check_states q = 
    match q with
    | [] -> true
    | (qi,s,qf)::t -> 
      List.for_all (fun (x,y,z) -> not (x=qi && y=s && z<>qf)) t 
      && check_states t
  in
  check_states fa.tf

(*
Checks is given fa is valid.
valid: fa -> bool
*)
let valid fa = 
  let rec has_dupes l =
    match l with
    | [] -> false
    | h::t -> List.mem h t || has_dupes t
  in
  not (has_dupes fa.states)
  && List.mem fa.start fa.states 
  && List.for_all (fun x -> List.mem x fa.states) fa.final
  && is_deterministic fa


(*
Returns list of states that are reachable from the start state.
fa -> state list
*)
let reachable fa = 
  let rec find_tfs tf q =
  match tf with
  | [] -> []
  | (qi,_,qf)::t when (qi=q) -> qf::find_tfs t qf
  | h::t -> find_tfs t q
  in 
  List.sort_uniq compare (find_tfs fa.tf fa.start)

(*
Checks whether a FA accepts at least one word.
non_empty: fa -> bool
*)
let non_empty fa =
  List.exists (fun x -> List.mem x fa.final) (reachable fa)

(*
Removes all unreachable (dead) states from a valid FA, also removing them from 
fa.states, removing transition functions that contain them, and fa.final.
remove_dead_states: fa -> fa
*)
let remove_dead_states fa = 
  let good = (reachable fa) 
  in
  {states = List.filter (fun x -> List.mem x good) fa.states;
   start = fa.start;
   tf = List.filter (fun (qi,_,qf) -> List.mem qi good || List.mem qf good) fa.tf;
   final = List.filter (fun x -> List.mem x good) fa.final}