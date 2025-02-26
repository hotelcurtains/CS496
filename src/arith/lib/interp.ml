(**
Daniel Detore
Anthony Santilli
I pledge my honor that I have abided by the Stevens Honor System.
*)

open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser

let rec maxl l = 
  match l with
  | [] -> min_int
  | h::t -> max h (maxl t)
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Maxl(es) ->
    eval_exprs es >>= fun l ->
    if l = [] then Error "maxl: empty sequence"
    else return (maxl(l))
  | _ -> failwith "Not implemented yet!"
and
  eval_exprs : expr list -> (int list) result =
  fun es ->
  match es with
  | [] -> return []
  | h::t -> 
    eval_expr h >>= fun n ->
    eval_exprs t >>= fun m ->
    Ok (n::m)




(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog



