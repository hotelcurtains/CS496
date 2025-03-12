(**
@author Daniel Detore, Anthony Santilli
@date 3/9/2025
I pledge my honor that I have abided by the Stevens Honor System.
*)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Tuple(es) ->
    eval_exprs es >>= fun n ->
    return (TupleVal n)
  | Untuple (ids,e1,e2) -> 
    eval_expr e1 >>= 
    list_of_tupleVal >>= fun l ->
    if List.length ids = List.length l
      then extend_env_list ids l >>+
    eval_expr e2
  else error "extend_env_list: Arguments do not match parameters!"
  | EmptyList(_) -> 
    return (ListVal ([]))
    | Cons(e1,e2) ->
      eval_expr e1 >>= fun n1 -> 
      eval_expr e2 >>= 
      list_of_listVal >>= fun n2 ->
    return (ListVal (n1::n2))
  | Hd(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun n ->
    if n = [] then error "hd: List is empty!"
    else return (List.hd n)
  | Tl(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun n ->
    if n = [] then error "tl: List is empty!"
    else return (ListVal (List.tl n))
  | IsEmpty(e) ->
    eval_expr e >>=
    list_of_listVal >>= fun n ->
    return (BoolVal (n = []))
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> failwith "Not implemented yet!"
  and
  eval_exprs : expr list -> (exp_val list) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h :: t -> eval_expr h >>= fun i ->
  eval_exprs t >>= fun l ->
  return (i :: l)


(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


