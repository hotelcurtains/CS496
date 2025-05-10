(*
  HW5
  CHECKED_PLUS type-checker implementation
  for references, pairs, lists, and trees

  NAMES: Anthony Santilli
         Daniel Detore

  I pledge my honor that I have abided by the Stevens Honor System.
*)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match declaration")

(* hw5: references*)

  | NewRef(e) -> chk_expr e >>= fun v -> return @@ RefType v
  | DeRef(e) -> chk_expr e >>= fun v ->
    (match v with
    | RefType va -> return va
    | _ -> error "deref: Expected a reference type"
    )
  | SetRef(e1,e2) -> 
    chk_expr e1 >>= fun v1 ->
      chk_expr e2 >>= fun v2 ->
        (match v1 with
        | RefType va1 when va1 = v2 -> return UnitType
        | RefType _ -> error "setref: Expected a reference type"
        | _ -> error "setref: Expected a reference type"
        )
  | BeginEnd([]) -> return UnitType
  | BeginEnd(es) ->
    chk_exprs es >>= fun fs ->
      return (List.hd (List.rev fs))

(* LISTS *)

  | EmptyList(t) -> (
    match t with
    | Some e -> return (ListType e)
    | _ -> error "emptylist: Expected a type"
  )
  | Cons(e1, e2) ->
    chk_expr e1 >>= fun h ->
    chk_expr e2 >>= fun t -> (
    match t with
    | ListType tl when h = tl -> return t
    | ListType _ -> error "cons: type of head and tail do not match"
    | _ -> error "cons: Expected ListType"
  )

  | IsEmpty(e) -> chk_expr e >>= fun l -> (
    match l with
    | ListType _ -> return BoolType
    | TreeType _ -> return BoolType
    | _ -> error "isempty: Expected either a List or Tree"
  )
  | Hd(e) -> chk_expr e >>= fun l -> (
    match l with
    | ListType h -> return h
    | _ -> error "hd: Expected ListType"
  )
  | Tl(e) -> chk_expr e >>= fun l -> (
    match l with
    | ListType(_) -> return l
    | _-> error "tl: Expected ListType"
  )


(* TREES *)
  | EmptyTree(t) -> (
    match t with
    | Some l -> return (TreeType l)
    | None -> error "emptytree: type definition missing"
  )
  | Node(de, le, re) ->
    chk_expr le >>= fun l ->
    chk_expr re >>= fun r ->
    chk_expr de >>= fun p ->
    if (TreeType p = l && TreeType p = r)
    then return (TreeType p)
    else error "node: mismatched data between parent and children"
  | CaseT(target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= fun t ->(
    match t with
    | TreeType(tt) -> 
      extend_tenv id1 tt >>+
      extend_tenv id2 (TreeType tt) >>+
      extend_tenv id3 (TreeType tt) >>+
      chk_expr nodecase >>= fun nt ->
      chk_expr emptycase >>= fun et ->
      if (nt = et) 
      then chk_expr emptycase
      else error "caseT: Mismatched branch types"
    | _ -> error "caseT: Expected target to be a tree"
  )
    

    
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_prog (AProg(_,e)) =
  chk_expr e

and (* From our eval_exprs in the let HW assignment *)
chk_exprs =
fun es ->
match es with
| [] -> return []
| h :: t -> chk_expr h >>= fun i ->
chk_exprs t >>= fun l ->
return (i :: l)

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



