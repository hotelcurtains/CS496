(* This file defines expressed values and environments *)
open Parser_plaf.Ast


(* expressed values and environments are defined mutually recursively *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*expr*env
  | PairVal of exp_val*exp_val
  | TupleVal of exp_val list
  (* string is the given name of this field *)
  (* bool is for whether this field is mutable or not *)
  (* exp_val is the expressed value of the field *)
  | RecordVal of (string*(bool*exp_val)) list 
  | UnitVal
  | RefVal of int
and
  env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env
  | ExtendEnvRec of string*string*expr*env


type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return (v:'a) : 'a ea_result =
  fun _env ->
  Ok v

let error (s:string) : 'a ea_result =
  fun _env ->
  Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv


let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let sequence (cs: ('a ea_result) list) : ('a list) ea_result  =
  let mcons p q = p >>= fun x -> q >>= fun y -> return (x::y)
  in List.fold_right mcons cs (return []) 

let mapM (f:'a -> 'b ea_result) (vs:'a list) : ('b list) ea_result =
   sequence (List.map f vs)



(* Operations on environments *)
let empty_env : unit -> env ea_result =
  fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v ->
  fun env ->
  Ok (ExtendEnv(id,v,env))


let extend_env_rec : string -> string -> expr -> env ea_result =
  fun id par body ->
  fun env  ->
  Ok (ExtendEnvRec(id,par,body,env))

let rec apply_env : string -> exp_val ea_result =
  fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail
  | ExtendEnvRec(v,par,body,tail) ->
    if id=v
    then Ok (ProcVal (par,body,env))
    else apply_env id tail                                              

let lookup_env : env ea_result =
  fun env ->
  Ok env



(* operations on expressed values *)
        
let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let pair_of_pairVal : exp_val -> (exp_val*exp_val) ea_result = function
  | PairVal(v1,v2) -> return (v1,v2)
  | _ -> error "Expected a pair!"

let tupleVal_to_list_of_evs: exp_val -> (exp_val list) ea_result = function
  | TupleVal(evs) -> return evs
  | _ -> error "Expected a tuple!"

let fields_of_recordVal: exp_val -> ((string*(bool*exp_val)) list) ea_result = function
  | RecordVal(fs) -> return fs
  | _ -> error "Expected a record!"

let clos_of_procVal : exp_val -> (string*expr*env) ea_result =
  fun ev ->
  match ev with
  | ProcVal(id,body,en) -> return (id,body,en)
  | _ -> error "Expected a closure!"          

let int_of_refVal =  function
  |  RefVal n -> return n
  | _ -> error "Expected a reference!"

let rec string_of_expval = function
  |  NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ProcVal (id,body,env) -> "ProcVal ("^ id ^","^string_of_expr
                               body^","^ string_of_env' []
                                             env^")"
  | PairVal(v1,v2) -> "PairVal("^string_of_expval
                        v1^","^string_of_expval v2^")"
  | TupleVal(evs) ->  "Tuple (" ^ String.concat "," (List.map
                                                   string_of_expval
                                                   evs)  ^ ")" 
  | UnitVal -> "UnitVal " 
  | RefVal i -> "RefVal ("^string_of_int i^")"
  | RecordVal(fs) -> "RecordVal("^ String.concat "," (List.map (fun (n,(b,ev)) ->
      n^ (if b then "<=" else "=") ^string_of_expval ev) fs) ^")"
and
   string_of_env' ac = function
  | EmptyEnv ->  "["^String.concat ",\n" ac^"]"
  | ExtendEnv(id,v,env) -> string_of_env' ((id^":="^string_of_expval v)::ac) env
  | ExtendEnvRec(id,param,body,env) -> string_of_env'
                                         ((id^":=Rec("^param^","^string_of_expr body^")")::ac) env
                                         
let string_of_env : string ea_result =
  fun env ->
  match env with
  | EmptyEnv -> Ok ">>Environment:\nEmpty"
  | _ -> Ok (">>Environment:\n"^ string_of_env' [] env)




