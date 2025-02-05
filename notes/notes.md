- mostly-weekly quizzes on wednesdays
  - one lowest quiz grade dropped
- one-page cheat sheet for mid/endterms

# OCaml
- if we run in the utop interpreter:
  ```
  utop # 1;;
  - : int = 1
  ```
  - hyphen (-): this variable has no name
  - type (int)
  - value (1)

## types, operations
- think like a business major, they use this language for trading
- float operations look like `+.`
- concatenation: `"hello" ^ "bye = "hellobye""`
- inequality conditional: `1 <> 1` = true
- equality conditional: `"h" = "h"` = true
- deep equality conditional: `"h" = "h"` = false
  - they are not the exact same object so it doesn't count
- inline if/then: `if x then y else z`
  - x must be a conditional; can't be cute and use 1 = true
  - y and z must be the same type
- top-level declaration: `let x = 1`
- local-scope declaration: `let x = 7 in x + x` 
  - i.e. x only =7 for that bit of code, then it returns to its global value or vanishes afterward
  - you can do this in c by just putting brackets to declare a local scope
- int, string, char, bool are *scalar* types
## functions
- functions are defined in modules, like Java packages/classes
  ```ocaml
  String.length "hello";;
  - : int = 5
  ```
  - "fully qualified name" = calling something with a bunch of dots
    ```ocaml
    Char.uppercase_ascii 'a';;
    A
    ```
- defining a function: `let f i = i+1`
  - f = name of function
  - i = formal parameter
  - i+1 = body
  - works like f(i) = i+1
  - utop tells us `val f : int -> int = <fun>`
    - i.e. f is a function that takes a parameter int and returns an int
- call a function: `f 2` returns 3
  - like f(2)
- if you do `f f 2`
  - ocaml reads f(f)(2)
  - which is not how we wrote f, and we probably won't
  - just write f (f 2)
- we will use built-in functions instead of constantly redefining them
- identity function: one that boils down to `let f i = i` which always returns its input
- constant function: one that boils down to `let f i = 3` which always returns the same value
- function of two arguments: `let f x y = x + y` ≡ `f(x, y) = x+y`
  - utop tells us `val f : int -> int -> int = <fun>`
  - which means int -> (int -> int) 
    - like how we know 8/4/2 = (8/4)/2 and nothing else
  - this means that f is a function that, given a number, returns a function that, given a number, returns a function
  - you can't give more arguments but you can give less and get a function back


## lists
- lists: `[1;2;3]`
  - head: `list.hd [1;2;3]` = 1
  - tail: `list.tl [1;2;3]` = 3
  - doing either on an empty list throws an exception
- order is specific
  - `[1;2;3] == [3;2;1]` returns false 
- the empty list is an arbitrary type; you can concatenate it to anything and vice versa
- list concatenation: `[1;2] @ [3;4]` = `[1;2;3;4]`
- if we have a function `let f i = 7;;`, what type is `i`?
  - if it's never used, then it can be any type
  - ocaml decides `val f : 'a -> int = <fun>`
  - that `a` is a type variable
- if we have a `let f i = i;;`, what type is `i` and what type is the function?
  - we get a weird type that we'll ignore.
  - we'll avoid them because why would you use this function
- a type that has type variables is called a polymorphic type
- an expression whose type is polymorphic is itself polymorphic
## tuples
- tuples
  - `(1,2)` = `int * int`
  - `(1,true)` = `int * bool`
  - `(1,(2, 3))` = `int * (int * int)`
  - `fst (1, true)` = `1`
  - `snd (1, true)` = `true`
- what type might `fst` be?
  - it takes a tuple obviously, but of type `'a * 'b`
  - since it's first we know it outputs type `'a`
  - fst is polymorphic because its output is a type variable
- what about `let f i = (i,i);;`
  - this is the delta function
  - it's `'a -> 'a * 'a`
- you can take a tuple as a parameter
  - `let f (x,y) = (y,x)`
    - type `'a * 'b = 'b * 'a`
## anonymous functions
- `let f i = if i 2 then 3 else 4`
  - i must be a function with type `int -> bool` because we're doing i(2) AND i(2) is in the conditional spot
  - this makes f a higher-order function that takes another function as its input
  - if we do `g i = i+1 > 0` and `f g` we get 3
  - instead of naming g, we can use an anonymous function
- anonymous function: `fun i -> i+1`
  - you can use it right away as `(fun i -> i+1) 2` = `3`
  - or you can store it is `let f  = fun i -> i+1`
    - this type will look the same as if you just did `let f i = i+1`
- what about `let f g i = g (g i)`?
  - we know f takes two arguments, so it's type is `type of g -> type of i -> type of f's output`
  - we don't do anything to i here that mandates its type
    - this means i is `'a`
  - g is a function but of what type?
    - its input must be `'a`, since we named that as i's type
    - its output must also be `'a` because we also feed g(i) back into g
    - therefore its type is `'a -> 'a`
  - f's output will be `'a` because it outputs whatever g outputs
  - therefore f's type is `('a -> 'a) -> 'a -> 'a`
  - f is polymorphic because at least one of its parameters is a type variable
  - it is also a higher-order function because it takes a a function as an argument
  - `f (fun i -> i*i) 2` = 16
  - `f (fun i -> i^"!") "hello"` = `"hello!!"`

## exercises
- define my_and and my_or functions
```ocaml
let my_and a b = if a then b else false 
let my_or a b = if a then true else j
```
- provide a
  - bool: `true`
  - int * int: `(1,2)`
  - bool -> int: `fun i -> if i then 3 else 7`
  - (int * int) -> bool: `fun i -> fst i - snd i > 5`
  - int -> (int -> int): `fun i j = i + j`
    - the parentheses are not necessary because the arrows are right-associative
  - (bool -> bool) * int: `((fun i -> i = true),1)`

## recursive functions
- recursive factorial:
  ```ocaml
  let rec fact n =
    match n with
    | 0 -> 1
    | m -> m * fact(m-1)
  ```
  - we must include `rec` or ocaml doesn't know it's recursive and complains
    - it needs to know to fix its scoping rules
  - match checks n against the left side of arrows
    - exactly like a java switch statement
  - if we run this on -1 it overflows the stack
  - let's fix it:
    ```ocaml
    let rec fact n =
      match n with
      | 0 -> 1
      | m when m > 0 -> m * fact (m-1)
      | _ -> failwith "fact: negative input"
    ```
    - the _ is the wildcard
    - we generally don't bother with this, we assume the user is competent
  - we'll instead tell the user:
    ```ocaml
    (* [fact n] computes the factorial of [n]
      Precondition: [n] is positive *)
    let rec fact n = ...
    ```
  - this is contract-based programming

### recursion on lists
- cons: `1 :: [2;3] = [1;2;3]`
  - `1 :: 2 :: 3 :: [] = [1;2;3]`
  - still right associative
- we use this to make a list with one element repeated
  ``` ocaml
  let rec repeat n e = 
    match n with 
    | 0 -> []
    | m -> e :: repeat (m-1) e
  ```
- you can turn infix operators into functions by putting them in parentheses, (+)
- length function:
  ```ocaml
  let rec length l : 'a list -> int =
    match l with
    | [] -> 0             (*tail*)
    | h::t -> 1+length t  (*head*)
  ```
  - the cons pattern here is a deconstructor, it deconstructs the list into head and tail
    - `h::t` is anything consecrated with anything; i.e. any head (first item) h connected to any tail (everything else) t
    - it's like racket
    - you can't match `[]` against `h::t` because it's got no head *or* tail. a list with one item is ok because it has a head (and an empty tail)
- check if list l contains element e
  ```ocaml
  let rec mem e l =
    match l with
    | [] -> false
    | h::t -> (h=e) || mem e t
  ```
  - this is short-circuit evaluation
    - we might use long-circuit to make sure that the rest of the statement doesn't fail
- remove duplicates
  ```ocaml
  let rec rem_dups l =
    match l with
    | [] -> []
    | h::t ->
      if mem h t
      then rem dups t
      else h:: rem_dups t

  let rec rem_dups' l =
    match l with
    | [] -> []
    | h::t when mem h t -> rem dups' t
    | h::t -> h:: rem_dups' t
  ```

### map
```ocaml
let rec upperl l =
  match l with
  | [] => []
  | h::t -> upper h :: upperl t
```
we can collapse this into:
```ocaml
let upperl' l = map upper l

let rec map : ('a -> 'b) -> 'a list -> 'b list =
  fun f l ->
  match l with
  | [] -> []
  | h::t -> f h :: map f t
```

### filter
``` ocaml
let rec fgtz l =
  match l with
  | [] -> []
  | h::t -> if h>0 then h::fgtz t
            else fgtz t
```
we can transform this into:
```ocaml
let is_positive h = h>0

let rec filter p l =
  match l with
  | [] -> []
  | h::t -> if p l then h::flter p t
            else filter p t
```
### fold
fold a list into a value by summing of all its elements
```ocaml
let rec suml l =
  match l with
  | [] -> 0
  | h::t -> h + suml t
```
do the same by taking the && of all its elements
```ocaml
let rec andl l =
  match l with
  | [] -> true
  | h::t -> h && andl t
```
- base case must be true because we're taking the &&; if it were false it would always return false
concat multiple lists that are within l:
```ocaml
let rec concat l =
  match l with
  | [] -> []
  | h::t -> h @ concat t
```
and our generic version looks like:
```ocaml
let rec foldr a f l =
  match l with
  | [] -> a
  | h::t -> f h (foldr a f t)
```
for
- base case a 
- binary function f
- list l to operate on
it has type `'b -> ('a -> 'b -> 'b) -> 'a list`

there's also the alternate version foldl:
```ocaml
let rec foldl a f l =
  match l with
  | [] -> a
  | h::t -> (f a h) f t
```
but we'll use foldr because it's slightly better

## variant types
define your own type:
```ocaml
type dow = Mo | Tu | We | Th | Fr | Sat | Su

let next d = 
  match d with
  | Mo -> Tu
  | Tu -> We
  ...

let is_weekend d =
  match d with 
  | Sa | Su -> true
  | _ -> false
```
it's very similar to a python enum
- Mo, Tu, etc are constructors that take no arguments
something more complicated:
```ocaml
type fla = Van | Cho | Str
type ic = Cup of fla | Cone of fla*fla | Bucket of fla list

let ic1 = Cup(cho)
let ic2 = Cone(Cho,Str)
let ic3 = Bucket([Van;Str;Van;Cho])

let cost i =
  match i with
  | Cup(_) -> 1
  | Cone(_,_) -> 2
  | Bucket(_) -> 5

let is_boring i =
  match i with
  | Cup(Van) -> true
  | Cone(Van,Van) -> true
  | Bucket l -> list.for_all ((=)Van) l
  | _ -> false
```

### dictionaries
```ocaml
let d_temp = [("nyc",32); ("bar harbour",10); ("anchorage",-1)]

(* the type of this version doesn't tell us that the operation can fail...*)
let rec_lookup_confusing d k = (* for dictionary d and key k*)
  match d with 
  | [] -> raise Not_found
  | (key, v)::t -> a
    if key=k
    then v
    else lookup t k

(* we'll use this to signal that the operation can fail *)

let 'a option = None | Some of 'a
(* this is built in *)

(* it's now type ('a * 'b) list -> 'a -> 'b option *)
(* the option part tells the user that the operation can fail *)
let rec_lookup d k = (* for dictionary d and key k*)
  match d with 
  | [] -> None
  | (key, v)::t -> a
    if key=k
    then Some v
    else lookup t k
```

## trees
given binary trees defined as
```ocaml
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt
```
we want to make
```
     33
    /  \
  22    77
       /
      44
```
so we'll do:
```ocaml
Node(33, 
    Node(22, Empty, Empty),
    Node(77,
      Node(44, Empty, Empty)
      Empty
    )
)
```

