# 1-22

- mostly-weekly quizzes on wednesdays
  - one lowest quiz grade dropped
- one-page cheat sheet for mid/endterms
- if we run in the utop interpreter:
  ```
  utop # 1;;
  - : int = 1
  ```

  - hyphen (-): this variable has no name
  - type (int)
  - value (1)

# 1-24
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

# 1-27
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
