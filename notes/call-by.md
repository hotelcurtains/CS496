what do we get when we call...
```ocaml
utop # interp "let a=2
in let f = proc (x) { proc (y) { x+y+debug(3) } }
in ((f a) (a+3))";;
```
in a language that uses...

# Call by value (default)
```ocaml
>>Environment:
[a:=RefVal (0),
x:=RefVal (2),
y:=RefVal (3)]

>>Store:
0->NumVal 2,
1->ProcVal (x,Proc(y,Add(Add(Var x,Var y),Debug(Int 3))),[a:=RefVal (0)]),
2->NumVal 2,
3->NumVal 5
- : exp_val Implicit_refs.Ds.result = Error "Debug called"
```

# Call by name
```ocaml
Environment:
(a,RefVal (0))
(x,RefVal (0))
(y,RefVal (2))

>>Store:
0->NumVal 2,
1->ProcVal (x,Proc(y,Add(Add(Var x,Var y),Debug(Int 3))),(a,RefVal (0))),
2->Thunk(Add(Var a,Int 3),(a,RefVal (0))(f,RefVal (1)))
- : exp_val Cbname.Ds.result = Error "Reached breakpoint"
```

# Call by need
```ocaml
Environment:
(a,RefVal (0))
(x,RefVal (0))
(y,RefVal (2))

>>Store:
0->NumVal 2,
1->ProcVal (x,Proc(y,Add(Add(Var x,Var y),Debug(Int 3))),(a,RefVal (0))),
2->NumVal 5
- : exp_val Cbneed.Ds.result = Error "Reached breakpoint"
```

# Call by reference
```ocaml
Environment:
(a,RefVal (0))
(x,RefVal (0))
(y,RefVal (2))

>>Store:
0->NumVal 2,
1->ProcVal (x,Proc(y,Add(Add(Var x,Var y),Debug(Int 3))),(a,RefVal (0))),
2->NumVal 5
- : Cbref.Ds.exp_val Cbref.Ds.result = Cbref.Ds.Error "Reached breakpoint"
```