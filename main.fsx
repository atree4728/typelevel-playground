#r "./obj/Debug/net8.0/typelevel-playground.dll"

open Model
open Providers

[<Literal>]
let expr = "3 4 + 2 1 - *"

type Expr = RPNProvider<expr>

let value = Expr.Value
eval value
