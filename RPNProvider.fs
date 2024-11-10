namespace DesignTime

open System.Reflection
open System.ComponentModel
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes
open Model

type Wrapper =
    static member inline ty<'Type>() = ty< ^Type>

[<AutoOpen>]
module Helper =
    let rec toType (n: uint) =
        match n with
        | 0u -> typeof<Z>
        | _ -> typedefof<S<_>>.MakeGenericType(toType (n - 1u))

    let parse (expr: string) =
        let tokens = expr.Split()
        let stack = System.Collections.Generic.Stack()

        for token in tokens do
            match token with
            | "+" ->
                if stack.Count < 2 then
                    failwith "Insufficient operands for '+'"

                let b = stack.Pop()
                let a = stack.Pop()
                let addType = typedefof<Add<_, _>>.MakeGenericType(a, b)
                stack.Push(addType)
            | "-" ->
                if stack.Count < 2 then
                    failwith "Insufficient operands for '-'"

                let b = stack.Pop()
                let a = stack.Pop()
                let addType = typedefof<Sub<_, _>>.MakeGenericType(a, b)
                stack.Push(addType)
            | "*" ->
                if stack.Count < 2 then
                    failwith "Insufficient operands for '*'"

                let b = stack.Pop()
                let a = stack.Pop()
                let mulType = typedefof<Mul<_, _>>.MakeGenericType(a, b)
                stack.Push(mulType)
            | nstr ->
                match System.UInt32.TryParse(nstr) with
                | (true, n) ->
                    let ntype = toType n
                    stack.Push(ntype)
                | _ -> failwithf "Invalid token: %s" token

        if stack.Count <> 1 then
            failwith "Invalid RPN expression"
        else
            stack.Pop()

[<EditorBrowsable(EditorBrowsableState.Never); TypeProvider>]
type RPNTypeProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)
    let asm = Assembly.GetExecutingAssembly()
    let ns = "Providers"
    let baseTy = typeof<obj>
    let prm = [ ProvidedStaticParameter("expr", typeof<string>) ]
    let t = ProvidedTypeDefinition(asm, ns, "RPNProvider", Some baseTy)

    do
        t.DefineStaticParameters(
            parameters = prm,
            instantiationFunction =
                (fun tyName prmValues ->
                    match prmValues with
                    | [| :? string as expr |] ->
                        let exprTy = parse expr
                        let tyTy = typedefof<Ty<_>>.GetGenericTypeDefinition()
                        let wrappedTy = tyTy.MakeGenericType(exprTy)

                        let method =
                            typeof<Wrapper>.GetMethod("ty", BindingFlags.Public ||| BindingFlags.Static)

                        let wrapped = Expr.Call(method.MakeGenericMethod(exprTy), [])

                        let providedTy =
                            ProvidedTypeDefinition(asm, ns, tyName, baseType = Some <@@ wrappedTy @@>.Type)

                        let valuet =
                            ProvidedProperty(
                                propertyName = "Value",
                                propertyType = wrappedTy,
                                isStatic = true,
                                getterCode = (fun _ -> Expr.Call(method.MakeGenericMethod(exprTy), []))
                            )

                        providedTy.AddMember valuet
                        providedTy

                    | _ -> failwith "unexpected parameter values")
        )

    do this.AddNamespace(ns, [ t ])

[<assembly: TypeProviderAssembly>]
do ()
