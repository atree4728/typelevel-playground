module Model

[<Struct>]
type Ty<'Type> =
    override _.ToString() =
        let s =
            typeof<'Type>
                .ToString()
                .Replace("Model+", "")
                .Replace("[", "<")
                .Replace("]", ">")
                .Replace("`1", "")
                .Replace("`2", "")

        System.Text.RegularExpressions.Regex.Replace(s, @"FSI_\d{4}\+", "")


let inline ty< ^Type> : Ty< ^Type > = Ty()

let inline eval (x: Ty< ^A >) : Ty< ^B > =
    (^A: (static member inline eval: Ty< ^A > -> Ty< ^B >) x)

type Z = Z
    with

        static member eval(x: Ty<Z>) = x
        static member inline add(_: Ty<Z>, y) = eval y
        static member inline sub(x, _: Ty<Z>) = eval x
        static member inline mul(_: Ty<Z>, _) = ty<Z>


type S<'n> =
    | S of 'n

    static member inline eval(_: Ty<S< ^N >>) : _ when ^N: (static member eval: Ty< ^N > -> Ty< ^N' >) = ty<S< ^N' >>

    static member inline add
        (_: Ty<S< ^X >>, _: Ty< ^Y >)
        : _
              when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >)
              and ^X': (static member add: Ty< ^X' > * Ty< ^Y > -> Ty< ^Z >)
        =
        ty<S< ^Z >>

    static member inline sub
        (_: Ty<S< ^X >>, _: Ty<S< ^Y >>)
        : _
              when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >)
              and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >)
              and ^Y': (static member sub: Ty< ^X' > * Ty< ^Y' > -> Ty< ^Z >)
        =
        ty< ^Z>

    static member inline mul
        (_: Ty<S< ^X >>, _: Ty< ^Y >)
        : _
              when ^X: (static member mul: Ty< ^X > * Ty< ^Y > -> Ty< ^M >)
              and ^M: (static member add: Ty< ^M > * Ty< ^Y > -> Ty< ^Z >)
        =
        ty< ^Z>


type Add<'x, 'y> =
    | Add of 'x * 'y

    static member inline eval
        (_: Ty<Add< ^X, ^Y >>)
        : _ when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >) and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >) =
        (^X': (static member add: _ * _ -> _) ty< ^X'>, ty< ^Y'>)

type Sub<'x, 'y> =
    | Sub of 'x * 'y

    static member inline eval
        (_: Ty<Sub< ^X, ^Y >>)
        : _ when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >) and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >) =
        (^Y': (static member sub: _ * _ -> _) ty< ^X'>, ty< ^Y'>)

type Mul<'x, 'y> =
    | Mul of 'x * 'y

    static member inline eval
        (_: Ty<Mul< ^X, ^Y >>)
        : _ when ^X: (static member eval: Ty< ^X > -> Ty< ^X' >) and ^Y: (static member eval: Ty< ^Y > -> Ty< ^Y' >) =
        (^X': (static member mul: _ * _ -> _) ty< ^X'>, ty< ^Y'>)

type Three = Add<S<Z>, S<S<Z>>>
let three = eval ty<Three>
type Nine = Mul<S<S<S<Z>>>, Three>
let nine = eval ty<Nine>
let zero = eval ty<Mul<Nine, Z>>
let six = eval ty<Sub<Nine, Three>>
