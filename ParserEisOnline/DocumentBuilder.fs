namespace ParserEis

type DocResult<'a> =
    | Succ of 'a
    | Err of string

type DocumentBuilder() =
    member this.Bind(m, f) =
        match m with
        | Err e -> Err e
        | Succ a -> f a
    member this.Return(x) = Succ x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f
    member this.Zero() = Succ ""
    member this.Combine(a, b) =
        match a with
        | Succ a' -> b()
        | Err e -> Err e
    member this.Run(f) = f()