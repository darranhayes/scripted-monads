module Continuations

type Cont<'a, 'b> =
    Cont of (('a -> 'b) -> 'b)

module Cont =
    let ret (x: 'a) : Cont<'a, 'b> =
        Cont(fun k -> k x)

    let run (Cont m) (k: 'a -> 'b) : 'b =
        m k

    let delay f =
        Cont(fun k -> run (f ()) k)

    let bind (f: 'a-> Cont<'a, 'b>) (Cont m) : Cont<'a, 'b> =
        Cont(fun k -> m (fun a -> run (f a) k))

    let eval (Cont m) : 'a =
        m id

    type Builder() =
        member this.Return (f: 'a) : Cont<'a, 'b> =
            ret f

        member this.ReturnFrom f =
            f

        member this.Delay (f: unit -> Cont<'a, 'b>) : Cont<'a, 'b> =
            delay f

        member this.Bind (m: Cont<'a,'b>, f: 'a -> Cont<'a, 'b>) : Cont<'a, 'b> =
            bind f m

let cont = Cont.Builder()

let map (f: 'a -> 'b) (list: List<'a>) : List<'b> =
    let rec loop list =
        cont {
            match list with
            | [] ->
                return []
            | x::xs ->
                let y = f x
                let! ys = loop xs
                return y::ys
        }

    loop list
    |> Cont.eval

[1..500_000] |> map (fun x -> x * x)
// [1..10] |> map (fun x -> x * x)