#load "Continuation.fsx"
open Continuations

type FList<'a> =
    | Empty
    | Cons of head: 'a * tail: FList<'a>

module FList =
    let ofSeq (sequence: seq<'a>) : FList<'a> =
        let folder s i =
            Cons(i, s)

        Seq.fold folder Empty sequence

    let rec toSeq (list: FList<'a>) : seq<'a> =
        seq {
            match list with
            | Empty ->
                yield! Seq.empty
            | Cons(x, xs) ->
                yield x
                yield! toSeq xs
        }

    let head (list: FList<'a>) : 'a =
        match list with
        | Empty ->
            failwith "Cannot take the head of an empty list"
        | Cons (x, _) ->
            x

    let tail (list: FList<'a>) : FList<'a> =
        match list with
        | Empty ->
            Empty
        | Cons (_, xs) ->
            xs

    let fold (f: 'State -> 'T -> 'State) (state: 'State) (list: FList<'T>) : 'State =
        let rec loop state list =
            match list with
            | Empty ->
                state
            | Cons(x, xs) ->
                loop (f state x) xs
        loop state list

    let foldBack (f: 'T -> 'State -> 'State) (list: 'T FList) (state: 'State) : 'State =
        let rec loop list =
            cont {
                match list with
                | Empty ->
                    return state
                | Cons(x, xs) ->
                    let! newState = loop xs
                    return f x newState
            }
        loop list |> Cont.eval

    let rec reduce (f: 'a -> 'a -> 'a) (list: FList<'a>) : 'a =
        match list with
        | Empty ->
            failwith "Cannot reduce an empty list"
        | Cons(x, Empty) ->
            x
        | Cons(x, xs) ->
            fold f x xs

    let rec tryReduce (f: 'a -> 'a -> 'a) (list: FList<'a>) : 'a option =
        match list with
        | Empty ->
            None
        | _ ->
            reduce f list |> Some

    let min (list: FList<'a>) : 'a =
        reduce min list

    let max (list: FList<'a>) : 'a =
        reduce max list

    let length (list: FList<'a>) : int =
        let inc length = fun _ -> length + 1
        fold inc 0 list

    let reverse (list: FList<'a>) : FList<'a> =
        let cons s i = Cons(i, s)
        fold cons Empty list

    let concat (list1: FList<'a>) (list2: FList<'a>) : FList<'a> =
        let cons i s = Cons(i, s)
        foldBack cons list1 list2

    let flatten (lists: FList<FList<'a>>) : FList<'a> =
        foldBack concat lists Empty

    let map (fn: 'a -> 'b) (list: FList<'a>) : FList<'b> =
        let rec loop list =
            cont {
                match list with
                | Empty ->
                    return Empty
                | Cons (x, xs) ->
                    let y = fn x
                    let! ys = loop xs
                    return Cons(y, ys)
            }

        loop list |> Cont.eval

    let bind (fn: 'a -> FList<'b>) (list: FList<'a>) : FList<'b> =
        flatten (map fn list)

    let toString (list: FList<'a>) : string =
        list
        |> map (fun i -> i.ToString())
        |> toSeq
        |> String.concat "; "
        |> sprintf "[ %s ]"

    (* Computation Expression *)
    type Builder() =
        member this.Return(x) =
            Cons(x, Empty)

        member this.ReturnFrom(x) =
            x

        member this.Yield(x) =
            this.Return(x)

        member this.YieldFrom(x) =
            x

        member this.Zero() =
            this.Return ()

        member this.Bind(m, fn) =
            bind fn m

        /// accept FList.Cons(...) in for declarations
        member this.For(m: FList<_>, fn) =
            this.Bind(m, fn)

        /// accept sequences/ranges in for declarations
        member this.For(m: seq<_>, fn) =
            let list = ofSeq m
            this.Bind(list, fn)

let flist = FList.Builder()

flist {
    let! x = Cons('a', Cons('b', Empty))
    let! y = Cons(1, Cons(2, Empty))
    let! z = [| '*'; '_'; '/' |] |> FList.ofSeq
    return (x, y, z)
}

flist {
    for i in Cons(1, Cons(2, Cons(3, Empty))) do
        yield i * 10
}

flist {
    for i in 0..2..10 do
        yield i * 10
}

flist {
    for i in 1..3 do
        for j in 10..10..20 do
            yield i * j
}

flist {
    for i in 0..5..10 do
        yield! Cons(i + 5, Cons(i + 6, Empty))
} |> FList.toString

let lista = Cons(1, Cons(2, Cons(3, Empty)))
let listb = lista |> FList.map (fun x -> x + 10)
let listc = FList.concat lista listb

listc |> FList.toSeq

listc |> FList.length

let listd = Cons(100, Cons(200, Cons(300, Cons(400, Empty))))

let listSum =
    Cons(100, Cons(200, Cons(300, Cons(400, Empty))))
    |> FList.fold (+) 0


let ll = Cons(lista, Cons(listb, Cons(listd, Empty)))
ll |> FList.flatten |> FList.reverse |> (FList.map (fun x -> x * 3)) |> FList.toString

let random =
    let r = System.Random()
    fun () -> r.Next()

let la = seq { for i in 1..500_000 -> random () } |> FList.ofSeq

let lb = la |> FList.map (fun i -> i / 2)

let lc = la |> FList.map (fun i -> i / 3)

let ld =  FList.concat lb lc

ld |> FList.min
ld |> FList.max
ld |> FList.length