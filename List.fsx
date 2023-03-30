
type FList<'a> =
    | Empty
    | Cons of head: 'a * tail: FList<'a>

module FList =
    let ofArray ([<System.ParamArray>] paramArray: 'a[]) =
        let rec listBuilder (arr: 'a array) index acc =
            match index with
            | 0 ->
                Cons(arr[0], acc)
            | idx ->
                listBuilder arr (idx - 1) (Cons(arr[idx], acc))
        listBuilder paramArray (paramArray.Length - 1) Empty

    let toString list =
        let rec render list acc =
            match list with
            | Empty ->
                acc
            | Cons (head, Empty) ->
                acc + head.ToString()
            | Cons (head, tail) ->
                render tail (acc + head.ToString() + "; ")
        sprintf "[ %s ]" (render list "")

    let head = function
        | Empty ->
            failwith "Cannot take the head of an empty list"
        | Cons (h, _) ->
            h

    let tail = function
        | Empty ->
            Empty
        | Cons (_, ts) ->
            ts

    let length list =
        let rec loop list acc =
            match list with
            | Empty ->
                acc
            | Cons(_, tail) ->
                loop tail (1 + acc)
        loop list 0

    let reverse list =
        let rec loop list acc =
            match list with
            | Empty ->
                acc
            | Cons(head, tail) ->
                loop tail (Cons(head, acc))
        loop list Empty

    let concat lista listb =
        let rec loop k lista listb =
            match lista, listb with
            | Empty, Empty ->
                k Empty
            | Empty, ys ->
                k ys
            | Cons(x, xs), ys ->
                loop (fun acc -> k (Cons(x, acc))) xs ys
        loop id lista listb

    let flatten lists =
        let rec loop k lists =
            match lists with
            | Empty ->
                k Empty
            | Cons(x, xs) ->
                loop (fun acc -> k (concat x acc)) xs
        loop id lists

    let map (fn: 'a -> 'b) list =
        let rec loop k list =
            match list with
            | Empty ->
                k Empty
            | Cons (head, tail) ->
                loop (fun acc -> k (Cons(fn head, acc))) tail
        loop id list

    let bind (fn: 'a -> FList<'b>) list =
        flatten (map fn list)

(* Computation Expressions *)

type FListBuilder() =
    member this.Return(x) =
        FList.Cons(x, FList.Empty)

    member this.ReturnFrom(x) =
        x

    member this.Yield(x) =
        this.Return(x)

    member this.YieldFrom(x) =
        x

    member this.Zero() =
        this.Return ()

    member this.Bind(m, fn) =
        FList.bind fn m

    /// accept FList.Cons(...) in for declarations
    member this.For(m: FList<_>, fn) =
        this.Bind(m, fn)

    /// accept sequences/ranges in for declarations
    member this.For(m: seq<_>, fn) =
        let list = FList.ofArray (Seq.toArray m)
        this.Bind(list, fn)

let flist = FListBuilder()

flist {
    let! x = Cons('a', Cons('b', Empty))
    let! y = Cons(1, Cons(2, Empty))
    let! z = [| '*'; '_'; '/' |] |> FList.ofArray
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

listc |> FList.length

let listd = Cons(100, Cons(200, Cons(300, Cons(400, Empty))))

let ll = Cons(lista, Cons(listb, Cons(listd, Empty)))
ll |> FList.flatten |> FList.reverse |> FList.toString