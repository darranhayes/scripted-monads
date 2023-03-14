
type FList<'a> =
    | Empty
    | Cons of head: 'a * tail: FList<'a>

module FList =
    let ofArray ([<System.ParamArray>] paramArray: 'a[]) =
        let rec listBuilder (arr: 'a array) index acc =
            match index with
            | 0 -> Cons(arr[0], acc)
            | idx -> listBuilder arr (idx - 1) (Cons(arr[idx], acc))
        listBuilder paramArray (paramArray.Length - 1) Empty

    let toString list =
        let rec render list acc =
            match list with
            | Empty -> acc
            | Cons (head, Empty) -> acc + head.ToString()
            | Cons (head, tail) -> render tail (acc + head.ToString() + "; ")
        sprintf "[ %s ]" (render list "")

    let head list =
        match list with
        | Empty -> failwith "Cannot take the head of an empty list"
        | Cons (i, l) -> i

    let tail list =
        match list with
        | Empty -> Empty
        | Cons (_, l) -> l

    let length list =
        let rec length' list acc =
            match list with
            | Empty -> acc
            | Cons(head, tail) -> length' tail (1 + acc)
        length' list 0

    let reverse list =
        let rec reverse' list acc =
            match list with
            | Empty -> acc
            | Cons(head, tail) -> reverse' tail (Cons(head, acc))
        reverse' list Empty

    let concat lista listb =
        let rec concat' lista listb acc =
            match lista, listb with
            | Empty, Empty -> acc
            | Empty, ys -> concat' ys Empty acc
            | Cons(x, xs), ys -> concat' xs ys (Cons(x, acc))
        concat' lista listb Empty |> reverse

    let flatten lists =
        let rec flatten' lists acc =
            match lists with
            | Empty -> acc
            | Cons(x, xs) -> flatten' xs (concat x acc)
        flatten' (lists |> reverse) Empty

    let map (fn: 'a -> 'b) list =
        let rec map' fn list acc =
            match list with
            | Empty -> acc
            | Cons (head, tail) ->
                map' fn tail (Cons (fn head, acc))
        map' fn list Empty |> reverse

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