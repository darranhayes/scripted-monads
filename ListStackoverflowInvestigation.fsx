type ContinuationMonad() =
    member __.Bind(m, f) = fun c -> m (fun a -> f a c)
    member __.Return(x) = fun k -> k x
    member this.Delay(mk) = fun c -> mk () c

let cont = ContinuationMonad()

type FList<'a> =
    | Empty
    | Cons of head: 'a * tail: FList<'a>

module FList =
    let ofSeq s =
        let folder s i =
            Cons(i, s)
        Seq.fold folder Empty s

    let reverse list =
        let rec loop list acc =
            match list with
            | Empty -> acc
            | Cons(head, tail) -> loop tail (Cons(head, acc))
        loop list Empty

    let length list =
        let rec loop list acc =
            match list with
            | Empty -> acc
            | Cons(_, tail) -> loop tail (1 + acc)
        loop list 0

    /// map, but throws a stackoverflow exception
    let rec map (fn: 'a -> 'b) list =
        match list with
        | Empty -> Empty
        | Cons (head, tail) ->
            Cons (fn head, map fn tail) // not tail recursive

    /// map, but with a reverse requiring accumulator for tail recursion
    let mapAcc (fn: 'a -> 'b) list =
        let rec loop fn list acc =
            match list with
            | Empty -> acc
            | Cons (head, tail) ->
                loop fn tail (Cons (fn head, acc)) // tail recursive
        loop fn list Empty |> reverse

    /// map, but with continuation passing style (k = continuation function)
    let mapK (fn: 'a -> 'b) list =
        let rec loop k list =
            match list with
            | Empty ->
                k Empty
            | Cons (head, tail) ->
                loop (fun acc -> k (Cons(fn head, acc))) tail // tail recursive
        loop id list

    /// map, but with tail recursive continuation monad magic
    let mapCont (fn: 'a -> 'b) list =
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
        loop list id

let testMapForStackoverflow fn =
    seq { 1..2_000_000 }
    |> FList.ofSeq
    |> fn ((+) 1)
    |> FList.length

// testMap FList.map     // stackoverflow exception
testMapForStackoverflow FList.mapAcc  // safe
testMapForStackoverflow FList.mapK    // safe
testMapForStackoverflow FList.mapCont // safe