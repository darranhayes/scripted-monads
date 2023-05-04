#load "Continuation.fsx"
open Continuation

type FList<'a> =
    | Empty
    | Cons of head: 'a * tail: FList<'a>

[<RequireQualifiedAccessAttribute>]
module rec FList =
    let ret x =
        Cons(x, Empty)

    let cons x xs =
        Cons(x, xs)

    let ofSeq (sequence: seq<'a>) : FList<'a> =
        let folder i s =
            Cons(i, s)

        Seq.foldBack folder sequence Empty

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

    let fold (f: 'State -> 'a -> 'State) (initialState: 'State) (list: FList<'a>) : 'State =
        let rec loop state list =
            match list with
            | Empty ->
                state
            | Cons(x, xs) ->
                loop (f state x) xs
        loop initialState list

    let foldBack (f: 'a -> 'State -> 'State) (list: FList<'a>) (initialState: 'State) : 'State =
        let rec loop list =
            cont {
                match list with
                | Empty ->
                    return initialState
                | Cons(x, xs) ->
                    let! newState = loop xs
                    return f x newState
            }
        loop list |> Cont.eval

    let unfold (generator: 'State -> ('a * 'State) option) (initialState: 'State) : FList<'a> =
        let rec loop state =
            cont {
                match generator state with
                | Some (i, s) ->
                    let! xs = loop s
                    return Cons(i, xs)
                | None ->
                    return Empty
            }
        loop initialState |> Cont.eval

    let length (list: FList<'a>) : int =
        let inc length = fun _ -> length + 1
        fold inc 0 list

    let reverse (list: FList<'a>) : FList<'a> =
        let cons s i = Cons(i, s)
        fold cons Empty list

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
        reduce Operators.min list

    let max (list: FList<'a>) : 'a =
        reduce Operators.max list

    let scan (f: 'State -> 'a -> 'State) (initialState: 'State) (list: FList<'a>) : FList<'State> =
        let rec loop state list =
            cont {
                match list with
                | Empty ->
                    return ret state
                | Cons(x, xs) ->
                    let! ys = loop (f state x) xs
                    return (Cons(state, ys))
            }
        loop initialState list |> Cont.eval

    let scanBack (f: 'a -> 'State -> 'State) (list: 'a FList) (initialState: 'State) : FList<'State> =
        let rec loop list state =
            cont {
                match list with
                | Empty ->
                    return ret initialState
                | Cons(x, xs) ->
                    let! ys = loop xs state
                    let q = head ys
                    let y = f x q
                    return (Cons(y, ys))
            }
        loop list initialState |> Cont.eval

    let concat (list1: FList<'a>) (list2: FList<'a>) : FList<'a> =
        foldBack cons list1 list2

    let (@) = concat
    let (^+) x xs = cons x xs

    let tryTake (n: int) (list: FList<'a>) : FList<'a> option =
        let rec loop list i =
            cont {
                match list with
                // input list is too short
                | Empty when i < n ->
                    return None
                // input list is the right length
                | Empty ->
                    return Some Empty
                // still taking elements, add to output list
                | Cons(x, xs) when i < n ->
                    let! ys = loop xs (i + 1)
                    return Option.map (cons x) ys
                // finish taking elements
                | _ ->
                    return Some Empty
            }
        loop list 0 |> Cont.eval

    let trySkip (n: int) (list: FList<'a>) : FList<'a> option =
        let rec loop list i : FList<'a> option =
            match list with
            // input list is too short
            | Empty ->
                None
            // still skipping items
            | Cons(_, xs) when i < n ->
                loop xs (i + 1)
            // finished skipping items, return remaining list items
            | xs ->
                xs |> Some
        loop list 0

    let flatten (lists: FList<FList<'a>>) : FList<'a> =
        foldBack concat lists Empty

    let filter (predicate: 'a -> bool) (list: FList<'a>) : FList<'a> =
        let rec loop list =
            cont {
                match list with
                | Empty ->
                    return Empty
                | Cons(x, xs) ->
                    if predicate x then
                        let! ys = loop xs
                        return Cons(x, ys)
                    else
                        return! loop xs
            }
        loop list |> Cont.eval

    let partition (predicate: 'a -> bool) (list: FList<'a>) : FList<'a> * FList<'a> =
        let rec loop list (accLeft) (accRight) =
            match list with
            | Empty ->
                accLeft, accRight
            | Cons(x, xs) ->
                if predicate x then
                    loop xs (Cons(x, accLeft)) accRight
                else
                    loop xs accLeft (Cons(x, accRight))
        loop list Empty Empty

    let itemAt (index: int) (list: FList<'a>) : 'a =
        let rec loop i l =
            match l with
            | Empty -> failwith "index must be less than the length of the list"
            | Cons(x, xs) ->
                if i < index then
                    loop (i + 1) xs
                else
                    x
        loop 0 list

    let insertAt (index: int) (item: 'a) (list: FList<'a>) : FList<'a> =
        let rec loop i l =
            match l with
            | Empty ->
                if i = index then
                    Cons(item, Empty)
                else
                    failwith "index must be less than the length of the list"
            | Cons(x, xs) ->
                if i = index then
                    Cons(item, Cons(x, xs))
                else
                    Cons(x, loop (i + 1) xs)
        loop 0 list

    let binarySearch (item: 'a) (list: FList<'a>) : int =
        let rec findBetween low high =
            if low > high then
                ~~~low
            else
                let mid = (low + high) / 2
                match compare item (itemAt mid list) with
                | 0 -> mid
                | x when x < 0 -> findBetween low (mid - 1)
                | x when x > 0 -> findBetween (mid + 1) high
                | _ -> failwith "unexpected error"

        findBetween 0 (length list)

    let private sort' (predicate: 'a -> 'a -> bool) (list: FList<'a>) : FList<'a> =
        let rec quicksort list =
            match list with
            | Empty ->
                Empty
            | Cons(x, xs) ->
                let left, right =
                    xs
                    |> partition (predicate x)
                quicksort left @ ret x @ quicksort right
        quicksort list

    let sort (list: FList<'a>) : FList<'a> =
        sort' (fun x i -> i <= x) list

    let sortDescending (list: FList<'a>) : FList<'a> =
        sort' (fun x i -> i > x) list

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
            ret x

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

    /// Zipper is a faster performing wapper structure that enables imperative left/right
    /// movements along a list and to make in place modifications without compromising
    /// on immutability and without having to traverse the list from head every time
    type Zipper<'a> = {
        Left: FList<'a>
        LeftLength: int
        Right: FList<'a>
        RightLength: int
    }

    module Zipper =
        let zipper (l: FList<'a>) (r: FList<'a>) =
            { Left = l; LeftLength = FList.length l; Right = r; RightLength = FList.length r }

        let fromList list =
            zipper FList.Empty list

        let left (z: Zipper<'a>) : Zipper<'a> option =
            // take first item from left, cons onto right
            match z with
            | { Left = Empty } ->
                None
            | { Left = Cons(x, xs); LeftLength = ll; Right = right; RightLength = rl } ->
                Some { Left = xs; LeftLength = ll - 1; Right = Cons(x, right); RightLength = rl + 1 }

        let right (z: Zipper<'a>) : Zipper<'a> option =
            // take first item from right, cons onto left
            match z with
            | { Right = Empty } ->
                None
            | { Left = left; LeftLength = ll; Right = Cons(x, xs); RightLength = rl } ->
                Some { Left = Cons(x, left); LeftLength = ll + 1; Right = xs; RightLength = rl - 1 }

        let item (z: Zipper<'a>) : 'a option =
            match z with
            | { Right = Empty } ->
                None
            | { Right = Cons(x, _) } ->
                Some x

        let setItem (value: 'a) (z: Zipper<'a>) : Zipper<'a> =
            match z with
            | { Right = Empty } ->
                { z with Right = Cons(value, Empty); RightLength = z.RightLength + 1 }
            | { Right = Cons(_, xs) } ->
                { z with Right = Cons(value, xs) }

        let length (z: Zipper<'a>) : int =
            z.LeftLength + z.RightLength

        let moveTo (n: int) (z: Zipper<'a>) : Zipper<'a> =
            if n < 0 || n >= length z then
                failwithf "Position must be greater than zero and less than length of the list. n = %i" n
            else
                let currentPosition = z.LeftLength
                let moveFn, times =
                    if n < currentPosition then
                        left, currentPosition - n
                    else
                        right, n - currentPosition

                let rec loop i zz =
                    if i = times then
                        zz
                    else
                        loop (i + 1) (moveFn zz |> Option.get)

                loop 0 z

        let toList (z: Zipper<'a>) : FList<'a> =
            let zz = z |> moveTo 0
            zz.Right

let (@) = FList.concat
let (^+) x xs = FList.cons x xs

let flist = FList.Builder()

// flist {
//     let! x = Cons('a', Cons('b', Empty))
//     let! y = 1 ^+ 2 ^+ Empty
//     let! z = [| '*'; '_'; '/' |] |> FList.ofSeq
//     return (x, y, z)
// }

// flist {
//     for i in Cons(1, Cons(2, Cons(3, Empty))) do
//         yield i * 10
// }

// flist {
//     for i in 0..2..10 do
//         yield i * 10
// }

// flist {
//     for i in 1..3 do
//         for j in 10..10..20 do
//             yield i * j
// }

// flist {
//     for i in 0..5..10 do
//         yield! i + 5 ^+ i + 6 ^+ Empty
// } |> FList.toString

// let lista = Cons(1, Cons(2, Cons(3, Empty)))
// let listb = lista |> FList.map (fun x -> x + 10)
// let listc = FList.concat lista listb

// listc |> FList.toSeq

// listc |> FList.length

// let listd = 100 ^+ 200 ^+ 300 ^+ 400 ^+ Empty

// let listSum =
//     100 ^+ 200 ^+ 300 ^+ 400 ^+ Empty
//     |> FList.fold (+) 0


// let ll = lista ^+ listb ^+ listd ^+ Empty
// ll |> FList.flatten |> FList.reverse |> (FList.map (fun x -> x * 3)) |> FList.toString

// let random =
//     let r = System.Random()
//     fun () -> r.Next()

// let la = seq { for i in 1..1000 -> random () } |> FList.ofSeq

// let lb = la |> FList.map (fun i -> i / 2)

// let lc = la |> FList.map (fun i -> i / 3)

// let ld =  FList.concat lb lc

// ld |> FList.min
// ld |> FList.max
// ld |> FList.length
// ld |> FList.sort |> FList.toSeq

// seq { 20..-1..1 } |> FList.ofSeq |> FList.sort
// seq { 20..-1..1 } |> FList.ofSeq |> FList.sortDescending

// let transactions =
//     seq {
//         -100.00
//         450.34
//         -62.34
//         -127.00
//         -13.50
//         -12.92
//     }
//     |> FList.ofSeq

// FList.fold (+) 1122.73 transactions
// FList.scan (+) 1122.73 transactions |> FList.toSeq |> Seq.toArray

// type Charge =
//     | In of int
//     | Out of int

// let inputs =
//     [ In 1; Out 2; In 3 ]
//     |> FList.ofSeq

// FList.scanBack (fun charge acc ->
//     match charge with
//     | In i -> acc + i
//     | Out o -> acc - o) inputs 0

// FList.scanBack (+) (seq { 1; 2;3 } |> FList.ofSeq) 0

// seq { 1 .. 1000 }
// |> FList.ofSeq
// |> FList.trySkip 500
// |> Option.bind (FList.tryTake 5)

let xonacci takeN initialState =
    let generator (list, total) =
        if total <= takeN then
            let x = FList.head list
            let xs = FList.tail list
            let sum = FList.reduce (+) list
            let newList =
                xs @ (FList.ret sum)
            let newTotal = total + 1
            Some (x, (newList, newTotal))
        else
            None
    FList.unfold generator (initialState, 0)

0 ^+ 1 ^+ Empty |> xonacci 30 |> FList.toString |> printfn "%s"
0 ^+ 0 ^+ 0 ^+ 0 ^+ 1 ^+ Empty |> xonacci 30 |> FList.toString |> printfn "%s"

let list = 0 ^+ 0 ^+ 0 ^+ 0 ^+ 1 ^+ Empty |> xonacci 30

let index = FList.binarySearch 62 list
FList.insertAt (~~~index) 62 list

0 ^+ 1 ^+ Empty |> xonacci 30 |> FList.toSeq |> Seq.toArray

