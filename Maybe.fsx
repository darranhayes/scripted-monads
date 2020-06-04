type Maybe<'a> =
| Something of 'a
| Nothing

[<RequireQualifiedAccess>]
module Maybe =
    let map f m =
        match m with
        | Something x -> Something (f x)
        | Nothing -> Nothing

    let bind f m =
        match m with
        | Something x -> f x
        | Nothing -> Nothing

    type MaybeBuilder() =
        member x.Bind(comp, func) = bind func comp
        member x.Return(value) = Something value

let maybe = Maybe.MaybeBuilder()

open System

let tryParseInt (x : string) =
    let result = Int32.TryParse(x)
    match result with
    | true, i -> Something i
    | false, _ -> Nothing

let isOld age =
    match age with
    | x when x > 40 -> true
    | x -> false

let print x =
    match x with
    | Something x -> printfn "Something: %O" x
    | Nothing -> printfn "Nothing"

type CustomerRepository = int -> Maybe<{| Id : int; Age : Maybe<int> |}>

let showCustomerAge (customerRepo: CustomerRepository) strCustomerId =
    tryParseInt strCustomerId
    |> Maybe.bind customerRepo
    |> Maybe.bind (fun cust -> cust.Age)
    |> Maybe.map isOld

showCustomerAge (fun id -> Something {| Id = id; Age = Something id |}) "42" |> print
showCustomerAge (fun id -> Something {| Id = id; Age = Something id |}) "35" |> print
showCustomerAge (fun id -> Something {| Id = id; Age = Nothing |}) "42" |> print
showCustomerAge (fun _ -> Nothing) "42" |> print

// Something 5 + Something 3
Something 5
|> Maybe.bind (fun x -> Something 3 |> Maybe.map ((+) x))
|> print

// Something 5 + Nothing + Something 5
Something 5
|> Maybe.bind (fun x -> Nothing |> Maybe.map ((+) x))
|> Maybe.bind (fun x -> Something 5 |> Maybe.map ((+) x))
|> print

maybe {
    let! a = Something 5
    let! c = Something 10
    return (a + c)
} |> print

maybe {
    let! a = Something 5
    let! b = Nothing
    let! c = Something 10
    return (a + b + c)
} |> print
