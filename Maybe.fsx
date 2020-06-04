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

type CustomerRepository = int -> Maybe<{| Id : int; Age : Maybe<int> |}>

let showCustomerAge (customerRepo: CustomerRepository) strCustomerId =
    tryParseInt strCustomerId
    |> Maybe.bind customerRepo
    |> Maybe.bind (fun cust -> cust.Age)
    |> Maybe.map isOld
    |> (fun x ->
        match x with
        | Something x -> printfn "Is old: %O" x |> ignore
        | Nothing -> printfn "No age specified" |> ignore
    )

showCustomerAge (fun id -> Something {| Id = id; Age = Something id |}) "42"
showCustomerAge (fun id -> Something {| Id = id; Age = Something id |}) "35"
showCustomerAge (fun id -> Something {| Id = id; Age = Nothing |}) "42"
showCustomerAge (fun _ -> Nothing) "42"
