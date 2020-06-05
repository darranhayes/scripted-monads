open System

type Maybe<'a> =
    | Something of 'a
    | Nothing

module Maybe =
    let map f m =
        match m with
        | Something x -> Something(f x)
        | Nothing -> Nothing

    let bind f m =
        match m with
        | Something x -> f x
        | Nothing -> Nothing

let print x =
    match x with
    | Something x -> printfn "Something: %O" x
    | Nothing -> printfn "Nothing"

(* Traditional long handed composition *)

let a = Something 3
let b = Something 4
let c = Something 10

Maybe.bind (fun x -> Maybe.map (fun y -> x + y) b) a
|> print

Maybe.bind (fun x -> Maybe.map (fun y -> Maybe.map (fun z -> x + y + z) c) b) a
|> print

(* Pipeline style *)

let tryParseInt (x: string) =
    let result = Int32.TryParse(x)
    match result with
    | true, i -> Something i
    | false, _ -> Nothing

let isOld age =
    match age with
    | x when x > 40 -> true
    | x -> false

type Customer = { Id: int; Age: int Maybe }

type CustomerRepository = int -> Customer Maybe

let getCustomerAge (customerRepo: CustomerRepository) strCustomerId =
    tryParseInt strCustomerId
    |> Maybe.bind customerRepo
    |> Maybe.bind (fun cust -> cust.Age)
    |> Maybe.map isOld

getCustomerAge (fun id -> Something { Id = id; Age = Something id }) "42"
|> print

getCustomerAge (fun id -> Something { Id = id; Age = Something id }) "35"
|> print

getCustomerAge (fun id -> Something { Id = id; Age = Nothing }) "42"
|> print

getCustomerAge (fun _ -> Nothing) "42"
|> print

(* Computation expressions *)

type MaybeBuilder() =
    member __.Bind(m, func) = Maybe.bind func m
    member __.Return(m) = Something m

let maybe = MaybeBuilder()

maybe {
    let! a = Something 5
    let! c = Something 10
    return (a + c)
}
|> print

maybe {
    let! a = Something 5
    let! b = Nothing
    let! c = Something 10
    return (a + b + c)
}
|> print

maybe {
    let! customerId = tryParseInt "33"
    let! customer = Something { Id = customerId; Age = Something customerId } // successful customer load
    return (customer.Age)
}
|> print

maybe {
    let! customerId = tryParseInt "33"
    let! customer = if (customerId > 40) then Something { Id = customerId; Age = Something customerId } else Nothing  // unsuccessful customer load
    return (customer.Age)
}
|> print
