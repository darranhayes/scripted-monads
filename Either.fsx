open System

type Either<'e, 't> =
    | OK of 't
    | Failure of 'e

module Either =
    let map f m =
        match m with
        | OK t -> OK (f t)
        | Failure e -> Failure e

    let bind f m =
        match m with
        | OK t -> f t
        | Failure e -> Failure e

let print x =
    match x with
    | OK x -> printfn "OK: %O" x
    | Failure e -> printfn "Failure: %O" e

(* Pipeline style *)

let tryParseInt (x: string) =
    let result = Int32.TryParse(x)
    match result with
    | true, i -> OK i
    | false, _ -> Failure "Not a valid integer"

type Error = string
type Order = { Id: int; Total: int }
type Customer = { Id: int; MostRecentOrderId : int }
type CustomerRepository = int -> Either<Error, Customer>
type OrderRepository = int -> Either<Error, Order>

let getCustomerOrderTotal (customerRepo: CustomerRepository) (orderRepo : OrderRepository) strCustomerId =
    tryParseInt strCustomerId
    |> Either.bind customerRepo
    |> Either.map (fun c -> c.MostRecentOrderId)
    |> Either.bind orderRepo
    |> Either.map (fun o -> o.Total)

getCustomerOrderTotal (fun id -> OK { Id = id; MostRecentOrderId = 15 }) (fun id -> OK { Id = id; Total = 100 }) "1"
|> print

getCustomerOrderTotal (fun id -> OK { Id = id; MostRecentOrderId = 15 }) (fun _ -> Failure "Order not found" ) "1"
|> print

getCustomerOrderTotal (fun _ -> Failure "Customer not found") (fun id -> OK { Id = id; Total = 100 }) "1"
|> print

getCustomerOrderTotal (fun id -> OK { Id = id; MostRecentOrderId = 15 }) (fun id -> OK { Id = id; Total = 100 }) "hello world"
|> print

(* Computation expressions *)

type EitherBuilder() =
    member __.Bind(m, func) = Either.bind func m
    member __.Return(m) = OK m

let either = EitherBuilder()

either {
    let! id = tryParseInt "42"
    let! customer = OK { Id = id; MostRecentOrderId = 15 }
    let! order = OK { Id = customer.MostRecentOrderId; Total = 1000 }
    return order.Total
}
|> print

either {
    let! id = tryParseInt "42"
    let! _ = OK { Id = id; MostRecentOrderId = 15 }
    let! order = Failure "Order not found"
    return order.Total
}
|> print
