open System

let inline (>>=) x f = Option.bind f x // bind
let inline (>=>) f g = f >> Option.bind g // Kleisli composition

type CustomerId = CustomerId of int
type DateOfBirth = { Year: int; Month: int; Day: int }

type Customer =
    { Id: CustomerId
      Name: string
      DateOfBirth: DateOfBirth option }

let tryParseInt (str: string) =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | false, _ -> None

let tryConvertToId (i: int) = Some(CustomerId i)

let tryLoadCustomer (id: CustomerId) =
    Some
        ({ Id = id
           Name = "Customer's Name"
           DateOfBirth = Some { Day = 5; Month = 10; Year = 1964 } })

let tryGetDateOfBirth (customer: Customer) = customer.DateOfBirth

let getCustomerDateOfBirth =
    tryParseInt
    >=> tryConvertToId
    >=> tryLoadCustomer
    >=> tryGetDateOfBirth

let getCustomerDateOfBirth' (str: string) =
    Some str
    >>= tryParseInt
    >>= tryConvertToId
    >>= tryLoadCustomer
    >>= tryGetDateOfBirth
