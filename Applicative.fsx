open System

module Result =
    let apply fx fy =
        match fx, fy with
        | Ok f, Ok x -> Ok(f x)
        | Error ex, Ok _ -> Error ex
        | Ok _, Error ex -> Error ex
        | Error ex1, Error ex2 -> Error(List.concat [ ex1; ex2 ])

module Domain =
    type ValidationError =
        | EmptyId
        | EmptyString
        | InvalidAge

    type DateOfBirth =
        private { Day: int
                  Month: int
                  Year: int }

    type CustomerId = private | CustomerId of Guid
    type CustomerName = private | CustomerName of string
    type UserId = private | UserId of Guid

    type CreateCustomerCommand =
        private { Id: CustomerId
                  Name: CustomerName
                  DateOfBirth: DateOfBirth
                  UserId: UserId }

    let (<!>) = Result.map
    let (<*>) = Result.apply

    module Customer =
        let create guid =
            match guid with
            | g when g = Guid.Empty -> Error [ EmptyId ]
            | _ -> Ok(CustomerId guid)

    module User =
        let create guid =
            match guid with
            | g when g = Guid.Empty -> Error [ EmptyId ]
            | _ -> Ok(UserId guid)

    module Name =
        let create name =
            match name with
            | "" -> Error [ EmptyString ]
            | _ -> Ok(CustomerName name)

    module DateOfBirth =
        let create dob =
            match dob with
            | dt when dt >= DateTime(1900, 01, 01) ->
                Ok
                    { Day = dt.Day
                      Month = dt.Month
                      Year = dt.Year }
            | _ -> Error [ InvalidAge ]

    let createCustomerCommand id name dateOfBirth userId =
        let create' id' name' dateOfBirth' userId' =
            { Id = id'
              Name = name'
              DateOfBirth = dateOfBirth'
              UserId = userId' }

        create'
        <!> (Customer.create id)
        <*> (Name.create name)
        <*> (DateOfBirth.create dateOfBirth)
        <*> (User.create userId)

open Domain

match (createCustomerCommand Guid.Empty "" (DateTime(1800, 01, 01)) Guid.Empty) with
| Ok cmd -> printfn "OK! %O" cmd
| Error (xs) -> printfn "Errors: %O" xs

match (createCustomerCommand (Guid.NewGuid()) "John Doe" (DateTime(1972, 06, 15)) (Guid.NewGuid())) with
| Ok cmd -> printfn "OK! %O" cmd
| Error (xs) -> printfn "Errors: %O" xs
