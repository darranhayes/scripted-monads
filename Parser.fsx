// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

let run (parser: Parser<'a>) (input: string) : ParseResult<'a * string> =
    let (Parser innerFn) = parser
    innerFn input

let andThen (parser1: Parser<'a>) (parser2: Parser<'b>) : Parser<'a * 'b> =
    let innerFn input =
        match run parser1 input with
        | Failure err ->
            Failure err
        | Success (value1, remaining1) ->
            match run parser2 remaining1 with
            | Failure err ->
                Failure err
            | Success (value2, remaining2) ->
                let newValue = value1, value2
                Success (newValue, remaining2)

    Parser innerFn

let orElse (parser1: Parser<'a>) (parser2: Parser<'a>) : Parser<'a> =
    let innerFn input =
        match run parser1 input with
            | Success v ->
                Success v
            | Failure _ ->
                run parser2 input

    Parser innerFn

let mapP (f: 'a -> 'b) (parser: Parser<'a>) : Parser<'b> =
    let innerFn input =
        match run parser input with
        | Success (value1, remaining1) ->
            let value2 = f value1
            Success (value2, remaining1)
        | Failure err ->
            Failure err

    Parser innerFn

/// Lift value x into a parser
let returnP (x: 'a) : Parser<'a> =
    let innerFn input =
        Success (x, input)
    Parser innerFn

/// unwrap function in fp and apply it to the value in xP
let applyP (fP: Parser<'a -> 'b>) (xP: Parser<'a>) : Parser<'b> =
    andThen fP xP
    |> mapP (fun (f, x) -> f x)

/// andThen
let (.>>.) = andThen

/// orElse
let (<|>) = orElse

/// mapP
let (<!>) = mapP

/// applyP
let (<*>) = applyP

/// Pipe parser to mapP
let (|>>) (x: Parser<'a>) (f:'a -> 'b) : Parser<'b> =
    mapP f x

/// retain result of left parser
let (.>>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a> =
    p1 .>>. p2
    |>> (fun (x, y) -> x)

/// retain the result of right parser
let (>>.) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'b> =
    p1 .>>. p2
    |>> (fun (x, y) -> y)

let choice (listOfParsers: Parser<'a> list) : Parser<'a> =
    List.reduce (<|>) listOfParsers

/// lift binary functions and apply to parsers
let lift2 (f: 'a -> 'b -> 'c) (xP: Parser<'a>) (yP: Parser<'b>) : Parser<'c> =
    returnP f <*> xP <*> yP

/// turn a list of parsers into a parser that contains a list of parsed values
let rec sequence (list: Parser<'a> list) : Parser<'a list> =
    let cons head tail = head::tail

    let consP = lift2 cons

    match list with
    | []
        -> returnP []
    | x::xs ->
        consP x (sequence xs)

let pchar charToMatch =
    let innerFn input =
        if input = "" then
            Failure "No more input"
        else
            let first = input[0]
            if first = charToMatch then
                let remainding = input[1..]
                let msg = $"Found {first}"
                Success (charToMatch, remainding)
            else
                let msg = $"Expecting: {charToMatch}, but got: {first}"
                Failure msg
    Parser innerFn

let charListToString chars =
    chars
    |> List.toArray
    |> System.String

let pstring (input: string) : Parser<string> =
    input
    |> Seq.toList
    |> List.map pchar
    |> sequence
    |>> charListToString

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice

let parseLowercase =
    anyOf ['a'..'z']

let parseUppercase =
    anyOf ['A'..'Z']

let parseDigit =
    anyOf ['0'..'9']

let parse3Digits =
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> fun ((c1, c2), c3) -> System.String [| c1; c2; c3 |]
    |>> int

run parse3Digits "453ijk"

let addP = lift2 (+)

let exp = addP (parse3Digits .>> pchar ' ') parse3Digits

run exp "100 200"

let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
let combined = sequence parsers

run combined "ABCD"

let parsePublicModifier = pstring "public"

run parsePublicModifier "public "