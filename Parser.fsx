// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

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

let run parser input =
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

let (.>>.) = andThen

let parseA: Parser<char> = pchar 'A'
let parseB: Parser<char> = pchar 'B'

let parseAThenB = parseA .>>. parseB

run parseAThenB ""
run parseAThenB "A"
run parseAThenB "AB"
run parseAThenB "AC"
run parseAThenB "ABC"
run parseAThenB "ZBC"
