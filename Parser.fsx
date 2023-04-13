// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a>)

let pchar charToMatch =
    let innerFn str =
        if str = "" then
            Failure "No more input"
        else
            let first = str[0]
            if first = charToMatch then
                let remainding = str[1..]
                let msg = $"Found {first}"
                Success (charToMatch, remainding)
            else
                let msg = $"Expecting: {charToMatch}, but got: {first}"
                Failure msg
    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input

let parseA = pchar 'A'

run parseA "ABC"