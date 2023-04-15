/// <summary>
/// A monadic parser library
/// </summary>
module Parser

// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

/// returnP lifts a normal value into the world of parsers.
let returnP (x: 'a) : Parser<'a> =
    let innerFn input =
        Success (x, input)
    Parser innerFn

let run (parser: Parser<'a>) (input: string) : ParseResult<'a * string> =
    let (Parser innerFn) = parser
    innerFn input

/// bindP chains the result of a parser to another parser-producing function.
let bindP (f: 'a -> Parser<'b>) (p: Parser<'a>) : Parser<'b> =
    let innerFn input =
        match run p input with
        | Failure err ->
            Failure err
        | Success (value1, remainder1) ->
            let newParser = f value1
            run newParser remainder1
    Parser innerFn

/// bindP
let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    bindP f p

/// mapP transforms the result of a parser.
let mapP (f: 'a -> 'b) : Parser<'a> -> Parser<'b> =
    bindP (f >> returnP)

/// mapP
let (<!>) =
    mapP

/// andThen compose two parsers
let andThen (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b> =
    p1 >>= (fun p1Result ->
    p2 >>= (fun p2Result ->
        returnP (p1Result, p2Result)))

/// andThen
let (.>>.) =
    andThen

/// applyP lifts multi-parameter functions into functions that work on parsers.
let applyP (fP: Parser<'a -> 'b>) (xP: Parser<'a>) : Parser<'b> =
    fP >>= (fun f ->
    xP >>= (fun x ->
        returnP (f x)))

/// applyP
let (<*>) =
    applyP

let orElse (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    let innerFn input =
        match run p1 input with
            | Success v ->
                Success v
            | Failure _ ->
                run p2 input

    Parser innerFn

/// orElse
let (<|>) =
    orElse

/// Pipe parser to mapP
let (|>>) (p: Parser<'a>) (f:'a -> 'b) : Parser<'b> =
    mapP f p

/// .>> keeps only the result of the left side parser.
let (.>>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a> =
    p1 .>>. p2
    |>> (fun (x, y) -> x)

/// >>. keeps only the result of the right side parser.
let (>>.) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'b> =
    p1 .>>. p2
    |>> (fun (x, y) -> y)

let choice (list: Parser<'a> list) : Parser<'a> =
    List.reduce (<|>) list

/// lift binary functions and apply to parsers
let lift2 (f: 'a -> 'b -> 'c) (xP: Parser<'a>) (yP: Parser<'b>) : Parser<'c> =
    returnP f <*> xP <*> yP

/// sequence converts a list of parsers into a parser containing a list.
let rec sequence (list: Parser<'a> list) : Parser<'a list> =
    let cons head tail = head::tail

    let consP = lift2 cons

    match list with
    | [] ->
        returnP []
    | x::xs ->
        consP x (sequence xs)

let rec parseZeroOrMore (p: Parser<'a>) (input: string) : list<'a> * string =
    match run p input with
    | Failure _ ->
        ([], input)
    | Success (value1, remaining1) ->
        let (value2, remaining2 ) =
            parseZeroOrMore p remaining1
        (value1::value2, remaining2)

/// many matches zero or more occurrences of the specified parser.
let many (p: Parser<'a>) : Parser<'a list> =
    let innerFn input =
        Success (parseZeroOrMore p input)

    Parser innerFn

/// many1 matches one or more occurrences of the specified parser.
let many1 (p: Parser<'a>) : Parser<'a list> =
    p >>= (fun head ->
    many p >>= (fun tail ->
        returnP (head::tail)))

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

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice

/// opt matches an optional occurrence of the specified parser.
let opt (p : Parser<'a>) : Parser<'a option> =
    let some = p |>> Some
    let none = returnP None
    some <|> none

/// between keeps only the result of the middle parser.
let between (p1: Parser<'a>) (p2: Parser<'b>) (p3: Parser<'c>) : Parser<'b> =
    p1 >>. p2 .>> p3

/// sepBy1 parses one or more occurrences of a parser with a separator.
let sepBy1 (p: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> =
    p .>>. many (separator >>. p)
    |>> List.Cons

/// sepBy parses zero or more occurrences of a parser with a separator.
let sepBy (p: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> =
    sepBy1 p separator <|> returnP []

let whitespaceChar =
    anyOf [ ' '; '\t'; '\n' ]

let whitespace =
    many whitespaceChar

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

let charListToInt list =
    list
    |> List.toArray
    |> System.String
    |> int

let pint: Parser<int> =
    let resultToInt (sign, chars) =
        let i =
            chars
            |> charListToInt

        match sign with
        | Some '-' -> -i
        | _ -> i

    let sign =
        opt (anyOf [ '-'; '+' ])
    let digits =
        many1 (anyOf [ '0'..'9' ])

    sign .>>. digits
    |>> resultToInt

(* *)

let parseOp =
    whitespace >>. anyOf [ '*'; '/'; '+'; '-' ] .>> whitespace

let parseLParen =
    whitespace .>>. pchar '(' .>>. whitespace

let parseRParen =
    whitespace .>>. pchar ')' .>>. whitespace

let parseTerm =
    whitespace >>. pint .>> whitespace

let expression =
    let expr =
        parseTerm .>>. parseOp .>>. parseTerm

    let withParens =
        between parseLParen expr parseRParen

    expr <|> withParens

run expression " ( +123  +  -456 ) "
run expression " -123 / +1 "

let comma = pchar ','
let digit = anyOf ['0'..'9']

let zeroOrMoreDigitList = sepBy digit comma
let oneOrMoreDigitList = sepBy1 digit comma

run oneOrMoreDigitList "1;"      // Success (['1'], ";")
run oneOrMoreDigitList "1,2;"    // Success (['1'; '2'], ";")
run oneOrMoreDigitList "1,2,3;"  // Success (['1'; '2'; '3'], ";")
run oneOrMoreDigitList "Z;"      // Failure "Expecting '9'. Got 'Z'"

run zeroOrMoreDigitList "1;"     // Success (['1'], ";")
run zeroOrMoreDigitList "1,2;"   // Success (['1'; '2'], ";")
run zeroOrMoreDigitList "1,2,3;" // Success (['1'; '2'; '3'], ";")
run zeroOrMoreDigitList "Z;"     // Success ([], "Z;")

