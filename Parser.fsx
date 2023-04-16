module Parser =

    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/

    type ParserLabel = string
    type ParserError = string

    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError

    type Parser<'a> = {
        ParserFn : string -> ParseResult<'a * string>
        Label : ParserLabel
    } with
        static member create
            (fn: string -> ParseResult<'a * string>)
            (label: ParserLabel)
            : Parser<'a> =
            { ParserFn = fn; Label = label }

    let run (parser: Parser<'a>) (input: string) : ParseResult<'a * string> =
        parser.ParserFn input

    let charListToString chars =
        chars
        |> List.toArray
        |> System.String

    let charListToInt list =
        list
        |> List.toArray
        |> System.String
        |> int

    let rec parseZeroOrMore (p: Parser<'a>) (input: string) : list<'a> * string =
        match run p input with
        | Failure _ ->
            ([], input)
        | Success (value1, remaining1) ->
            let (value2, remaining2 ) =
                parseZeroOrMore p remaining1
            (value1::value2, remaining2)

    let satisfy predicate (label: string) =
        let innerFn input =
            if System.String.IsNullOrEmpty input then
                Failure (label, "No more input")
            else
                let first = input[0]
                if predicate first then
                    let remaining = input[1..]
                    Success (first, remaining)
                else
                    let error = sprintf "Unexpected: '%c'" first
                    Failure (label, error)

        Parser.create innerFn label

    let pChar charToMatch =
        let predicate ch = (ch = charToMatch)
        let label = sprintf "'%c'" charToMatch
        satisfy predicate label

    let printResult (result: ParseResult<'a * string>) =
        match result with
        | Failure (label, error) ->
            printfn "Error parsing %s\n%s" label error
        | Success (value) ->
            printfn "%A" value

    let setLabel (p: Parser<'a>) (newLabel: string) : Parser<'a> =
        let innerFn input =
            match p.ParserFn input with
            | Failure (_, error) ->
                Failure (newLabel, error)
            | Success s ->
                Success s
        Parser.create innerFn newLabel

    let (<?>) = setLabel

    let getLabel (p: Parser<'a>) : string =
        p.Label

    /// returnP lifts a normal value into the world of parsers.
    let returnP (x: 'a) : Parser<'a> =
        let label = sprintf "%A" x
        let innerFn input =
            Success (x, input)
        Parser.create innerFn label

    /// bindP chains the result of a parser to another parser-producing function.
    let bindP (f: 'a -> Parser<'b>) (p: Parser<'a>) : Parser<'b> =
        let label = "unknown"
        let innerFn input =
            match run p input with
            | Failure (label, error) ->
                Failure (label, error)
            | Success (value1, remainder1) ->
                let newParser = f value1
                run newParser remainder1
        Parser.create innerFn label

    /// bindP
    let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
        bindP f p

    /// mapP transforms the result of a parser.
    let mapP (f: 'a -> 'b) : Parser<'a> -> Parser<'b> =
        bindP (f >> returnP)

    /// mapP
    let (<!>) =
        mapP

    /// applyP lifts multi-parameter functions into functions that work on parsers.
    let applyP (fP: Parser<'a -> 'b>) (xP: Parser<'a>) : Parser<'b> =
        fP >>= (fun f ->
        xP >>= (fun x ->
            returnP (f x)))

    /// applyP
    let (<*>) =
        applyP

    /// lift binary functions and apply to parsers
    let lift2 (f: 'a -> 'b -> 'c) (xP: Parser<'a>) (yP: Parser<'b>) : Parser<'c> =
        returnP f <*> xP <*> yP

    /// andThen compose two parsers
    let andThen (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b> =
        let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
        p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result, p2Result)))
        <?> label

    /// andThen
    let (.>>.) =
        andThen

    let orElse (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
        let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)

        let innerFn input =
            match run p1 input with
                | Success v ->
                    Success v
                | Failure _ ->
                    run p2 input

        Parser.create innerFn label
        <?> label

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

    /// sequence converts a list of parsers into a parser containing a list.
    let rec sequence (list: Parser<'a> list) : Parser<'a list> =
        let cons head tail = head::tail
        let consP = lift2 cons

        match list with
        | [] ->
            returnP []
        | x::xs ->
            consP x (sequence xs)

    /// many matches zero or more occurrences of the specified parser.
    let many (p: Parser<'a>) : Parser<'a list> =
        let label = sprintf "many %s" (getLabel p)
        let innerFn input =
            Success (parseZeroOrMore p input)
        Parser.create innerFn label

    /// many1 matches one or more occurrences of the specified parser.
    let many1 (p: Parser<'a>) : Parser<'a list> =
        let label = sprintf "many %s" (getLabel p)
        p >>= (fun head ->
        many p >>= (fun tail ->
            returnP (head::tail)))
        <?> label

    let anyOf listOfChars =
        let label = sprintf "anyOf %A" listOfChars
        listOfChars
        |> List.map pChar
        |> choice
        <?> label

    /// opt matches an optional occurrence of the specified parser.
    let optional (p : Parser<'a>) : Parser<'a option> =
        let some = p |>> Some
        let none = returnP None
        some <|> none
        <?> sprintf "optional %s" (getLabel p)

    /// betweenInclusive returns the middle parser surrounded by the boundary parsers.
    let betweenInclusive (p1: Parser<'a>) (p2: Parser<'b>) (p3: Parser<'c>) : Parser<('a * 'b) * 'c> =
        (p1 .>>. p2) .>>. p3
        <?> sprintf "%s%s%s" (getLabel p1) (getLabel p2) (getLabel p3)

    /// betweenExclusive keeps only the result of the middle parser.
    let betweenExclusive (p1: Parser<'a>) (p2: Parser<'b>) (p3: Parser<'c>) : Parser<'b> =
        p1 >>. p2 .>> p3
        <?> sprintf "%s%s%s" (getLabel p1) (getLabel p2) (getLabel p3)

    /// separateBy1 parses one or more occurrences of a parser with a separator.
    let separateBy1 (p: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> =
        p .>>. many (separator >>. p)
        |>> List.Cons
        <?> sprintf "list of %s separated by %s" (getLabel p) (getLabel p)

    /// separateBy parses zero or more occurrences of a parser with a separator.
    let separateBy (p: Parser<'a>) (separator: Parser<'b>) : Parser<'a list> =
        separateBy1 p separator <|> returnP []

    let whitespaceChar =
        let predicate = System.Char.IsWhiteSpace
        let label = "whitespace"
        satisfy predicate label

    let spaces =
        many whitespaceChar

    let spaces1 =
        many1 whitespaceChar

    let punctuationChar =
        let predicate = System.Char.IsPunctuation
        let label = "punctuation"
        satisfy predicate label

    let pString (input: string) : Parser<string> =
        let label = sprintf "string %s" input
        input
        |> Seq.toList
        |> List.map pChar
        |> sequence
        |>> charListToString
        <?> label

    let pDigit =
        let predicate = System.Char.IsDigit
        let label = "digit"
        satisfy predicate label

    let pSign =
        anyOf [ '-'; '+' ]

    let pInt: Parser<int> =
        let label = "signed int"
        let resultToInt (sign, chars) =
            let i =
                chars
                |> charListToInt

            match sign with
            | Some '-' -> -i
            | _ -> i

        let sign =
            optional pSign
        let digits =
            many1 pDigit

        sign .>>. digits
        |>> resultToInt
        <?> label

(* *)
open Parser

let oi = pInt
let c = pChar 'A'
let accessModifier = pString "public" <|> pString "private"

run c "a3" |> printResult