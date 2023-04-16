module InputState =
    type Position = {
        Line : int
        Column : int
        CharPosition : int
        CurrentLineStartPosition : int
    }

    let initialPosition =
        { Line = 0; Column = 0; CharPosition = 0; CurrentLineStartPosition = 0 }

    let incrementColumn position =
        { position with
            Column = position.Column + 1
            CharPosition = position.CharPosition + 1 }

    let incrementLine position =
        { position with
            Line = position.Line + 1
            Column = 0
            CharPosition = position.CharPosition + 1;
            CurrentLineStartPosition = position.CharPosition + 1 }

    type InputState = {
        Chars : string
        Position : Position
    } with
        member this.updatePosition (newPosition: Position) : InputState =
            { this with Position = newPosition }
        member this.remaining : string =
            this.Chars.Substring this.Position.CharPosition

    let fromString (input: string) : InputState =
        { Chars = input; Position = initialPosition }

    let nextChar (input: InputState) : InputState * char option =
        let currentPosition = input.Position.CharPosition
        if currentPosition >= input.Chars.Length then
            input, None
        else
            let nextChar = input.Chars[currentPosition]
            let newInput =
                match nextChar with
                | '\n' ->
                    input.updatePosition (incrementLine input.Position)
                | _ ->
                    input.updatePosition (incrementColumn input.Position)
            newInput, Some nextChar

module Parser =

    open InputState

    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-2/
    // https://fsharpforfunandprofit.com/posts/understanding-parser-combinators-3/

    type ParserLabel = string
    type ParserError = string

    type ParserPosition = {
        CurrentLine : string
        Line : int
        Column : int
        CharPosition : int
    }

    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition

    type Parser<'a> = {
        ParserFn : InputState -> ParseResult<'a * InputState>
        Label : ParserLabel
    } with
        static member create
            (fn: InputState -> ParseResult<'a * InputState>)
            (label: ParserLabel)
            : Parser<'a> =
                { ParserFn = fn; Label = label }

    let run (parser: Parser<'a>) (input: InputState) : ParseResult<'a * InputState> =
        parser.ParserFn input

    let parserPositionFromInputState (inputState: InputState) : ParserPosition =
        let charPosition = inputState.Position.CharPosition
        let start = inputState.Position.CurrentLineStartPosition
        {
            CurrentLine = inputState.Chars[start..(charPosition + 20)]
            Line = inputState.Position.Line
            Column = inputState.Position.Column
            CharPosition = charPosition
        }

    let charListToString chars =
        chars
        |> List.toArray
        |> System.String

    let charListToInt list =
        list
        |> List.toArray
        |> System.String
        |> int

    let rec parseZeroOrMore (p: Parser<'a>) (input: InputState) : list<'a> * InputState =
        match run p input with
        | Failure _ ->
            ([], input)
        | Success (value1, remaining1) ->
            let (value2, remaining2 ) =
                parseZeroOrMore p remaining1
            (value1::value2, remaining2)

    let satisfy predicate (label: string) =
        let innerFn (input: InputState) =
            let parserPosition =
                parserPositionFromInputState input

            let (remaining, value) =
                nextChar input

            match value with
            | None ->
                Failure (label, "No more input", parserPosition)
            | Some char ->
                if predicate char then
                    Success (char, remaining)
                else
                    let error = sprintf "Unexpected: '%c'" char
                    Failure (label, error, parserPosition)

        Parser.create innerFn label

    let printResult (result: ParseResult<'a * InputState>) =
        match result with
        | Failure (label, error, position) ->
            let text = position.CurrentLine
            let columnPosition = position.Column
            let linePosition = position.Line
            let overallPosition = position.CharPosition
            let failureCaret = sprintf "%*s^%s" columnPosition "" error

            printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePosition columnPosition label text failureCaret
        | Success (value, inputState) ->
            printfn "Success: %A" value

    let setLabel (p: Parser<'a>) (newLabel: string) : Parser<'a> =
        let innerFn input =
            match p.ParserFn input with
            | Failure (_, error, position) ->
                Failure (newLabel, error, position)
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
            | Failure (label, error, position) ->
                Failure (label, error, position)
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

    /// Pipe parser to mapP
    let (|>>) (p: Parser<'a>) (f:'a -> 'b) : Parser<'b> =
        mapP f p

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

    /// sequence converts a list of parsers into a parser containing a list.
    let rec sequence (list: Parser<'a> list) : Parser<'a list> =
        let cons head tail = head::tail
        let consP = lift2 cons

        match list with
        | [] ->
            returnP []
        | x::xs ->
            consP x (sequence xs)

    let pChar charToMatch =
        let predicate ch = (ch = charToMatch)
        let label = sprintf "'%c'" charToMatch
        satisfy predicate label

    let pString (input: string) : Parser<string> =
        let label = sprintf "string %s" input
        input
        |> Seq.toList
        |> List.map pChar
        |> sequence
        |>> charListToString
        <?> label

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
        let label = "whitespace (including linebreaks)"
        satisfy predicate label

    let whitespaces =
        many whitespaceChar

    let whitespaces1 =
        many1 whitespaceChar

    // non-linebreaking whitespace (space & tab)
    let nbWhitespaceChar =
        pChar ' ' <|> pChar '\t'
        <?> "non-linebreaking whitespace"

    let nbWhitespaces =
        many nbWhitespaceChar

    let nbWhitespaces1 =
        many1 nbWhitespaceChar

    let spaceChar =
        pChar ' '

    let spaces =
        many spaceChar

    let spaces1 =
        many1 spaceChar

    let pWindowsStyleLinebreak =
        pString "\r\n"
        <?> "Windows style linebreak"

    let pNixStyleLinebreak =
        pString "\n"
        <?> "(U)nix style linebreak"

    let pLinebreak =
        pWindowsStyleLinebreak <|> pNixStyleLinebreak
        <?> "linebreak (any style)"

    (* Text *)

    let pLetter =
        let predicate = fun c ->
            System.Char.IsLetter c
        let label = "letter"
        satisfy predicate label

    let pLetters =
        many pLetter

    let pLetters1 =
        many1 pLetter

    let pPunctuationChar =
        let predicate = System.Char.IsPunctuation
        let label = "punctuation"
        satisfy predicate label

    let pTextChar =
        let predicate = fun c ->
            System.Char.IsLetterOrDigit c
            || System.Char.IsPunctuation c
            || c = ' '
            || c = '\t'
        let label = "any letter, digit, puncuation, or non-breaking whitespace"
        satisfy predicate label

    let pManyChars cp =
        many cp
        |>> charListToString

    let pManyChars1 cp =
        many1 cp
        |>> charListToString

    let pText =
        separateBy (pManyChars pTextChar) pLinebreak
        <?> "multiple lines of text separated by any-style line breaks"

    (* Numbers *)

    let pDigit =
        let predicate = System.Char.IsDigit
        let label = "digit"
        satisfy predicate label

    let pDigits =
        pManyChars pDigit

    let pDigits1 =
        pManyChars1 pDigit

    let pSign =
        anyOf [ '-'; '+' ]

    let pOptionalSign =
        optional pSign

    let pInt: Parser<int> =
        let label = "signed int"
        let resultToInt (sign, chars) =
            let i =
                chars
                |> int

            match sign with
            | Some '-' -> -i
            | _ -> i

        pOptionalSign .>>. pDigits1
        |>> resultToInt
        <?> label

    let pfloat =
        let label = "signed float"

        let resultToFloat (sign, digits1, _, digits2) =
            let fl =
                sprintf "%s.%s" digits1 digits2
                |> float

            match sign with
            | Some '-' -> -fl
            | _ -> fl

        let flatten =
            fun (((s, d), p), ds) -> (s, d, p, ds)

        // a float is sign, digits, point, digits (ignore exponents for now)
        pOptionalSign .>>. pDigits1 .>>. pChar '.' .>>. pDigits1
        |>> flatten
        |>> resultToFloat
        <?> label

open Parser

let input = ""

run pText (InputState.fromString input) |> printResult
