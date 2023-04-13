// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/

type Parser = char * string -> string * string

let pchar : Parser = fun (charToMatch, str) ->
    if str = "" then
        ("No more input", "")
    else
        let first = str[0]
        if first = charToMatch then
            let remainding = str[1..]
            let msg = $"Found {first}"
            (msg, remainding)
        else
            let msg = $"Expecting: {charToMatch}, but got: {first}"
            (msg, str)

pchar ('A', "")
pchar ('A', "ZBC")
pchar ('A', "ABC")