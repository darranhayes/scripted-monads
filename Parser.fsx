// https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/

type Parser = string -> bool * string

let parseA : Parser = fun str ->
    let c = str[0]
    let remainder = str[1..]
    if str = "" then
        (false, "")
    else
        if c = 'A' then
            (true, remainder)
        else
            (false, str)

parseA ""