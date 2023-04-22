#load "./AssociatedConstructs/Stack.fsx"

open System
open Stack

let precendence (t: string) =
    let p =
        match t with
        | "*" -> 3
        | "/" -> 3
        | "+" -> 4
        | "-" -> 4
        | _ -> failwith "not an op"
    0 - p

let (|Operator|Term|) (t: string) =
    match t with
    | "*" -> Operator t
    | "/" -> Operator t
    | "+" -> Operator t
    | "-" -> Operator t
    | n -> Term n

let rebuildTerm (parseStack: Stack<string>) : Stack<string> =
    let arg2, parseStack =
        parseStack |> Stack.pop

    let operator, parseStack =
        parseStack |> Stack.pop

    let arg1, parseStack =
        parseStack |> Stack.pop

    let newExpr =
        sprintf "(%s %s %s)" arg1 operator arg2

    Stack.push newExpr parseStack

let rec handleOperator (currentOperator: string) (parseStack: Stack<string>) : Stack<string> =
    let previousOperator =
        Stack.tryPeak 1 parseStack

    match currentOperator, previousOperator with
    | current, Some previous
        when precendence current <= precendence previous ->
            rebuildTerm parseStack
            |> handleOperator current
    | current, _ ->
        Stack.push current parseStack

let rec rebuildRemainingTerms (parseStack: Stack<string>) : Stack<string> =
    if parseStack |> Stack.isEmpty || parseStack |> Stack.count = 1 then
        parseStack
    else
        rebuildTerm parseStack
        |> rebuildRemainingTerms

let sya (exprS: string) =
    let expr = exprS.Split(" ") |> Array.toList

    let rec loop tokens parseStack =
        match tokens with
        | [] ->
            rebuildRemainingTerms parseStack
        | token::tokens ->
            match token with
            | Operator (operator) ->
                handleOperator operator parseStack
                |> loop tokens
            | Term (term) ->
                loop tokens (Stack.push term parseStack)

    loop expr Stack.empty

sya "5 + 10 + 4 * 3 - 6 / 4"
|> printfn "%A"

Stack.empty
|> Stack.push "5"
|> Stack.push "+"
|> Stack.push "10"
|> rebuildTerm