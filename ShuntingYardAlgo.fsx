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

let rebuildTerm (terms, operators) =
    let arg2, terms =
        terms |> Stack.pop
    let arg1, terms =
        terms |> Stack.pop

    let operator, operators =
        operators |> Stack.pop

    let newExpr =
        sprintf "(%s %s %s)" arg1 operator arg2

    Stack.push newExpr terms, operators

let rec handleOperator currentOperator (terms, operators) =
    let previousOperator =
        Stack.tryPeak 0 operators

    match currentOperator, previousOperator with
    | current, Some previous
        when precendence current <= precendence previous ->
            rebuildTerm (terms, operators)
            |> handleOperator current
    | current, _ ->
        terms, Stack.push current operators

let rec rebuildRemainingTerms (terms, operators) =
    if operators |> Stack.isEmpty then
        terms, operators
    else
        rebuildTerm (terms, operators)
        |> rebuildRemainingTerms

let sya (exprS: string) =
    let expr = exprS.Split(" ") |> Array.toList

    let rec loop tokens (terms, operators) =
        match tokens with
        | [] ->
            rebuildRemainingTerms (terms, operators)
        | token::tokens ->
            match token with
            | Operator (operator) ->
                handleOperator operator (terms, operators)
                |> loop tokens
            | Term (term) ->
                loop tokens ((Stack.push term terms), operators)

    loop expr (Stack.empty, Stack.empty)

sya "5 + 10 + 4 * 3 - 6 / 4"
|> printfn "%A"
