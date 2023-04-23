#load "./AssociatedConstructs/Stack.fsx"

open System
open Stack

let (|Operator|Constant|) (token: string) =
    match token with
    | "*" -> Operator token
    | "/" -> Operator token
    | "+" -> Operator token
    | "-" -> Operator token
    | n -> Constant n

/// <summary>
/// Order of Precedence taken from wikipedia:
/// https://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
/// </summary>
let precendence (t: string) =
    match t with
    | Operator "*" -> 3
    | Operator "/" -> 3
    | Operator "+" -> 4
    | Operator "-" -> 4
    | _ -> failwith "not an op"

type Associativity =
    | Left
    | Right

/// Returns Left or Right associativity of the operator
let operatorAssociatesTo (operator: string) =
    match operator with
    | Operator "*" -> Left
    | Operator "/" -> Left
    | Operator "+" -> Left
    | Operator "-" -> Left
    | _ -> failwith "not an op"

/// returns a comparer appropriate to the operator associativity
let associateTo (direction: Associativity) =
    match direction with
    | Left -> (>=)
    | Right -> (>)

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

    let isHigher = associateTo (operatorAssociatesTo currentOperator)

    match currentOperator, previousOperator with
    // | ")", Some _ ->
        // unwind until "(" and pop back as a single entry
    | current, Some previous
        when isHigher (precendence current) (precendence previous) ->
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
            | Constant (term) ->
                loop tokens (Stack.push term parseStack)

    loop expr Stack.empty

sya "5 + 10 + 4 * 3 - 6 / 4"
|> printfn "%A"

Stack.empty
|> Stack.push "5"
|> Stack.push "+"
|> Stack.push "10"
|> rebuildTerm