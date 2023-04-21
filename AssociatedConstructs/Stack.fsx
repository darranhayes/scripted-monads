type Stack<'a> =
    | Empty
    | Value of 'a * Stack<'a>

module Stack =
    let empty =
        Empty

    let isEmpty = function
        | Empty   -> true
        | Value _ -> false

    let push (v: 'a) (stack: Stack<'a>) : Stack<'a> =
        Value (v, stack)

    let tryPop (stack: Stack<'a>) : Option<'a * Stack<'a>> =
        match stack with
        | Empty ->
            None
        | (Value (v, stack)) ->
            Some (v , stack)

    let pop (stack: Stack<'a>) : 'a * Stack<'a> =
        match stack with
        | Empty ->
            failwith "empty"
        | (Value (v, stack)) ->
            v , stack

    let tryPeak (n: int) (stack: Stack<'a>) : Option<'a> =
        let rec loop stack i =
            match stack with
            | Empty ->
                None
            | (Value (v, rest)) ->
                if i = n then
                    Some v
                else
                    loop rest (i+1)
        loop stack 0

    let peak (n: int) (stack: Stack<'a>) : 'a =
        match tryPeak n stack with
        | None ->
            failwith "empty"
        | Some v ->
            v

    let fold (folder: 'b -> 'a -> 'b) (state: 'b) (stack: Stack<'a>) : 'b =
        let rec loop state stack =
            match stack with
            | Empty ->
                state
            | (Value (v, rest)) ->
                loop (folder state v) rest
        loop state stack

    let toList (stack: Stack<'a>) : List<'a> =
        let rec loop stk =
            match stk with
            | Empty ->
                []
            | (Value (v, rest)) ->
                v :: loop rest
        loop stack

    let ofList (list: List<'a>) : Stack<'a> =
        let rec loop stack list: Stack<'b> =
            match list with
            | [] ->
                stack
            | x::xs ->
                loop (push x stack) xs
        list |> List.rev |> loop empty

    let map (mapper: 'a -> 'b) (stack: Stack<'a>) : Stack<'b> =
        stack |> toList |> List.map mapper |> ofList

    let rec iter (fn : 'a -> unit) (stack: Stack<'a>) : unit =
        match stack with
        | Empty ->
            ()
        | (Value (v, rest)) ->
            fn v
            iter fn rest

    let count (stack: Stack<'a>) : int =
        fold (fun count _ -> count + 1) 0 stack
