(* DeSugaring a function with support for generic arguments *)

type Delegate<'a, 'b> = Delegate of ('a -> 'b * 'a)

let deSugar delegateInstance arg = // this function returns f(arg)
    let (Delegate(f)) = delegateInstance in f arg

let d = Delegate(fun x -> x.ToString(), x)

let func = deSugar d
let value = func 5

printfn "%s" (value |> fst)
