(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
https://gist.github.com/jwosty/5338fce8a6691bbd9f6f
*)

type State<'a, 'b> = State of ('a -> ('b * 'a))

module State =
    // run' unwraps the function from the state monad and applies the argument to that function
    let inline private run' container arg = let (State (f)) = container in (f arg)
    let run initialState stateM = run' stateM initialState |> fst
    // bind composes 2 state-monad returning functions by running the first, and applying the result of the 1st
    // expression as an argument to the 2nd, while also passing the new state also
    let bind f stateM =
        State(fun state ->
            let result, newState = run' stateM state // evaluate 1st expression
            run' (f result) newState) // return result of evaluating 2nd expression

    let get () = State(fun s -> s, s)
    let put newState = State(fun _ -> (), newState)

type StateBuilder() =
    member this.Return x = State(fun s -> x, s)
    member this.ReturnFrom x = x
    member this.Bind(stateM, func) = State.bind func stateM

let state = StateBuilder()

(* Examples *)

let inc sM =
    sM
    |> State.bind (fun _ -> State.get ())
    |> State.bind (fun x -> State.put (x + 1))

let explicitSimpleProgram =
    State(fun x -> x, x)
    |> inc
    |> inc
    |> State.bind (fun _ -> State.get ())

explicitSimpleProgram |> State.run 1

let ceSimpleProgram =
    state {
        let! x = State.get ()
        do! State.put (x + 1)
        let! y = State.get ()
        do! State.put (y + 1)
        return! State.get ()
    }

ceSimpleProgram |> State.run 1

type GameState = bool * int
let gameString = "abcaaacbbcabbab"
let initialState = (false, 0)
let toString (xs: char list) = xs |> Seq.toArray |> System.String

let updateScore c =
    state {
        let! on, score = State.get ()

        do! match c with
            | 'a' -> State.put (on, score + 1)
            | 'b' -> State.put (on, score - 1)
            | 'c' -> State.put (not on, score)
            | _ -> State.put (on, score)
    }

let rec playGame str =
    state {
        match (str |> Seq.toList) with
        | [] -> return! State.get ()
        | x :: xs ->
            do! updateScore x
            return! playGame (xs |> toString)
    }

playGame gameString |> State.run (false, 0)
