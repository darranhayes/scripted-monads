(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
https://gist.github.com/jwosty/5338fce8a6691bbd9f6f
*)

type State<'a, 'b> = State of ('a -> ('b * 'a))

module State =
    let inline private run' state program = let (State (f)) = program in f state
    let run initialState program = run' initialState program |> fst
    let bind f m =
        State(fun state ->
            let result, state = run' state m
            run' state (f result))

type StateBuilder() =
    member this.Return x = State(fun s -> x, s)
    member this.ReturnFrom x = x
    member this.Bind (m, func) = State.bind func m

let state = StateBuilder()

let get () = State(fun s -> s, s)
let put newState = State(fun _ -> (), newState)

let explicitSimpleProgram =
    State(fun x -> x, x)
    |> State.bind (fun _ -> get ())
    |> State.bind (fun x -> put (x + 1))
    |> State.bind (fun _ -> get ())
    |> State.bind (fun x -> put (x + 1))
    |> State.bind (fun _ -> get ())

explicitSimpleProgram |> State.run 1

let ceSimpleProgram =
    state {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + 1)
        return! get ()
    }

ceSimpleProgram |> State.run 1

type GameState = bool * int
let gameString = "abcaaacbbcabbab"
let initialState = (false, 0)
let toString (xs: char list) = xs |> Seq.toArray |> System.String

let rec playGame str =
    state {
        match (str |> Seq.toList) with
        | [] -> return! get ()
        | x :: xs ->
            let! on, score = get ()

            do! match x with
                | 'a' -> put (on, score + 1)
                | 'b' -> put (on, score - 1)
                | 'c' -> put (not on, score)
                | _ -> put (on, score)

            return! playGame (xs |> toString)
    }

playGame gameString |> State.run (false, 0)
