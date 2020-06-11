(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
http://matthewmanela.com/blog/functional-stateful-program-in-f/
*)

type State<'a, 'b> = 'b * 'a

module State =
    let run initialState stateM = stateM initialState |> fst

    let bind f stateM =
        fun state ->
            let result, newState = stateM state
            (f result) newState

    let get () = fun state -> State(state, state)
    let put s = fun _ -> State((), s)

type StateBuilder() =
    member this.Return x = fun s -> State(x, s)
    member this.ReturnFrom x = x
    member this.Bind(stateM, func) = State.bind func stateM

let state = StateBuilder()

(* Examples *)

let inc sM =
    sM
    |> State.bind (fun _ -> State.get ())
    |> State.bind (fun x -> State.put (x + 1))

let explicitSimpleProgram =
    (fun x -> State(x, x))
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

playGame gameString |> State.run initialState
