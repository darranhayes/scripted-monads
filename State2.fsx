(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
http://matthewmanela.com/blog/functional-stateful-program-in-f/
*)

type State<'st, 'a> = 'a * 'st

module State =
    let bind f m =
        fun state ->
            let result = m state
            match result with
            | (value, state2) -> (f value) state2

type StateBuilder() =
    member this.Return x = fun s -> State(x, s)
    member this.ReturnFrom x = x
    member this.Bind (m, func) = State.bind func m

let state = StateBuilder()

let get () = fun state -> State(state, state)
let put s = fun _ -> State((), s)

let explicitSimpleProgram () =
    (fun x -> State(x, x))
    |> State.bind (fun _ -> get ())
    |> State.bind (fun x -> put (x + 1))
    |> State.bind (fun _ -> get ())
    |> State.bind (fun x -> put (x + 1))
    |> State.bind (fun _ -> get ())

explicitSimpleProgram () 1 |> fst

let ceSimpleProgram () =
    state {
        let! x = get ()
        do! put (x + 1)
        let! y = get ()
        do! put (y + 1)
        return! get ()
    }

ceSimpleProgram () 1 |> fst

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

playGame gameString initialState |> fst
