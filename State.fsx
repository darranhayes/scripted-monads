(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
*)

let toString (xs: char list) = xs |> Seq.toArray |> System.String

(* V1, pass state recursively with game implementation *)
type GameState = bool * int
let gameString = "abcaaacbbcabbab"
let initialState = (false, 0)

module ExplicitState =
    let rec playGame (str: string) (state: GameState) =
        let on, score = state
        match (str |> Seq.toList) with
        | [] -> state
        | x :: xs ->
            let newState =
                match x with
                | 'a' -> (on, score + 1)
                | 'b' -> (on, score - 1)
                | 'c' -> (not on, score)
                | _ -> (on, score)

            playGame (xs |> toString) newState

ExplicitState.playGame (gameString) initialState

(* https://gist.github.com/jwosty/5338fce8a6691bbd9f6f *)

type State<'a, 'b> = State of ('a -> ('b * 'a))

let get = State(fun s -> s, s)
let put newState = State(fun _ -> (), newState)

module State =
    let inline private run' state program = let (State (f)) = program in f state
    let run initialState program = run' initialState program |> fst

    let bind f m =
        State(fun state ->
            let result, state = run' state m
            run' state (f result))

type StateBuilder() =
    member this.Return x = State(fun s -> x, s)
    member this.ReturnFrom(x: State<'s, 'v>) = x
    member this.Bind(m, func): State<'s, 'v> =
        printfn "Bind f: %O, m: %O" func m
        State.bind func m

let state = StateBuilder()

let simpleProgram =
    state {
        let! x = get
        do! put (x + 1)
        let! y = get
        do! put (y + 1)
        return! get
    }

simpleProgram |> State.run (1)

let rec playGame str =
    state {
        match (str |> Seq.toList) with
        | [] -> return! get
        | x :: xs ->
            let! on, score = get

            do! match x with
                | 'a' -> put (on, score + 1)
                | 'b' -> put (on, score - 1)
                | 'c' -> put (not on, score)
                | _ -> put (on, score)

            return! playGame (xs |> toString)
    }

let recursiveProgram = playGame gameString
let result = State.run (false, 0) recursiveProgram
