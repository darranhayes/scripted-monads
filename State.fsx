(*
Goal: implement state monad as per https://wiki.haskell.org/State_Monad
*)

let toString (xs : char list) = xs |> Seq.toArray |> System.String

(* V1, pass state recursively with game implementation *)
type GameState = (bool * int)
let gameString = "abcaaacbbcabbab"
let initialState = (false, 0)

module ExplicitState =
    let rec playGame (str: string) (state: GameState) =
        let on, score = state
        match (str |> Seq.toList) with
        | [] -> state
        | x::xs ->
            let newState =
                match x with
                | 'a' -> (on, score + 1)
                | 'b' -> (on, score - 1)
                | 'c' -> (not on, score)
                | _ -> (on, score)
            playGame (xs |> toString) newState

ExplicitState.playGame (gameString) initialState
