(*
Goal: implement state monad influenced by https://wiki.haskell.org/State_Monad
http://matthewmanela.com/blog/functional-stateful-program-in-f/
*)

type State<'s, 'a> = 'a * 's

module State =
    /// return - construct a stateful computation with the value x
    let ret (x : 'a) : 's -> State<'s, 'a> =
        fun s -> State(x, s)

    let get () =
        fun state -> State(state, state)

    let put s  =
        fun _ -> State((), s)

    /// applies the initial state to the computation m
    let inline run state (m : 's -> State<'s, 'a>) =
        m state

    let bind (f : 'a -> 's -> State<'s, 'b>) (m : 's -> State<'s, 'a>) : 's -> State<'s, 'b> =
        fun state ->
            let result, newState = m state
            (f result) newState

    let map (f : 'a -> 'b) (m : 's -> State<'s, 'a>) : 's -> State<'s, 'b> =
        bind (f >> ret) m

type StateBuilder() =
    member this.Return (x : 'a) : 's -> State<'s, 'a> =
        State.ret x
    member this.ReturnFrom x =
        x
    member this.Bind(m, f) =
        State.bind f m

let state = StateBuilder()

let (>>=) m f = State.bind f m
let (<!>) m f = State.map f m

(* Examples *)

let op x y =
    State.get ()
    >>= (fun s -> State.put (s + 1)) // track the number of operations performed
    >>= (fun _ -> State.ret (x + y))

let explicitSimpleProgram (initialValue : int) =
    op initialValue 5
    >>= (fun a -> op a 7)
    <!> (fun b -> 1000 - b) // no stateful effects from this calculation, use map <!> instead of bind >>=
    >>= (fun c -> State.ret (sprintf "Calculation result is %i" c))

explicitSimpleProgram 100 |> State.run 0


let ceSimpleProgram (initialValue : int) =
    state {
        let! a = op initialValue 5
        let! b = op a 7
        let c = 1000 - b // no stateful effects from this calculation, use let instead of let!
        return (sprintf "Calculation result is %i" c)
    }

ceSimpleProgram 100 |> State.run 0

type GameState = bool * int
let gameString = "abcaaacbbcabbab"
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