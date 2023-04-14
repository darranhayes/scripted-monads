(*
Goal: implement state monad influenced by https://wiki.haskell.org/State_Monad
https://gist.github.com/jwosty/5338fce8a6691bbd9f6f
*)

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    /// return - construct a stateful computation with the value x
    let ret x =
        State(fun s -> x, s)

    let get () =
        State(fun s -> s, s)

    let put newState =
        State(fun _ -> (), newState)

    /// applies the initial state to the computation m
    let inline run state (m : State<'s, 'a>)  =
        let (State f) = m
        f state

    let bind f m =
        State(
            fun state ->
                let result, newState = run state m
                run newState (f result)
        )

    let map f m =
        bind (f >> ret) m

type StateBuilder() =
    member this.Return x =
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

let explicitSimpleProgram initialValue =
    op initialValue 5
    >>= (fun a -> op a 7)
    <!> (fun b -> 1000 - b) // no stateful effects from this calculation, use map <!> instead of bind >>=
    >>= (fun c -> State.ret (sprintf "Calculation result is %i" c))

explicitSimpleProgram 100 |> State.run 0

let ceSimpleProgram initialValue =
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

// Example expansion of a simple stateful computation
// Each step can be evaluated interactively
let cmp = State.get() >>= (fun x -> State.put (x + 1))
State.run 1 cmp

// substitute cmp for its value
State.run 1 (State(fun s -> s, s) >>= (fun x -> State(fun _ -> (), (x + 1))))

// expand (>>=), and substitute 'run' for resolved call to State.run
State.run 1 (State(fun state ->
    let result, newState = State.run state (State(fun s -> s, s))
    State.run newState ((fun x -> State(fun _ -> (), (x + 1))) result)
))

// expand State.run 1
(fun state ->
    let result, newState = State.run state (State(fun s -> s, s))
    State.run newState ((fun x -> State(fun _ -> (), (x + 1))) result)) 1

// apply 1 to lambda arg 'state' and evaluate
let result, newState = State.run 1 (State(fun s -> s, s))
State.run newState ((fun x -> State(fun _ -> (), (x + 1))) result)

// expand State.run 1
let result', newState' = (fun s -> s, s) 1
State.run newState' ((fun x -> State(fun _ -> (), (x + 1))) result')

// calculate result'' & newState''
let result'', newState'' = 1,1
State.run newState'' ((fun x -> State(fun _ -> (), (x + 1))) result'')

// reduce State.run newState' ...
State.run 1 ((fun x -> State(fun _ -> (), (x + 1))) 1)

// apply 1 to lambda arg 'x'
State.run 1 (State(fun _ -> (), (1 + 1)))

// Expand State.run 1
(fun _ -> (), (1 + 1)) 1
(), (1 + 1)
(), 2

type F1<'s, 'a> = int -> State<'s, 'a>
type F2<'s, 'a> = string -> State<'s, 'a>

let intToString : F1<'s, string> = fun v -> State.ret(v.ToString())
let stringLength : F2<'s, int> = fun v -> State.ret(v.Length)

let program i = State.ret i >>= intToString >>= stringLength
State.run "" (program 5)

