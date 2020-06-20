(*
    Based on Reader monad:
    https://bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/
*)
type Reader<'env, 'out> = Reader of ('env -> 'out)

module Reader =
    let run (env: 'env) (Reader fn): 'out = fn env
    let inline value (x: 'out): Reader<'env, 'out> = Reader(fun _ -> x)
    let inline apply (fn: 'env -> 'out): Reader<'env, 'out> = Reader fn

    let bind f reader =
        Reader(fun env ->
            let result = run env reader
            run env (f result))

type ReaderBuilder() =
    member inline _.Zero x = Reader.value x
    member inline _.Return x = Reader.value x
    member inline _.ReturnFrom x = x
    member inline _.Bind(reader, func) = Reader.bind func reader
    member inline this.Delay(f) = this.Bind(this.Zero(), f)
    member inline this.Combine(fx, fy) = this.Bind(fx, (fun () -> fy))

let reader = ReaderBuilder()

module Infrastructure =
    module Types =
        [<Interface>]
        type ILogger =
            abstract Debug: string -> unit
            abstract Info: string -> unit

        [<Interface>]
        type ILog =
            abstract Logger: ILogger

        [<Interface>]
        type IDatabase =
            abstract Load: int -> string
            abstract Save: string -> unit

        [<Interface>]
        type IDB =
            abstract Database: IDatabase

    module Impl =
        open Types

        type LiveLogger() =
            interface ILogger with
                member _.Debug str = printf "DEBUG: %s\n" str
                member _.Info str = printf "INFO: %s\n" str

        type FakeDB() =
            interface IDatabase with
                member _.Load i = ("Entity_" + i.ToString())
                member _.Save _ = ()

module Log =
    open Infrastructure.Types

    let debug str =
        let dep s =
            Reader.apply (fun (x: #ILog) -> x.Logger.Debug s)

        Printf.kprintf dep str

    let info str =
        let dep s =
            Reader.apply (fun (x: #ILog) -> x.Logger.Info s)

        Printf.kprintf dep str

module DB =
    open Infrastructure.Types

    let load i =
        Reader.apply (fun (x: #IDB) -> x.Database.Load i)

    let save str =
        Reader.apply (fun (x: #IDB) -> x.Database.Save str)

open Infrastructure

let getById id =
    reader {
        do! Log.debug "Loading... %i" id
        let! existing = DB.load id
        do! Log.debug "Loaded: \"%s\"" existing
        return existing
    }

let save newValue =
    reader {
        do! Log.debug "Updating..."
        do! DB.save newValue
        do! Log.debug "Updated with: %s" newValue
        return newValue
    }

let updateEntityWorkflow entityId =
    reader {
        do! Log.info "Begin"

        let! result =
            reader {
                if entityId > 0 then
                    let! existing = getById 99
                    let! updated = save (existing + "_updated")
                    return Ok("Saved: " + updated)
                else
                    do! Log.debug "Skipping %i" entityId
                    return Error(sprintf "Invalid id: %i" entityId)
            }

        do! Log.info "End"

        return result
    }

open Infrastructure.Types

type AppEnv() =
    interface ILog with
        member _.Logger = (Impl.LiveLogger() :> ILogger)

    interface IDB with
        member _.Database = (Impl.FakeDB() :> IDatabase)

let result =
    updateEntityWorkflow 99 |> Reader.run (AppEnv())

match result with
| Ok str -> printf "Success: %s\n" str
| Error error -> printf "Failure: %s\n" error
