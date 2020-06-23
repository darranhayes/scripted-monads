(*
    Based on dependency injection using flexible type arguments:
    https://bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/
*)

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

    let debug (env: #ILog) fmt = Printf.kprintf env.Logger.Debug fmt

    let info (env: #ILog) fmt = Printf.kprintf env.Logger.Info fmt

module DB =
    open Infrastructure.Types

    let load (env: #IDB) i = env.Database.Load i

    let save (env: #IDB) str = env.Database.Save str


open Infrastructure

let getById env id =
    Log.debug env "Loading... %i" id
    let existing = DB.load env id
    Log.debug env "Loaded: \"%s\"" existing
    existing

let save env newValue =
    Log.debug env "Updating..."
    DB.save env newValue
    Log.debug env "Updated with: %s" newValue
    newValue

let updateEntityWorkflow env entityId =
    Log.info env "Begin"

    let result =
        if entityId > 0 then
            let existing = getById env 99
            let updated = save env (existing + "_updated")
            Ok("Saved: " + updated)
        else
            Log.debug env "Skipping %i" entityId
            Error(sprintf "Invalid id: %i" entityId)

    Log.info env "End"
    result

open Infrastructure.Types

type AppEnv() =
    interface ILog with
        member _.Logger = (Impl.LiveLogger() :> ILogger)

    interface IDB with
        member _.Database = (Impl.FakeDB() :> IDatabase)

let result = updateEntityWorkflow (AppEnv()) 99

match result with
| Ok str -> printf "Success: %s\n" str
| Error error -> printf "Failure: %s\n" error
