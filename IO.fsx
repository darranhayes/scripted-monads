open System
open System.IO

type IO<'a> = IO of (unit -> 'a)

module IO =
    let ret v = IO (fun () -> v)

    let run (IO f) = f ()

    let bind (f: 'a -> IO<'b>) (ma: IO<'a>) : IO<'b> =
        IO (
            fun () ->
                run ma
                |> f
                |> run
        )

    type IOBuilder() =
        member this.Return v =
            ret v

        member this.ReturnFrom v =
            v

        member this.Bind (ma: IO<'a>, f: 'a -> IO<'b>) : IO<'b> =
            bind f ma

        member this.Bind(ma : IO<Async<'a>>, f: 'a -> IO<Async<'b>>) : IO<Async<'b>> =
            IO (
                fun () ->
                    async {
                        let! x = run ma
                        return! run (f x)
                    }
            )

        member this.Bind(ma : IO<Async<'a>>, f: 'a -> IO<'b>) : IO<Async<'b>> =
            IO (
                fun () ->
                    async {
                        let! x = run ma
                        return run (f x)
                    }
            )

    let runAsync (IO f) =
        f ()
        |> Async.RunSynchronously

let io = IO.IOBuilder()

let getLine = IO (fun () -> Console.ReadLine())

let putStrLn s = IO (fun () -> printfn "%s" s)

let printTotalFileBytesUsingAsync (path: string) =
    IO(fun () ->
        async {
            let! bytes = File.ReadAllBytesAsync(path) |> Async.AwaitTask
            let fileName = Path.GetFileName(path)
            return sprintf $"File {fileName} has %d{bytes.Length} bytes"
        }
    )

let echo =
    io {
        let! fileName = getLine
        let! fileDetails = printTotalFileBytesUsingAsync fileName
        do! putStrLn fileDetails
        return fileDetails
    }

let fileDetails = echo |> IO.runAsync
