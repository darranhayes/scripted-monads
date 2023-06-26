(* Simple free monad plus interpreter
   Based on Free Monad recipe by Mark Seemann: https://blog.ploeh.dk/2017/08/07/f-free-monad-recipe/ *)
open System

module Blogging =
    type BlogId = BlogId of Guid
    type Title = Title of string

    type AuthorId = AuthorId of Guid
    type Name = Name of string

    type PostId = PostId of Guid
    type Content = Content of string

    type Blog = { Id: BlogId; Title: Title }

    type Author = { Id: AuthorId; Name: Name }

    type Post = {
        Id: PostId
        BlogId: BlogId
        AuthorId: AuthorId
        Title: Title
        Content: Content
    }

    let newBlog x = {
        Id = BlogId(Guid.NewGuid())
        Title = x }

    let createAuthor x = {
        Id = AuthorId(Guid.NewGuid())
        Name = x }

    let newPost blogId authorId postTitle = {
        Id = PostId(Guid.NewGuid())
        Title = postTitle
        BlogId = blogId
        AuthorId = authorId
        Content = Content "" }

module BloggingFree =
    open Blogging

    type BlogInstruction<'a> =
        | NewBlog of (Title * (Blog -> 'a))
        | CreateAuthor of (Name * (Author -> 'a))
        | NewBlogPost of ((BlogId * AuthorId * Title) * (Post -> 'a))

    let private mapI f =
        function
        | NewBlog (args, next) -> NewBlog(args, next >> f)
        | CreateAuthor (args, next) -> CreateAuthor(args, next >> f)
        | NewBlogPost (args, next) -> NewBlogPost(args, next >> f)

    type BlogProgram<'a> =
        | Free of BlogInstruction<BlogProgram<'a>>
        | Pure of 'a

    type Builder() =
        let rec bind f = function
            | Free m -> m |> mapI (bind f) |> Free
            | Pure x -> f x

        member this.Bind(m, f) = bind f m
        member this.Return x = Pure x
        member this.ReturnFrom x = x
        member this.Zero() = Pure ()

module Interpreters =
    open Blogging
    open BloggingFree

    [<RequireQualifiedAccess>]
    module Pure =
        let rec interpreter = function
            | Pure x ->
                x
            | Free (NewBlog (name, next)) ->
                newBlog name |> (next >> interpreter)
            | Free (CreateAuthor (name, next)) ->
                createAuthor name |> (next >> interpreter)
            | Free (NewBlogPost ((blogId, authorId, title), next)) ->
                newPost blogId authorId title |> (next >> interpreter)

    [<RequireQualifiedAccess>]
    module Async =
        let rec interpreter = function
            | Pure x ->
                async { return x }
            | Free (NewBlog (name, next)) ->
                async { return! newBlog name |> (next >> interpreter) }
            | Free (CreateAuthor (name, next)) ->
                async { return! createAuthor name |> (next >> interpreter) }
            | Free (NewBlogPost ((blogId, authorId, title), next)) ->
                async { return! newPost blogId authorId title |> (next >> interpreter) }

    [<RequireQualifiedAccess>]
    module PrettyPrinter =
        open FSharp.Reflection

        let GetUnionCaseName (x: obj) =
            match FSharpValue.GetUnionFields(x, x.GetType()) with
            | case, _ ->
                case.Name

        let print instruction args obj =
            let caseName = GetUnionCaseName instruction
            printfn "%s (%A) = \n%O" caseName args obj
            obj

        let rec interpreter p =
            match p with
            | Pure value ->
                value
            | Free instruction ->
                match instruction with
                | NewBlog (name, next) ->
                    newBlog name
                    |> print instruction [| name |]
                    |> (next >> interpreter)
                | CreateAuthor (name, next) ->
                    createAuthor name
                    |> print instruction [| name |]
                    |> (next >> interpreter)
                | NewBlogPost ((blogId, authorId, title), next) ->
                    let args : obj[] = [| blogId; authorId; title |]

                    newPost blogId authorId title
                    |> print instruction args
                    |> (next >> interpreter)

let blogging = BloggingFree.Builder()

module Commands =
    open BloggingFree

    let newBlog blogTitle =
        Free(NewBlog(blogTitle, Pure))
    let createAuthor name =
        Free(CreateAuthor(name, Pure))
    let newBlogPost blogId authorId title =
        Free(NewBlogPost((blogId, authorId, title), Pure))

open Blogging
open Commands

let createPostProgram =
    blogging {
        let! newAuthor = createAuthor (Name "John Doe")
        let! createdBlog = newBlog (Title "John's blog")
        let! newPost = newBlogPost (createdBlog.Id) (newAuthor.Id) (Title "Welcome!")
        return newPost
    }

createPostProgram |> Interpreters.Pure.interpreter

createPostProgram |> Interpreters.Async.interpreter |> Async.RunSynchronously

createPostProgram |> Interpreters.PrettyPrinter.interpreter