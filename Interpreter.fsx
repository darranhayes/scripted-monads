(* DSL plus interpreter (structurally similar to Free.fsx but without the unnecessary Free abstraction) *)
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

module BloggingDSL =
    open Blogging

    type BlogInstruction<'a> =
        | NewBlog of (Title * (Blog -> BlogInstruction<'a>))
        | CreateAuthor of (Name * (Author -> BlogInstruction<'a>))
        | NewBlogPost of ((BlogId * AuthorId * Title) * (Post -> BlogInstruction<'a>))
        | Stop of 'a

    let rec bind f =
        function
        | NewBlog (args, next) -> NewBlog(args, next >> bind f)
        | CreateAuthor (args, next) -> CreateAuthor(args, next >> bind f)
        | NewBlogPost (args, next) -> NewBlogPost(args, next >> bind f)
        | Stop x -> f x

    type BlogBuilder() =
        member this.Bind(x, f) = bind f x
        member this.Return x = Stop x
        member this.ReturnFrom x = x
        member this.Zero() = Stop()

module Interpreters =
    open Blogging
    open BloggingDSL

    [<RequireQualifiedAccess>]
    module Pure =
        let rec interpreter =
            function
            | Stop x ->
                x
            | NewBlog (name, next) ->
                newBlog name |> (next >> interpreter)
            | CreateAuthor (name, next) ->
                createAuthor name |> (next >> interpreter)
            | NewBlogPost ((blogId, authorId, title), next) ->
                newPost blogId authorId title
                |> (next >> interpreter)

    [<RequireQualifiedAccess>]
    module Async =
        let rec interpreter =
            function
            | Stop x ->
                async { return x }
            | NewBlog (name, next) ->
                async { return! newBlog name |> (next >> interpreter) }
            | CreateAuthor (name, next) ->
                async { return! createAuthor name |> (next >> interpreter) }
            | NewBlogPost ((blogId, authorId, title), next) ->
                async {
                    return! newPost blogId authorId title
                            |> (next >> interpreter)
                }

    [<RequireQualifiedAccess>]
    module PrettyPrinter =
        let print instruction args obj =
            printfn "%s (%A) = \n%O" instruction args obj
            obj

        let rec interpreter p =
            match p with
            | Stop value ->
                value
            | NewBlog (name, next) ->
                newBlog name
                |> print (nameof NewBlog) [| name |]
                |> (next >> interpreter)
            | CreateAuthor (name, next) ->
                createAuthor name
                |> print (nameof CreateAuthor) [| name |]
                |> (next >> interpreter)
            | NewBlogPost ((blogId, authorId, title), next) ->
                let args : obj[] = [| blogId; authorId; title |]

                newPost blogId authorId title
                |> print (nameof NewBlogPost) args
                |> (next >> interpreter)

let blogging = BloggingDSL.BlogBuilder()

module Commands =
    open BloggingDSL

    let newBlog blogTitle =
        NewBlog(blogTitle, Stop)

    let createAuthor name =
        CreateAuthor(name, Stop)

    let newBlogPost blogId authorId title =
        NewBlogPost((blogId, authorId, title), Stop)

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
