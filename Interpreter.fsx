(* DSL plus interpreter (structurally similar to Free.fsx but without the unnecessary Free abstraction) *)
open System

module Blogging =
    module Domain =
        type BlogId = BlogId of Guid
        type Title = Title of string

        type AuthorId = AuthorId of Guid
        type Name = Name of string

        type PostId = PostId of Guid
        type Content = Content of string

        type Blog = { Id: BlogId; Title: Title }

        type Author = { Id: AuthorId; Name: Name }

        type Post =
            { Id: PostId
              BlogId: BlogId
              AuthorId: AuthorId
              Title: Title
              Content: Content }

    module Logic =
        open Domain

        let newBlog x =
            { Id = BlogId(Guid.NewGuid())
              Title = x }

        let createAuthor x =
            { Id = AuthorId(Guid.NewGuid())
              Name = x }

        let newPost blogId authorId postTitle =
            { Id = PostId(Guid.NewGuid())
              Title = postTitle
              BlogId = blogId
              AuthorId = authorId
              Content = Content "" }

module BlogTesting =
    module Implementation =
        open Blogging.Domain

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

    open Implementation

    type BlogBuilder() =
        member this.Bind(x, f) = bind f x
        member this.Return x = Stop x
        member this.ReturnFrom x = x
        member this.Zero() = Stop()

    module Commands =
        let newBlog blogTitle = NewBlog(blogTitle, Stop)
        let createAuthor name = CreateAuthor(name, Stop)

        let newBlogPost blogId authorId title =
            NewBlogPost((blogId, authorId, title), Stop)

    [<RequireQualifiedAccess>]
    module FastTests =
        open Blogging.Logic

        let rec interpreter =
            function
            | Stop x -> x
            | NewBlog (name, next) -> newBlog name |> (next >> interpreter)
            | CreateAuthor (name, next) -> createAuthor name |> (next >> interpreter)
            | NewBlogPost ((blogId, authorId, title), next) ->
                newPost blogId authorId title
                |> (next >> interpreter)

    [<RequireQualifiedAccess>]
    module AsyncTests =
        open Blogging.Logic

        let rec interpreter =
            function
            | Stop x -> async { return x }
            | NewBlog (name, next) -> async { return! newBlog name |> (next >> interpreter) }
            | CreateAuthor (name, next) -> async { return! createAuthor name |> (next >> interpreter) }
            | NewBlogPost ((blogId, authorId, title), next) ->
                async {
                    return! newPost blogId authorId title
                            |> (next >> interpreter)
                }

let blogTest = BlogTesting.BlogBuilder()

open Blogging.Domain
open BlogTesting.Commands

let simpleProgram =
    blogTest {
        let! newAuthor = createAuthor (Name "John Doe")
        let! createdBlog = newBlog (Title "John's blog")
        let! newPost = newBlogPost (createdBlog.Id) (newAuthor.Id) (Title "Welcome!")
        return (newAuthor, createdBlog, newPost)
    }

let result =
    simpleProgram |> BlogTesting.FastTests.interpreter

let asyncResult =
    simpleProgram
    |> BlogTesting.AsyncTests.interpreter
    |> Async.RunSynchronously
