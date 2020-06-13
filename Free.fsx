(* Simple free monad plus interpreter
   Based on Free Monad recipe by Mark Seemann: https://blog.ploeh.dk/2017/08/07/f-free-monad-recipe/ *)
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
            | NewBlog of (Title * (Blog -> 'a))
            | CreateAuthor of (Name * (Author -> 'a))
            | NewBlogPost of ((BlogId * AuthorId * Title) * (Post -> 'a))

        type BlogProgram<'a> =
            | Free of BlogInstruction<BlogProgram<'a>>
            | Pure of 'a

        let mapI f =
            function
            | NewBlog (args, next) -> NewBlog(args, next >> f)
            | CreateAuthor (args, next) -> CreateAuthor(args, next >> f)
            | NewBlogPost (args, next) -> NewBlogPost(args, next >> f)

        let rec bind f =
            function
            | Free x -> x |> mapI (bind f) |> Free
            | Pure x -> f x

    open Implementation

    type BlogBuilder() =
        member this.Bind(x, f) = bind f x
        member this.Return x = Pure x
        member this.ReturnFrom x = x
        member this.Zero() = Pure()

    module Commands =
        let newBlog blogTitle = Free(NewBlog(blogTitle, Pure))
        let createAuthor name = Free(CreateAuthor(name, Pure))

        let newBlogPost blogId authorId title =
            Free(NewBlogPost((blogId, authorId, title), Pure))

    [<RequireQualifiedAccess>]
    module FastTests =
        open Blogging.Logic

        let rec interpreter =
            function
            | Pure x -> x
            | Free (NewBlog (name, next)) -> newBlog name |> (next >> interpreter)
            | Free (CreateAuthor (name, next)) -> createAuthor name |> (next >> interpreter)
            | Free (NewBlogPost ((blogId, authorId, title), next)) ->
                newPost blogId authorId title
                |> (next >> interpreter)

    [<RequireQualifiedAccess>]
    module AsyncTests =
        open Blogging.Logic

        let rec interpreter =
            function
            | Pure x -> async { return x }
            | Free (NewBlog (name, next)) -> async { return! newBlog name |> (next >> interpreter) }
            | Free (CreateAuthor (name, next)) -> async { return! createAuthor name |> (next >> interpreter) }
            | Free (NewBlogPost ((blogId, authorId, title), next)) ->
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
