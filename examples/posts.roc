app [server] {
    pf: platform "https://github.com/roc-lang/basic-webserver/releases/download/0.7.0/vUnq0H5KAITUGuzI3av6AHYLS8LnaYI6qzIMsTNHq3M.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    gql: "../package/main.roc",
}

import pf.Task exposing [Task]
import pf.Http exposing [Request, Response]
import json.Json
import gql.Gql.Schema
import gql.Gql.Parse
import gql.Gql.Input exposing [const, required]
import gql.Gql.Enum
import gql.Gql.Value exposing [Value]
import gql.Gql.Output exposing [
    Object,
    string,
    int,
    listOf,
    nullable,
    ref,
    object,
    field,
    retField,
]

# -- SCHEMA --

schema =
    { query }

query : Object {} Value
query =
    object "Query" [
        retField "posts" (listOf (ref post)) \_ -> postsData,
        field "post" (nullable (ref post)) {
            takes: const {
                id: <- required "id" Gql.Input.int,
            },
            resolve: \_, { id } ->
                postsData
                |> List.findFirst \p -> p.id == id
                |> Result.mapErr \NotFound -> Nothing,
        },
    ]

postsData : List Post
postsData = [
    {
        id: 1,
        title: "Hi",
        body: Ok "Testing",
        section: News,
        author: { firstName: "John", lastName: "Doe" },
    },
    {
        id: 2,
        title: "Post 2",
        body: Err Nothing,
        section: Opinion,
        author: { firstName: "Agus", lastName: "Zubiaga" },
    },
]

Post : {
    id : I32,
    title : Str,
    body : Result Str [Nothing],
    author : Author,
    section : [News, Opinion],
}

post : Object Post Value
post =
    object "Post" [
        retField "id" int .id,
        retField "title" string .title,
        retField "body" (nullable string) .body,
        retField "author" (ref author) .author,
        retField "section" postSection .section,
    ]

postSection =
    Gql.Enum.new "PostSection" {
        news: <- Gql.Enum.withCase "NEWS",
        opinion: <- Gql.Enum.withCase "OPINION",
    }
    |> Gql.Enum.type \value ->
        when value is
            News ->
                .news

            Opinion ->
                .opinion

Author : {
    firstName : Str,
    lastName : Str,
}

author : Object Author Value
author =
    object "Author" [
        retField "firstName" string .firstName,
        retField "lastName" string .lastName,
    ]

# -- PARSE AND EXECUTE --

Model : {}

server = { init, respond }

init : Task Model [Exit I32 Str]_
init = Task.ok {}

handleReq : Request -> Task Response _
handleReq = \req ->
    req.body
    |> Decode.fromBytes Json.utf8
    |> Result.mapErr JsonErr
    |> Result.try \json -> json.query |> Gql.Parse.parseDocument |> Result.mapErr ParseErr
    |> Result.try \document ->
        Gql.Schema.execute {
            schema,
            document,
            operation: First,
            variables: Dict.empty {},
            rootValue: {},
            fromValue: \value -> value,
        }
        |> Result.mapErr ExecuteErr
    |> Task.fromResult
    |> Task.map
        (\data -> {
            status: 200,
            headers: [{ name: "Content-Type", value: "application/json" }],
            body: Object [("data", Object data)] |> Gql.Value.toJson |> Str.toUtf8,
        })

respond : Request, Model -> Task Response [ServerErr Str]_
respond = \req, _ ->
    handleReq req |> Task.onErr handleErr

handleErr = \error ->
    when error is
        ExecuteErr err -> err |> Gql.Schema.executeErrToStr |> respondWithError 400
        SelectionErr err -> err |> Gql.Output.resolveErrToStr |> respondWithError 400
        ParseErr err -> err |> Gql.Parse.errToStr |> respondWithError 400
        JsonErr _ -> "Failed to parse body JSON" |> respondWithError 400

respondWithError : Str, U16 -> Task Response []
respondWithError = \msg, status ->
    Task.ok {
        status,
        headers: [{ name: "Content-Type", value: "application/json" }],
        body: Object [("error", String msg)] |> Gql.Value.toJson |> Str.toUtf8,
    }
