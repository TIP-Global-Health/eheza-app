module Gizra.WebData exposing (sendWithHandler, whenNotAsked, resetError)

{-| Some functions for working with WebData.

@docs sendWithHandler, whenNotAsked, resetError

-}

import Http exposing (Error(..), Expect)
import HttpBuilder exposing (..)
import Json.Decode exposing (Decoder, decodeString, succeed)
import RemoteData exposing (RemoteData(..), WebData)


{-| This is a convenience for the common pattern where we build a request with
`HttpBuilder` and want to handle the result as a `WebData a`. So, consider someting
like this:

    type alias Model =
        -- Amongst other fields ...
        { id : WebData Int
        }

    type Msg
        -- Amongst other messages
        = HandleFetchedId (WebData Int)

    -- The point is that the `update` method can now be very simple, since what
    -- is passed to `HandleFetchedId` will already be the `WebData ...` we want
    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        case msg of
            HandleFetchedId id ->
                ( { model | id = id }
                , Cmd.none
                )

    -- Given the format of the JSON returned by the server, this picks out the
    -- thing of the type we're interested in
    decodeId : Decode.Decoder Int
    decodeId =
        Decode.at [ "data", "id" ] decodeInt

    -- You just need to build the `RequestBuilder`, and then call `sendWithHandler`
    fetchFromBackend =
        HttpBuilder.post
            |> -- whatever you need to finish the `RequestBuilder`
            |> sendWithHandler decodeId HandleFetchedId

-}
sendWithHandler : Decoder a -> (WebData a -> msg) -> RequestBuilder any -> Cmd msg
sendWithHandler decoder tagger builder =
    builder
        |> withExpect (Http.expectJson decoder)
        |> HttpBuilder.toTask
        |> RemoteData.asCmd
        |> Cmd.map tagger


{-| Given a possible `msg`, returns `Just msg` if the `RemoteData` is
`NotAsked`. Otherwise, returns `Nothing`.

The use case for this is where you want to start some process (e.g.
an HTTP GET) if we have no data, no request in progress, and no error.

See `resetError` to return the `RemoteData` from an error state back
to `NotAsked`, in cases where (for instance) the user has acknowledged
an error and wishes to retry the request.

    import Http exposing (Error(..))
    import Fixtures exposing (Msg(..))
    import RemoteData exposing (..)

    whenNotAsked DoThis NotAsked --> Just DoThis

    whenNotAsked DoThis (Success ()) --> Nothing

    whenNotAsked DoThis Loading --> Nothing

    whenNotAsked DoThis (Failure Timeout) --> Nothing

-}
whenNotAsked : msg -> RemoteData e a -> Maybe msg
whenNotAsked msg data =
    case data of
        NotAsked ->
            Just msg

        _ ->
            Nothing


{-| If the `RemoteData` is in a `Failure` state, then return `NotAsked`.
Otherwise, returns the `RemoteData` unchanged.

The use case for this is to deal with situations where the user has
acknowledged the error (perhaps clicked on a "Retry" button), and you'd now
like to engage some code that will retry the request if it is `NotAsked`. (See
`whenNotAsked`, for instance).

    resetError NotAsked --> NotAsked

    resetError (Success ()) --> Success ()

    resetError Loading --> Loading

    resetError (Failure Timeout) --> NotAsked

-}
resetError : RemoteData e a -> RemoteData e a
resetError data =
    case data of
        Failure _ ->
            NotAsked

        _ ->
            data
