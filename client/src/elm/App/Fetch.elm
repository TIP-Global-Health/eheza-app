module App.Fetch exposing (andThenFetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInModel)
import Backend.Fetch
import Date
import EveryDict
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Admin.Fetch
import Pages.Clinics.Fetch
import Pages.Device.Fetch
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.People.Fetch
import Pages.Person.Fetch
import Pages.Relationship.Fetch
import Pages.Session.Fetch
import Update.Extra exposing (sequence)


{-| Basically, we're following down the `view` hierarchy to determine, given
what the `view` methods are going to want, what messages we ought to send
in order to get the data they will need.

For the sake of convenience, this isn't called by our own `update` method --
it would need to be called at the end, after the new model is determined.
Instead, it's integrated up one level, in `Main.elm`, where we hook together
the Elm architecture. (This is really a kind of little extension to the Elm
architecture).

As a future optimization, one could actually integrate this with
`animationFrame`, since you don't need to figure out what to fetch for
views. more often than that.

-}
fetch : Model -> List Msg
fetch model =
    let
        currentDate =
            fromLocalDateTime (Date.fromTime model.currentTime)
    in
    case model.activePage of
        DevicePage ->
            List.map MsgIndexedDb Pages.Device.Fetch.fetch

        UserPage (ClinicsPage clinicId) ->
            Pages.Clinics.Fetch.fetch clinicId
                |> List.map MsgIndexedDb

        UserPage (PersonPage id) ->
            Pages.Person.Fetch.fetch id model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (PersonsPage search relation) ->
            Pages.People.Fetch.fetch search relation
                |> List.map MsgIndexedDb

        UserPage (RelationshipPage id1 id2) ->
            Pages.Relationship.Fetch.fetch id1 id2 model.indexedDb
                |> List.map MsgIndexedDb

        UserPage AdminPage ->
            []

        {- TODO
           getLoggedInModel model
               |> Maybe.map
                   (\loggedIn ->
                       Pages.Admin.Fetch.fetch currentDate loggedIn.backend loggedIn.adminPage
                           |> List.map (MsgLoggedIn << MsgPageAdmin)
                   )
               |> Maybe.withDefault []
        -}
        UserPage (SessionPage sessionId sessionPage) ->
            Pages.Session.Fetch.fetch sessionId sessionPage model.indexedDb
                |> List.map MsgIndexedDb

        _ ->
            []


{-| Given a `Msg`, do we need to fetch the data it would fetch? We only answer
`True` if the data is `NotAsked`. So, we don't automatically re-fetch errors.

Note that the data need not literally be a `RemoteData`, but that will be
common. The answer does need to flip to `False` when a request is in progress,
or we will enter an infinite loop.

-}
shouldFetch : Model -> Msg -> Bool
shouldFetch model msg =
    case msg of
        MsgIndexedDb subMsg ->
            Backend.Fetch.shouldFetch model.indexedDb subMsg

        _ ->
            False


{-| Given a Msg that would fetch some data, forget that data.
-}
forget : Msg -> Model -> Model
forget msg model =
    case msg of
        MsgIndexedDb subMsg ->
            let
                subModel =
                    Backend.Fetch.forget subMsg model.indexedDb
            in
            { model | indexedDb = subModel }

        _ ->
            model


andThenFetch : (Msg -> Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenFetch update ( model, cmd ) =
    -- Note that we call ourselves recursively. So, it's vitally important that
    -- the `fetch` implementations use a `WebData`-like strategy to indicate
    -- that a request is in progress, and doesn't need to be triggered again.
    -- Otherwise, we'll immediately be in an infinite loop.
    --
    -- We initially sequence through the app's `update`, and only recurse once
    -- all the messages have been processed.
    let
        -- These are messages to fetch the data we want now, without
        -- considering whether we already have it.
        dataNowWanted =
            fetch model

        -- These are the messages we should actually issue to fetch data now.
        -- As an improvement, we could compare with `model.dataWanted` to see
        -- whether the msg is **newly** desired ... that is, whether its status
        -- just flipped. We could treat newly desired data differently. For
        -- instance, we might try to re-fetch it even in a `Failure` state.
        -- (Since that wouldn't infinitely repeat).
        dataToFetch =
            List.filter (shouldFetch model) dataNowWanted

        -- Update our existing dataWanted to indicate that the data now wanted
        -- was last wanted now.
        dataWanted =
            List.foldl (\msg -> EveryDict.insert msg model.currentTime) model.dataWanted dataNowWanted

        fiveMinutes =
            5 * 1000 * 60

        -- Figure out what to remember and what to forget.
        ( dataToForget, dataToRemember ) =
            EveryDict.partition (\_ lastWanted -> model.currentTime - lastWanted > fiveMinutes) dataWanted

        -- Our new base model, remembering the desired data, and forgetting
        -- the data to forget.
        newModel =
            dataToForget
                |> EveryDict.keys
                |> List.foldl forget { model | dataWanted = dataToRemember }
    in
    if List.isEmpty dataToFetch then
        ( newModel, cmd )

    else
        sequence update dataToFetch ( newModel, cmd )
            |> andThenFetch update
