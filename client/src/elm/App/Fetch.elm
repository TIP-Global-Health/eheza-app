module App.Fetch exposing (fetch, forget, shouldFetch)

import App.Model exposing (..)
import App.Utils exposing (getLoggedInData)
import Backend.Fetch
import Date
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Clinics.Fetch
import Pages.Device.Fetch
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.People.Fetch
import Pages.Person.Fetch
import Pages.PinCode.Fetch
import Pages.Relationship.Fetch
import Pages.Session.Fetch
import Time


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
            fromLocalDateTime model.currentTime
    in
    case model.activePage of
        DevicePage ->
            List.map MsgIndexedDb Pages.Device.Fetch.fetch

        PinCodePage ->
            List.map MsgIndexedDb Pages.PinCode.Fetch.fetch

        UserPage (ClinicsPage clinicId) ->
            Pages.Clinics.Fetch.fetch clinicId
                |> List.map MsgIndexedDb

        UserPage (CreatePersonPage relatedId) ->
            Pages.Person.Fetch.fetchForCreateOrEdit relatedId model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (EditPersonPage relatedId) ->
            Pages.Person.Fetch.fetchForCreateOrEdit (Just relatedId) model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (PersonPage id) ->
            Pages.Person.Fetch.fetch id model.indexedDb
                |> List.map MsgIndexedDb

        UserPage (PersonsPage relation) ->
            getLoggedInData model
                |> Maybe.map
                    (\( _, loggedIn ) ->
                        Pages.People.Fetch.fetch relation loggedIn.personsPage
                            |> List.map MsgIndexedDb
                    )
                |> Maybe.withDefault []

        UserPage (RelationshipPage id1 id2) ->
            Pages.Relationship.Fetch.fetch id1 id2 model.indexedDb
                |> List.map MsgIndexedDb

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
