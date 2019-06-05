module Pages.PrenatalEncounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import EveryDict exposing (EveryDict)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD, fromLocalDateTime)
import Pages.PrenatalEncounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


update : PersonId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update motherId db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
