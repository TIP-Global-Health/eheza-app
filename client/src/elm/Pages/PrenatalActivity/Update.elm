module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import EveryDict exposing (EveryDict)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD, fromLocalDateTime)
import Pages.PrenatalActivity.Model exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)
import RemoteData exposing (RemoteData(..), WebData)


update : PersonId -> PrenatalActivity -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update motherId activity db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
