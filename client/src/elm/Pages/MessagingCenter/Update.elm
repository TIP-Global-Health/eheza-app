module Pages.MessagingCenter.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Pages.MessagingCenter.Model exposing (..)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
