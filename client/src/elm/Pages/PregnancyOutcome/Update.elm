module Pages.PregnancyOutcome.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Pages.PregnancyOutcome.Model exposing (..)


update : NominalDate -> PrenatalParticipantId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
