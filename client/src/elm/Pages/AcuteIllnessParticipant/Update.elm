module Pages.AcuteIllnessParticipant.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllnessParticipant.Model exposing (..)


update : NominalDate -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate personId msg model =
    case msg of
        MsgBackend msgBackend ->
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb msgBackend ]
            )

        RecordIllnessOutcome ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
