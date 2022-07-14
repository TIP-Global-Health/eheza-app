module Pages.PatientRecord.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)


update : NominalDate -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }
            , Cmd.none
            , []
            )

        SetViewMode mode ->
            ( { model | viewMode = mode }
            , Cmd.none
            , []
            )

        SetFilter filter ->
            if filter == FilterDemographics then
                update currentDate id (SetActivePage <| UserPage <| DemographicsReportPage (InitiatorPatientRecord id) id) model

            else
                ( { model | filter = filter }
                , Cmd.none
                , []
                )

        NoOp ->
            ( model
            , Cmd.none
            , []
            )
