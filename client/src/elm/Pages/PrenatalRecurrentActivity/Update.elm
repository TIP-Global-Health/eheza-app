module Pages.PrenatalRecurrentActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Decoder exposing (pregnancyTestResultFromString)
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BreastExamSign(..)
        , CSectionReason(..)
        , DangerSign(..)
        , FamilyPlanningSign(..)
        , HandsCPESign(..)
        , LegsCPESign(..)
        , LungsCPESign(..)
        , NeckCPESign(..)
        , PhotoUrl(..)
        , PostpartumChildDangerSign(..)
        , PostpartumMotherDangerSign(..)
        , PreviousDeliveryPeriod(..)
        , SocialHistoryHivTestingResult(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, prenatalTestResultFromString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (toSendToHCValueWithDefault, toVitalsValueWithDefault)
import Pages.AcuteIllnessActivity.Utils exposing (getCurrentReasonForMedicationNonAdministration, nonAdministrationReasonToSign)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalRecurrentActivity.Model exposing (..)
import Pages.PrenatalRecurrentActivity.Types exposing (..)
import Pages.PrenatalRecurrentActivity.Utils exposing (..)
import Pages.Utils exposing (setMultiSelectInputValue, tasksBarId)
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )
