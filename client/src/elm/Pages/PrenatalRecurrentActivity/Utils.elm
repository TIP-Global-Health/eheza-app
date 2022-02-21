module Pages.PrenatalRecurrentActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacIndication, muacValueFunc, prenatalLabExpirationPeriod, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..), PrenatalEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (Html)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (VitalsForm)
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.AcuteIllnessActivity.Utils exposing (getCurrentReasonForMedicationNonAdministration)
import Pages.PrenatalRecurrentActivity.Model exposing (..)
import Pages.PrenatalRecurrentActivity.Types exposing (..)
import Pages.PrenatalRecurrentEncounter.Model exposing (AssembledData)
import Pages.PrenatalRecurrentEncounter.Utils exposing (calculateEGAWeeks, isFirstEncounter)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)


expectActivity : NominalDate -> AssembledData -> PrenatalRecurrentActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        LabResults ->
            -- @todo
            True


activityCompleted : NominalDate -> AssembledData -> PrenatalRecurrentActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        LabResults ->
            -- @todo
            False
