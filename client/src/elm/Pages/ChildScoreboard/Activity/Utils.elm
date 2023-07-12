module Pages.ChildScoreboard.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, WellChildVaccineType(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , generateVaccinationProgressForVaccine
        , isTestResultValid
        , resolveLabTestDate
        , vitalsFormWithDefault
        )
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewNumberInput
        , viewQuestionLabel
        )
import Pages.WellChild.Activity.Types exposing (ImmunisationTask(..))
import Pages.WellChild.Encounter.Model exposing (VaccinationProgressDict)
import RemoteData exposing (RemoteData(..))
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> ChildScoreboardActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        ChildScoreboardNCDA ->
            True

        ChildScoreboardVaccinationHistory ->
            -- @todo:
            True


activityCompleted : NominalDate -> AssembledData -> ChildScoreboardActivity -> Bool
activityCompleted currentDate assembled activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate assembled activityToCheck
    in
    case activity of
        ChildScoreboardNCDA ->
            notExpected ChildScoreboardNCDA
                || isJust assembled.measurements.ncda

        ChildScoreboardVaccinationHistory ->
            notExpected ChildScoreboardVaccinationHistory
                || -- @todo:
                   False


generateVaccinationProgress : Person -> List ChildScoreboardMeasurements -> VaccinationProgressDict
generateVaccinationProgress person measurements =
    let
        bcgImmunisations =
            List.filterMap (.bcgImmunisation >> getMeasurementValueFunc)
                measurements

        dtpImmunisations =
            List.filterMap (.dtpImmunisation >> getMeasurementValueFunc)
                measurements

        ipvImmunisations =
            List.filterMap (.ipvImmunisation >> getMeasurementValueFunc)
                measurements

        mrImmunisations =
            List.filterMap (.mrImmunisation >> getMeasurementValueFunc)
                measurements

        opvImmunisations =
            List.filterMap (.opvImmunisation >> getMeasurementValueFunc)
                measurements

        pcv13Immunisations =
            List.filterMap (.pcv13Immunisation >> getMeasurementValueFunc)
                measurements

        rotarixImmunisations =
            List.filterMap (.rotarixImmunisation >> getMeasurementValueFunc)
                measurements
    in
    [ ( VaccineBCG, generateVaccinationProgressForVaccine bcgImmunisations )
    , ( VaccineOPV, generateVaccinationProgressForVaccine opvImmunisations )
    , ( VaccineDTP, generateVaccinationProgressForVaccine dtpImmunisations )
    , ( VaccinePCV13, generateVaccinationProgressForVaccine pcv13Immunisations )
    , ( VaccineRotarix, generateVaccinationProgressForVaccine rotarixImmunisations )
    , ( VaccineIPV, generateVaccinationProgressForVaccine ipvImmunisations )
    , ( VaccineMR, generateVaccinationProgressForVaccine mrImmunisations )
    ]
        |> Dict.fromList


immunisationVaccinationTasks : List ImmunisationTask
immunisationVaccinationTasks =
    [ TaskBCG
    , TaskOPV
    , TaskDTP
    , TaskPCV13
    , TaskRotarix
    , TaskIPV
    , TaskMR
    ]
