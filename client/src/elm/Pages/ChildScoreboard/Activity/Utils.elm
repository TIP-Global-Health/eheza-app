module Pages.ChildScoreboard.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
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
