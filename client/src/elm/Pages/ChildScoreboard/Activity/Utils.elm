module Pages.ChildScoreboard.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (ChildScoreboardMeasurements, VaccinationValue, VaccineDose(..), WellChildVaccineType(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Date exposing (Unit(..))
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
        , generateFutureVaccinationsData
        , generateVaccinationProgressForVaccine
        , getPreviousMeasurements
        , immunisationTaskToVaccineType
        , isTestResultValid
        , mergeVaccinationProgressDicts
        , resolveLabTestDate
        , vitalsFormWithDefault
        )
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
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


expectImmunisationTask : NominalDate -> Person -> VaccinationProgressDict -> ImmunisationTask -> Bool
expectImmunisationTask currentDate person vaccinationHistory task =
    let
        futureVaccinations =
            generateFutureVaccinationsData currentDate person False vaccinationHistory
                |> Dict.fromList

        ageInWeeks =
            Maybe.map
                (\birthDate ->
                    Date.diff Weeks birthDate currentDate
                )
                person.birthDate

        isTaskExpected vaccineType =
            Dict.get vaccineType futureVaccinations
                |> Maybe.Extra.join
                |> Maybe.map
                    (\( dose, date ) ->
                        let
                            defaultCondition =
                                not <| Date.compare date currentDate == GT
                        in
                        if vaccineType == VaccineOPV then
                            case dose of
                                VaccineDoseFirst ->
                                    Maybe.map
                                        (\ageWeeks ->
                                            -- First dose of OPV vaccine is given within first 2
                                            -- weeks from birth, or, starting from 6 weeks after birth.
                                            -- In latter case, there're only 3 doses, and not 4.
                                            if ageWeeks >= 2 && ageWeeks <= 5 then
                                                False

                                            else
                                                defaultCondition
                                        )
                                        ageInWeeks
                                        |> Maybe.withDefault False

                                VaccineDoseSecond ->
                                    Maybe.map
                                        (\ageWeeks ->
                                            -- Second dose of OPV vaccine is given starting from
                                            -- 6 weeks after birth.
                                            if ageWeeks < 6 then
                                                False

                                            else
                                                defaultCondition
                                        )
                                        ageInWeeks
                                        |> Maybe.withDefault False

                                _ ->
                                    defaultCondition

                        else
                            defaultCondition
                    )
                |> Maybe.withDefault False
    in
    immunisationTaskToVaccineType task
        |> Maybe.map isTaskExpected
        -- Only task that is not converted to vaccine type
        -- is 'Overview', which we allways show.
        |> Maybe.withDefault True


generateSuggestedVaccinations :
    NominalDate
    -> Person
    -> VaccinationProgressDict
    -> VaccinationProgressDict
    -> List ( WellChildVaccineType, VaccineDose )
generateSuggestedVaccinations currentDate person vaccinationHistory vaccinationProgress =
    Measurement.Utils.generateSuggestedVaccinations currentDate False person vaccinationHistory vaccinationProgress


generateVaccinationProgress : List ChildScoreboardMeasurements -> VaccinationProgressDict
generateVaccinationProgress measurements =
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


immunisationTasks : List ImmunisationTask
immunisationTasks =
    [ TaskBCG
    , TaskOPV
    , TaskDTP
    , TaskPCV13
    , TaskRotarix
    , TaskIPV
    , TaskMR
    ]


getFormByVaccineTypeFunc : WellChildVaccineType -> (ImmunisationData -> ChildScoreboardVaccinationForm)
getFormByVaccineTypeFunc vaccineType =
    case vaccineType of
        VaccineBCG ->
            .bcgForm

        VaccineDTP ->
            .dtpForm

        VaccineIPV ->
            .ipvForm

        VaccineMR ->
            .mrForm

        VaccineOPV ->
            .opvForm

        VaccinePCV13 ->
            .pcv13Form

        VaccineRotarix ->
            .rotarixForm

        -- Vaccine type not in use.
        VaccineHPV ->
            always emptyVaccinationForm


updateVaccinationFormByVaccineType : WellChildVaccineType -> ChildScoreboardVaccinationForm -> ImmunisationData -> ImmunisationData
updateVaccinationFormByVaccineType vaccineType form data =
    case vaccineType of
        VaccineBCG ->
            { data | bcgForm = form }

        VaccineDTP ->
            { data | dtpForm = form }

        VaccineIPV ->
            { data | ipvForm = form }

        VaccineMR ->
            { data | mrForm = form }

        VaccineOPV ->
            { data | opvForm = form }

        VaccinePCV13 ->
            { data | pcv13Form = form }

        VaccineRotarix ->
            { data | rotarixForm = form }

        -- Vaccine type not in use.
        VaccineHPV ->
            data


getMeasurementByVaccineTypeFunc : WellChildVaccineType -> ChildScoreboardMeasurements -> Maybe VaccinationValue
getMeasurementByVaccineTypeFunc vaccineType measurements =
    case vaccineType of
        VaccineBCG ->
            measurements.bcgImmunisation
                |> getMeasurementValueFunc

        VaccineDTP ->
            measurements.dtpImmunisation
                |> getMeasurementValueFunc

        VaccineIPV ->
            measurements.ipvImmunisation
                |> getMeasurementValueFunc

        VaccineMR ->
            measurements.mrImmunisation
                |> getMeasurementValueFunc

        VaccineOPV ->
            measurements.opvImmunisation
                |> getMeasurementValueFunc

        VaccinePCV13 ->
            measurements.pcv13Immunisation
                |> getMeasurementValueFunc

        VaccineRotarix ->
            measurements.rotarixImmunisation
                |> getMeasurementValueFunc

        -- Vaccine type not in use.
        VaccineHPV ->
            Nothing
