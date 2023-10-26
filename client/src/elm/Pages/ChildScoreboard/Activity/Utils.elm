module Pages.ChildScoreboard.Activity.Utils exposing (..)

import AssocList as Dict
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Measurement.Model
    exposing
        ( ChildScoreboardMeasurements
        , NCDASign(..)
        , VaccinationValue
        , VaccineDose(..)
        , WellChildVaccineType(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (..)
import Measurement.Utils
    exposing
        ( generateFutureVaccinationsData
        , immunisationTaskToVaccineType
        )
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import SyncManager.Model exposing (Site)


expectActivity : NominalDate -> Site -> AssembledData -> ChildScoreboardActivity -> Bool
expectActivity currentDate site assembled activity =
    case activity of
        ChildScoreboardNCDA ->
            True

        ChildScoreboardVaccinationHistory ->
            let
                childBehindOnVaccinationByVaccinaitonHistory =
                    generateSuggestedVaccinations currentDate
                        site
                        assembled.person
                        assembled.vaccinationHistory
                        assembled.vaccinationProgress
                        |> List.isEmpty
                        |> not

                childUpToDateByNCDAResponse =
                    getMeasurementValueFunc assembled.measurements.ncda
                        |> Maybe.map (.signs >> EverySet.member ChildBehindOnVaccination >> not)
                        |> Maybe.withDefault False
            in
            childBehindOnVaccinationByVaccinaitonHistory && childUpToDateByNCDAResponse


activityCompleted : NominalDate -> Site -> AssembledData -> ModelIndexedDb -> ChildScoreboardActivity -> Bool
activityCompleted currentDate site assembled db activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate site assembled activityToCheck
    in
    case activity of
        ChildScoreboardNCDA ->
            notExpected ChildScoreboardNCDA
                || isJust assembled.measurements.ncda

        ChildScoreboardVaccinationHistory ->
            notExpected ChildScoreboardVaccinationHistory
                || List.all (immunisationTaskCompleted currentDate site assembled db) immunisationTasks


immunisationTaskCompleted : NominalDate -> Site -> AssembledData -> ModelIndexedDb -> Measurement.Model.ImmunisationTask -> Bool
immunisationTaskCompleted currentDate site assembled db task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectImmunisationTask currentDate site assembled.person assembled.vaccinationHistory
    in
    case task of
        TaskBCG ->
            (not <| taskExpected TaskBCG) || isJust measurements.bcgImmunisation

        TaskDTP ->
            (not <| taskExpected TaskDTP) || isJust measurements.dtpImmunisation

        TaskIPV ->
            (not <| taskExpected TaskIPV) || isJust measurements.ipvImmunisation

        TaskMR ->
            (not <| taskExpected TaskMR) || isJust measurements.mrImmunisation

        TaskOPV ->
            (not <| taskExpected TaskOPV) || isJust measurements.opvImmunisation

        TaskPCV13 ->
            (not <| taskExpected TaskPCV13) || isJust measurements.pcv13Immunisation

        TaskRotarix ->
            (not <| taskExpected TaskRotarix) || isJust measurements.rotarixImmunisation

        -- Task not in use.
        TaskHPV ->
            not <| taskExpected TaskHPV

        -- Task not in use.
        TaskOverview ->
            not <| taskExpected TaskOverview


expectImmunisationTask : NominalDate -> Site -> Person -> VaccinationProgressDict -> ImmunisationTask -> Bool
expectImmunisationTask currentDate site person vaccinationHistory task =
    let
        futureVaccinations =
            generateFutureVaccinationsData currentDate site person False vaccinationHistory
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
    -> Site
    -> Person
    -> VaccinationProgressDict
    -> VaccinationProgressDict
    -> List ( WellChildVaccineType, VaccineDose )
generateSuggestedVaccinations currentDate site person vaccinationHistory vaccinationProgress =
    Measurement.Utils.generateSuggestedVaccinations currentDate site False person vaccinationHistory vaccinationProgress


generateVaccinationProgress : List ChildScoreboardMeasurements -> VaccinationProgressDict
generateVaccinationProgress =
    Measurement.Utils.generateVaccinationProgressForChildScoreboard


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

        VaccineDTPStandalone ->
            .dtpStandaloneForm

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

        VaccineDTPStandalone ->
            { data | dtpStandaloneForm = form }

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

        VaccineDTPStandalone ->
            measurements.dtpStandaloneImmunisation
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
