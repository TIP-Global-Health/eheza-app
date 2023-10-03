module Pages.ChildScoreboard.Activity.Utils exposing (..)

import AssocList as Dict
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Measurement.Model
    exposing
        ( ChildScoreboardMeasurements
        , ColorAlertIndication(..)
        , HeightInCm(..)
        , NCDASign(..)
        , NutritionAssessment(..)
        , VaccinationValue
        , VaccineDose(..)
        , WeightInKg(..)
        , WellChildVaccineType(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (calculateZScoreWeightForAge)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInDays, ageInMonths)
import Date exposing (Unit(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (..)
import Measurement.Utils
    exposing
        ( generateFutureVaccinationsData
        , heightFormWithDefault
        , immunisationTaskToVaccineType
        , muacFormWithDefault
        , muacMeasurementIsOff
        , nutritionFormWithDefault
        , weightFormWithDefault
        )
import Measurement.View exposing (zScoreForHeightOrLength)
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.Utils exposing (taskCompleted)
import ZScore.Model
import ZScore.Utils exposing (zScoreLengthHeightForAge)


generateNutritionAssessment : NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> List NutritionAssessment
generateNutritionAssessment currentDate zscores db assembled =
    let
        measurements =
            assembled.measurements

        muacValue =
            getMeasurementValueFunc measurements.muac

        nutritionValue =
            getMeasurementValueFunc measurements.nutrition
                |> Maybe.map .signs

        weightValue =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> weightValueFunc
                )
                measurements.weight
    in
    Backend.NutritionEncounter.Utils.generateNutritionAssessment currentDate zscores assembled.participant.person muacValue nutritionValue weightValue False db


expectActivity : NominalDate -> AssembledData -> ChildScoreboardActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        ChildScoreboardNutritionAssessment ->
            True

        ChildScoreboardNCDA ->
            True

        ChildScoreboardVaccinationHistory ->
            let
                childBehindOnVaccinationByVaccinaitonHistory =
                    generateSuggestedVaccinations currentDate
                        assembled.person
                        assembled.vaccinationHistory
                        |> List.isEmpty
                        |> not

                childUpToDateByNCDAResponse =
                    getMeasurementValueFunc assembled.measurements.ncda
                        |> Maybe.map (.signs >> EverySet.member ChildBehindOnVaccination >> not)
                        |> Maybe.withDefault False
            in
            childBehindOnVaccinationByVaccinaitonHistory && childUpToDateByNCDAResponse


activityCompleted : NominalDate -> ZScore.Model.Model -> AssembledData -> ModelIndexedDb -> ChildScoreboardActivity -> Bool
activityCompleted currentDate zscores assembled db activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate assembled activityToCheck
    in
    case activity of
        ChildScoreboardNutritionAssessment ->
            notExpected ChildScoreboardNutritionAssessment
                || List.all (nutritionAssessmentTaskCompleted currentDate zscores assembled db) allNutritionAssessmentTasks

        ChildScoreboardNCDA ->
            notExpected ChildScoreboardNCDA
                || isJust assembled.measurements.ncda

        ChildScoreboardVaccinationHistory ->
            notExpected ChildScoreboardVaccinationHistory
                || List.all (immunisationTaskCompleted currentDate assembled db) immunisationTasks


immunisationTaskCompleted : NominalDate -> AssembledData -> ModelIndexedDb -> Measurement.Model.ImmunisationTask -> Bool
immunisationTaskCompleted currentDate assembled db task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectImmunisationTask currentDate assembled.person assembled.vaccinationHistory
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
    -> List ( WellChildVaccineType, VaccineDose )
generateSuggestedVaccinations currentDate person vaccinationHistory =
    Measurement.Utils.generateSuggestedVaccinations currentDate False person vaccinationHistory


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


allNutritionAssessmentTasks : List NutritionAssessmentTask
allNutritionAssessmentTasks =
    [ TaskHeight, TaskMuac, TaskWeight, TaskNutrition ]


nutritionAssessmentTaskCompleted : NominalDate -> ZScore.Model.Model -> AssembledData -> ModelIndexedDb -> NutritionAssessmentTask -> Bool
nutritionAssessmentTaskCompleted currentDate zscores assembled db task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectNutritionAssessmentTask currentDate zscores assembled db
    in
    case task of
        TaskHeight ->
            (not <| taskExpected TaskHeight) || isJust measurements.height

        TaskMuac ->
            (not <| taskExpected TaskMuac) || isJust measurements.muac

        TaskNutrition ->
            (not <| taskExpected TaskNutrition) || isJust measurements.nutrition

        TaskWeight ->
            (not <| taskExpected TaskWeight) || isJust measurements.weight


expectNutritionAssessmentTask : NominalDate -> ZScore.Model.Model -> AssembledData -> ModelIndexedDb -> NutritionAssessmentTask -> Bool
expectNutritionAssessmentTask currentDate zscores assembled db task =
    case task of
        TaskHeight ->
            True

        -- Show for children that are at least 6 months old.
        TaskMuac ->
            ageInMonths currentDate assembled.person
                |> Maybe.map (\ageMonths -> ageMonths > 5)
                |> Maybe.withDefault False

        -- Any of other nutrition measurements is off.
        TaskNutrition ->
            List.filter (expectNutritionAssessmentTask currentDate zscores assembled db)
                [ TaskHeight, TaskMuac, TaskWeight ]
                |> List.any (nutritionMeasurementIsOff currentDate zscores assembled)

        TaskWeight ->
            True


nutritionMeasurementIsOff : NominalDate -> ZScore.Model.Model -> AssembledData -> NutritionAssessmentTask -> Bool
nutritionMeasurementIsOff currentDate zscores assembled task =
    case task of
        -- Height Z-score is bellow -2.
        TaskHeight ->
            Maybe.Extra.andThen2
                (\ageInDays (HeightInCm height) ->
                    zScoreLengthHeightForAge zscores ageInDays assembled.person.gender (ZScore.Model.Centimetres height)
                )
                (ageInDays currentDate assembled.person |> Maybe.map ZScore.Model.Days)
                (getMeasurementValueFunc assembled.measurements.height)
                |> Maybe.map (\score -> score < -2)
                |> Maybe.withDefault False

        -- MUAC is not green.
        TaskMuac ->
            getMeasurementValueFunc assembled.measurements.muac
                |> muacMeasurementIsOff

        TaskNutrition ->
            -- We don't need to check if Nutrition is off or not, so we
            -- set it to be ok.
            False

        -- Any of weight Z-scores is bellow -2.
        TaskWeight ->
            let
                zScoreForAgeOff =
                    Maybe.andThen (\(WeightInKg weight) -> calculateZScoreWeightForAge currentDate zscores assembled.person (Just weight))
                        (getMeasurementValueFunc assembled.measurements.weight)
                        |> Maybe.map (\score -> score < -2)
                        |> Maybe.withDefault False

                zScoreForHeightOff =
                    Maybe.Extra.andThen3
                        (\ageInDays (HeightInCm height) (WeightInKg weight) ->
                            zScoreForHeightOrLength zscores ageInDays (ZScore.Model.Centimetres height) assembled.person.gender weight
                        )
                        (ageInDays currentDate assembled.person |> Maybe.map ZScore.Model.Days)
                        (getMeasurementValueFunc assembled.measurements.height)
                        (getMeasurementValueFunc assembled.measurements.weight)
                        |> Maybe.map (\score -> score < -2)
                        |> Maybe.withDefault False
            in
            zScoreForAgeOff || zScoreForHeightOff


nutritionAssessmentTasksCompletedFromTotal : ChildScoreboardMeasurements -> NutritionAssessmentData -> NutritionAssessmentTask -> ( Int, Int )
nutritionAssessmentTasksCompletedFromTotal measurements assembled task =
    case task of
        TaskHeight ->
            let
                form =
                    measurements.height
                        |> getMeasurementValueFunc
                        |> heightFormWithDefault assembled.heightForm
            in
            ( taskCompleted form.height
            , 1
            )

        TaskMuac ->
            let
                form =
                    measurements.muac
                        |> getMeasurementValueFunc
                        |> muacFormWithDefault assembled.muacForm
            in
            ( taskCompleted form.muac
            , 1
            )

        TaskNutrition ->
            let
                form =
                    measurements.nutrition
                        |> getMeasurementValueFunc
                        |> nutritionFormWithDefault assembled.nutritionForm
            in
            ( taskCompleted form.signs
            , 1
            )

        TaskWeight ->
            let
                form =
                    measurements.weight
                        |> getMeasurementValueFunc
                        |> weightFormWithDefault assembled.weightForm
            in
            ( taskCompleted form.weight
            , 1
            )
