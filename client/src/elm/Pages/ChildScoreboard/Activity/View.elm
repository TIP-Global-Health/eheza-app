module Pages.ChildScoreboard.Activity.View exposing (view)

import AssocList as Dict
import Backend.ChildScoreboardActivity.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNewbornExamPregnancySummary
        , nutritionAssessmentForBackend
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
        , resolvePreviousValuesSetForChild
        )
import Date
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (isJust)
import Measurement.Model
    exposing
        ( ImmunisationTask(..)
        , NCDAData
        , VaccinationFormViewMode(..)
        , emptyVaccinationForm
        )
import Measurement.Utils
    exposing
        ( expectVaccineDoseForPerson
        , getAllDosesForVaccine
        , getIntervalForVaccine
        , heightFormWithDefault
        , immunisationTaskToVaccineType
        , initialVaccinationDateByBirthDate
        , muacFormWithDefault
        , ncdaFormWithDefault
        , nextVaccinationDataForVaccine
        , nutritionFormWithDefault
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm
        , wasInitialOpvAdministeredByVaccinationProgress
        , weightFormWithDefault
        )
import Measurement.View
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Activity.Utils exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.Nutrition.Activity.View exposing (viewHeightForm, viewMuacForm, viewNutritionForm, viewWeightForm)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (isTaskCompleted, tasksBarId, viewLabel, viewPersonDetailsExtended, viewSaveAction)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> Site -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores site id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site id activity db model) identity assembled


viewHeaderAndContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> ChildScoreboardEncounterId
    -> ChildScoreboardActivity
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate zscores site id activity db model assembled =
    div [ class "page-activity child-scoreboard" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate zscores site activity db model assembled
        ]


viewHeader : Language -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language <| Translate.ChildScoreboardActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| ChildScoreboardEncounterPage id
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> ChildScoreboardActivity
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate zscores site activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate zscores site activity assembled db model
        )


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> Site -> ChildScoreboardActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores site activity assembled db model =
    case activity of
        ChildScoreboardNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate zscores assembled db model.nutritionAssessmentData

        ChildScoreboardNCDA ->
            viewNCDAContent language currentDate assembled db model.ncdaData

        ChildScoreboardVaccinationHistory ->
            viewImmunisationContent language currentDate site assembled db model.immunisationData


viewNutritionAssessmenContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> AssembledData
    -> ModelIndexedDb
    -> NutritionAssessmentData
    -> List (Html Msg)
viewNutritionAssessmenContent language currentDate zscores assembled db data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNutritionAssessmentTask currentDate zscores assembled db) allNutritionAssessmentTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                iconClass =
                    case task of
                        TaskHeight ->
                            "height"

                        TaskMuac ->
                            "muac"

                        TaskNutrition ->
                            "nutrition"

                        TaskWeight ->
                            "weight"

                isCompleted =
                    nutritionAssessmentTaskCompleted currentDate zscores assembled db task

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNutritionAssessmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ChildScorecardNutritionAssessmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, nutritionAssessmentTasksCompletedFromTotal measurements data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        previousValuesSet =
            resolvePreviousValuesSetForChild currentDate assembled.participant.person db

        ( viewForm, assembledAfterSave ) =
            case activeTask of
                Just TaskHeight ->
                    let
                        form =
                            getMeasurementValueFunc measurements.height
                                |> heightFormWithDefault data.heightForm

                        -- We want to simulate value of height measurement after
                        -- form value is saved, so we can apply expectNutritionAssessmentTask()
                        -- to resolve offered tasks (with, or without Nutrition).
                        heightAfterSave =
                            Maybe.map
                                (\height ->
                                    case measurements.height of
                                        Just ( id, measurement ) ->
                                            -- If measurement already exists, we set form value.
                                            Just ( id, { measurement | value = HeightInCm height } )

                                        Nothing ->
                                            -- Otherwise, we simulate the measurement data.
                                            -- All we care about is value, so we can set dummy
                                            -- for everything else.
                                            Just
                                                ( toEntityUuid "dummy"
                                                , { dateMeasured = currentDate
                                                  , nurse = Nothing
                                                  , healthCenter = Nothing
                                                  , participantId = assembled.participant.person
                                                  , encounterId = Nothing
                                                  , value = HeightInCm height
                                                  }
                                                )
                                )
                                form.height
                                |> Maybe.withDefault measurements.height

                        measurementsAfterSave =
                            { measurements | height = heightAfterSave }
                    in
                    ( viewHeightForm language currentDate zscores assembled.person previousValuesSet.height SetHeight form
                    , { assembled | measurements = measurementsAfterSave }
                    )

                Just TaskMuac ->
                    let
                        form =
                            getMeasurementValueFunc measurements.muac
                                |> muacFormWithDefault data.muacForm

                        -- We want to simulate value of height measurement after
                        -- form value is saved, so we can apply expectNutritionAssessmentTask()
                        -- to resolve offered tasks (with, or without Nutrition).
                        muacAfterSave =
                            Maybe.map
                                (\muac ->
                                    case measurements.muac of
                                        Just ( id, measurement ) ->
                                            -- If measurement already exists, we set form value.
                                            Just ( id, { measurement | value = MuacInCm muac } )

                                        Nothing ->
                                            -- Otherwise, we simulate the measurement data.
                                            -- All we care about is value, so we can set dummy
                                            -- for everything else.
                                            Just
                                                ( toEntityUuid "dummy"
                                                , { dateMeasured = currentDate
                                                  , nurse = Nothing
                                                  , healthCenter = Nothing
                                                  , participantId = assembled.participant.person
                                                  , encounterId = Nothing
                                                  , value = MuacInCm muac
                                                  }
                                                )
                                )
                                form.muac
                                |> Maybe.withDefault measurements.muac

                        measurementsAfterSave =
                            { measurements | muac = muacAfterSave }
                    in
                    ( viewMuacForm language currentDate assembled.person previousValuesSet.muac SetMuac form
                    , { assembled | measurements = measurementsAfterSave }
                    )

                Just TaskNutrition ->
                    ( measurements.nutrition
                        |> getMeasurementValueFunc
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate SetNutritionSign
                    , assembled
                    )

                Just TaskWeight ->
                    let
                        heightValue =
                            assembled.measurements.height
                                |> getMeasurementValueFunc

                        form =
                            getMeasurementValueFunc measurements.weight
                                |> weightFormWithDefault data.weightForm

                        showWeightForHeightZScore =
                            False

                        -- We want to simulate value of height measurement after
                        -- form value is saved, so we can apply expectNutritionAssessmentTask()
                        -- to resolve offered tasks (with, or without Nutrition).
                        weightAfterSave =
                            Maybe.map
                                (\weight ->
                                    case measurements.weight of
                                        Just ( id, measurement ) ->
                                            -- If measurement already exists, we set form value.
                                            Just ( id, { measurement | value = WeightInKg weight } )

                                        Nothing ->
                                            -- Otherwise, we simulate the measurement data.
                                            -- All we care about is value, so we can set dummy
                                            -- for everything else.
                                            Just
                                                ( toEntityUuid "dummy"
                                                , { dateMeasured = currentDate
                                                  , nurse = Nothing
                                                  , healthCenter = Nothing
                                                  , participantId = assembled.participant.person
                                                  , encounterId = Nothing
                                                  , value = WeightInKg weight
                                                  }
                                                )
                                )
                                form.weight
                                |> Maybe.withDefault measurements.weight

                        measurementsAfterSave =
                            { measurements | weight = weightAfterSave }
                    in
                    ( viewWeightForm language
                        currentDate
                        zscores
                        assembled.person
                        heightValue
                        previousValuesSet.weight
                        showWeightForHeightZScore
                        SetWeight
                        form
                    , { assembled | measurements = measurementsAfterSave }
                    )

                Nothing ->
                    ( [], assembled )

        tasksAfterSave =
            List.filter (expectNutritionAssessmentTask currentDate zscores assembledAfterSave db) allNutritionAssessmentTasks

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasksAfterSave
                |> List.head

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        saveMsg =
                            case task of
                                TaskHeight ->
                                    SaveHeight personId measurements.height nextTask

                                TaskMuac ->
                                    SaveMuac personId measurements.muac nextTask

                                TaskNutrition ->
                                    let
                                        assessment =
                                            generateNutritionAssessment currentDate zscores db assembled
                                                |> nutritionAssessmentForBackend
                                    in
                                    SaveNutrition personId measurements.nutrition assessment nextTask

                                TaskWeight ->
                                    SaveWeight personId measurements.weight nextTask

                        disabled =
                            tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disabled
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewNCDAContent :
    Language
    -> NominalDate
    -> AssembledData
    -> ModelIndexedDb
    -> NCDAData
    -> List (Html Msg)
viewNCDAContent language currentDate assembled db data =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        config =
            { atHealthCenter = False
            , showTasksTray = True
            , behindOnVaccinations =
                generateSuggestedVaccinations currentDate
                    assembled.person
                    assembled.vaccinationHistory
                    |> List.isEmpty
                    |> not
                    |> Just
            , muacValue = getMeasurementValueFunc <| assembled.measurements.muac
            , pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate personId assembled.person db
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg = SaveNCDA personId assembled.measurements.ncda
            }
    in
    Measurement.View.viewNCDAContent language
        currentDate
        personId
        assembled.person
        config
        data.helperState
        form
        db


viewImmunisationContent :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> ModelIndexedDb
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate site assembled db data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectImmunisationTask currentDate assembled.person assembled.vaccinationHistory) immunisationTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskBCG ->
                            ( "bcg-vaccine"
                            , isJust measurements.bcgImmunisation
                            )

                        TaskDTP ->
                            ( "dtp-vaccine"
                            , isJust measurements.dtpImmunisation
                            )

                        TaskIPV ->
                            ( "ipv-vaccine"
                            , isJust measurements.ipvImmunisation
                            )

                        TaskMR ->
                            ( "mr-vaccine"
                            , isJust measurements.mrImmunisation
                            )

                        TaskOPV ->
                            ( "opv-vaccine"
                            , isJust measurements.opvImmunisation
                            )

                        TaskPCV13 ->
                            ( "pcv13-vaccine"
                            , isJust measurements.pcv13Immunisation
                            )

                        TaskRotarix ->
                            ( "rotarix-vaccine"
                            , isJust measurements.rotarixImmunisation
                            )

                        -- This task is not in use.
                        TaskHPV ->
                            ( ""
                            , False
                            )

                        -- This task is not in use.
                        TaskOverview ->
                            ( ""
                            , False
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveImmunisationTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.WellChildImmunisationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    ( task
                    , immunisationTasksCompletedFromTotal language
                        currentDate
                        assembled
                        data
                        task
                    )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                activeTask
                |> Maybe.withDefault ( 0, 0 )

        ( formForView, fullScreen, allowSave ) =
            Maybe.andThen immunisationTaskToVaccineType activeTask
                |> Maybe.map
                    (\vaccineType ->
                        let
                            vaccinationForm =
                                case vaccineType of
                                    VaccineBCG ->
                                        measurements.bcgImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.bcgForm

                                    VaccineDTP ->
                                        measurements.dtpImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.dtpForm

                                    VaccineIPV ->
                                        measurements.ipvImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.ipvForm

                                    VaccineMR ->
                                        measurements.mrImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.mrForm

                                    VaccineOPV ->
                                        measurements.opvImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.opvForm

                                    VaccinePCV13 ->
                                        measurements.pcv13Immunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.pcv13Form

                                    VaccineRotarix ->
                                        measurements.rotarixImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.rotarixForm

                                    -- Vaccine type not in use.
                                    VaccineHPV ->
                                        emptyVaccinationForm
                        in
                        ( viewVaccinationForm language currentDate site assembled vaccineType vaccinationForm
                        , False
                        , vaccinationForm.viewMode == ViewModeInitial
                        )
                    )
                |> Maybe.withDefault ( emptyNode, True, True )

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            personId =
                                assembled.participant.person

                            saveMsg =
                                case task of
                                    TaskBCG ->
                                        SaveBCGImmunisation personId measurements.bcgImmunisation nextTask

                                    TaskDTP ->
                                        SaveDTPImmunisation personId measurements.dtpImmunisation nextTask

                                    TaskIPV ->
                                        SaveIPVImmunisation personId measurements.ipvImmunisation nextTask

                                    TaskMR ->
                                        SaveMRImmunisation personId measurements.mrImmunisation nextTask

                                    TaskOPV ->
                                        SaveOPVImmunisation personId measurements.opvImmunisation nextTask

                                    TaskPCV13 ->
                                        SavePCV13Immunisation personId measurements.pcv13Immunisation nextTask

                                    TaskRotarix ->
                                        SaveRotarixImmunisation personId measurements.rotarixImmunisation nextTask

                                    TaskHPV ->
                                        NoOp

                                    TaskOverview ->
                                        NoOp

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewSaveAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
                |> showIf allowSave
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div
        [ classList
            [ ( "tasks-count", True )
            , ( "full-screen", fullScreen )
            ]
        ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div
        [ classList
            [ ( "ui full segment", True )
            , ( "full-screen", fullScreen )
            ]
        ]
        [ div [ class "full content" ]
            [ formForView
            , actions
            ]
        ]
    ]


immunisationTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> AssembledData
    -> ImmunisationData
    -> Measurement.Model.ImmunisationTask
    -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate assembled data task =
    Maybe.map
        (\vaccineType ->
            let
                form =
                    case vaccineType of
                        VaccineBCG ->
                            assembled.measurements.bcgImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.bcgForm

                        VaccineDTP ->
                            assembled.measurements.dtpImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.dtpForm

                        VaccineIPV ->
                            assembled.measurements.ipvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.ipvForm

                        VaccineMR ->
                            assembled.measurements.mrImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.mrForm

                        VaccineOPV ->
                            assembled.measurements.opvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.opvForm

                        VaccinePCV13 ->
                            assembled.measurements.pcv13Immunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.pcv13Form

                        VaccineRotarix ->
                            assembled.measurements.rotarixImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.rotarixForm

                        -- Vaccine type not in use.
                        VaccineHPV ->
                            emptyVaccinationForm

                ( _, tasksActive, tasksCompleted ) =
                    vaccinationFormDynamicContentAndTasks language
                        currentDate
                        assembled
                        vaccineType
                        form
            in
            ( tasksActive, tasksCompleted )
        )
        (immunisationTaskToVaccineType task)
        |> Maybe.withDefault ( 0, 0 )


viewVaccinationForm : Language -> NominalDate -> Site -> AssembledData -> WellChildVaccineType -> ChildScoreboardVaccinationForm -> Html Msg
viewVaccinationForm language currentDate site assembled vaccineType form =
    let
        ( contentByViewMode, _, _ ) =
            vaccinationFormDynamicContentAndTasks language currentDate assembled vaccineType form
    in
    div [ class "ui form vaccination" ] <|
        [ h2 [] [ text <| translate language <| Translate.WellChildImmunisationHeader vaccineType ]
        , div [ class "instructions" ] <|
            [ div [ class "header icon-label" ] <|
                [ i [ class "icon-open-book" ] []
                , div []
                    [ div [ class "description" ] [ text <| translate language <| Translate.WellChildImmunisationDescription site vaccineType ]
                    , div [ class "dosage" ] [ text <| translate language <| Translate.WellChildImmunisationDosage vaccineType ]
                    ]
                ]
            , viewLabel language (Translate.WellChildImmunizationHistory vaccineType)
            ]
                ++ contentByViewMode
        ]


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> WellChildVaccineType
    -> ChildScoreboardVaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate assembled vaccineType form =
    Maybe.map
        (\birthDate ->
            let
                config =
                    { birthDate = birthDate
                    , expectedDoses = expectedDoses
                    , dosesFromPreviousEncountersData = dosesFromPreviousEncountersData
                    , dosesFromCurrentEncounterData = dosesFromCurrentEncounterData
                    , setVaccinationFormViewModeMsg = SetVaccinationFormViewMode vaccineType
                    , setUpdatePreviousVaccinesMsg = SetUpdatePreviousVaccines vaccineType
                    , setWillReceiveVaccineTodayMsg = SetWillReceiveVaccineToday vaccineType
                    , setAdministrationNoteMsg = SetAdministrationNote vaccineType
                    , setVaccinationUpdateDateSelectorStateMsg = SetVaccinationUpdateDateSelectorState vaccineType
                    , setVaccinationUpdateDateMsg = SetVaccinationUpdateDate vaccineType
                    , saveVaccinationUpdateDateMsg = SaveVaccinationUpdateDate vaccineType
                    , deleteVaccinationUpdateDateMsg = DeleteVaccinationUpdateDate vaccineType
                    , nextVaccinationDataForVaccine = nextVaccinationDataForVaccine vaccineType initialOpvAdministered
                    , getIntervalForVaccine = always (getIntervalForVaccine vaccineType)
                    , firstDoseExpectedFrom =
                        initialVaccinationDateByBirthDate birthDate
                            initialOpvAdministered
                            ( vaccineType, VaccineDoseFirst )
                    , suggestDoseToday = False
                    }

                initialOpvAdministeredByForm =
                    wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm birthDate form

                initialOpvAdministered =
                    if form.administeredDosesDirty then
                        initialOpvAdministeredByForm

                    else
                        let
                            initialOpvAdministeredByProgress =
                                wasInitialOpvAdministeredByVaccinationProgress assembled.person assembled.vaccinationProgress
                        in
                        initialOpvAdministeredByForm || initialOpvAdministeredByProgress

                expectedDoses =
                    getAllDosesForVaccine initialOpvAdministered vaccineType
                        |> List.filter
                            (\dose -> expectVaccineDoseForPerson currentDate assembled.person initialOpvAdministered ( vaccineType, dose ))

                dosesFromPreviousEncountersData =
                    Dict.get vaccineType assembled.vaccinationHistory
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList

                dosesFromCurrentEncounterData =
                    Maybe.map2
                        (\doses dates ->
                            let
                                orderedDoses =
                                    EverySet.toList doses
                                        |> List.sortBy vaccineDoseToComparable

                                orderedDates =
                                    EverySet.toList dates
                                        |> List.sortWith Date.compare
                            in
                            List.Extra.zip orderedDoses orderedDates
                        )
                        form.administeredDoses
                        form.administrationDates
                        |> Maybe.withDefault []
            in
            Measurement.Utils.vaccinationFormDynamicContentAndTasks language currentDate config (WellChildVaccine vaccineType) form
        )
        assembled.person.birthDate
        |> Maybe.withDefault ( [], 0, 1 )
