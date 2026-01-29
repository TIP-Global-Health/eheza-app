module Pages.ChildScoreboard.Activity.View exposing (view)

import AssocList as Dict
import Backend.ChildScoreboardActivity.Model exposing (ChildScoreboardActivity(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNewbornExamPregnancySummary
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
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
        , immunisationTaskToVaccineType
        , initialVaccinationDateByBirthDate
        , ncdaFormWithDefault
        , nextVaccinationDataForVaccine
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm
        , wasInitialOpvAdministeredByVaccinationProgress
        )
import Measurement.View
import Pages.ChildScoreboard.Activity.Model exposing (ChildScoreboardVaccinationForm, ImmunisationData, Model, Msg(..))
import Pages.ChildScoreboard.Activity.Utils exposing (expectImmunisationTask, immunisationTasks)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , tasksBarId
        , viewLabel
        , viewPersonDetailsExtended
        , viewSaveAction
        )
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site id activity db model =
    let
        assembled =
            generateAssembledData site id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Site -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate site id activity db model assembled =
    div [ class "page-activity child-scoreboard" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate site activity db model assembled
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


viewContent : Language -> NominalDate -> Site -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate site activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate site activity assembled db model
        )


viewActivity : Language -> NominalDate -> Site -> ChildScoreboardActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate site activity assembled db model =
    case activity of
        ChildScoreboardNCDA ->
            viewNCDAContent language currentDate site assembled db model.ncdaData

        ChildScoreboardVaccinationHistory ->
            viewImmunisationContent language currentDate site assembled model.immunisationData


viewNCDAContent :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> ModelIndexedDb
    -> NCDAData
    -> List (Html Msg)
viewNCDAContent language currentDate site assembled db data =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        config =
            { atHealthCenter = False
            , showTasksTray = True
            , pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate personId assembled.person db
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStuntingLevelMsg = SetStuntingLevel
            , setWeightMsg = SetWeight
            , setMuacMsg = SetMuac
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg = SaveNCDA personId assembled.measurements.ncda
            }
    in
    Measurement.View.viewNCDAContent language
        currentDate
        site
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
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate site assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectImmunisationTask currentDate site assembled.person assembled.vaccinationHistory) immunisationTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

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

                        TaskDTPStandalone ->
                            ( "dtp-vaccine"
                            , isJust measurements.dtpStandaloneImmunisation
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
                    , text <| translate language (Translate.WellChildImmunisationTask site task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    ( task
                    , immunisationTasksCompletedFromTotal language
                        currentDate
                        site
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

                                    VaccineDTPStandalone ->
                                        measurements.dtpStandaloneImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.dtpStandaloneForm

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

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            personId =
                                assembled.participant.person

                            nextTask =
                                resolveNextTask task tasksCompletedFromTotalDict tasks

                            saveMsg =
                                case task of
                                    TaskBCG ->
                                        SaveBCGImmunisation personId measurements.bcgImmunisation nextTask

                                    TaskDTP ->
                                        SaveDTPImmunisation personId measurements.dtpImmunisation nextTask

                                    TaskDTPStandalone ->
                                        SaveDTPStandaloneImmunisation personId measurements.dtpStandaloneImmunisation nextTask

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
    -> Site
    -> AssembledData
    -> ImmunisationData
    -> Measurement.Model.ImmunisationTask
    -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate site assembled data task =
    Maybe.map
        (\vaccineType ->
            let
                form =
                    case vaccineType of
                        VaccineBCG ->
                            getMeasurementValueFunc assembled.measurements.bcgImmunisation
                                |> vaccinationFormWithDefault data.bcgForm

                        VaccineDTP ->
                            getMeasurementValueFunc assembled.measurements.dtpImmunisation
                                |> vaccinationFormWithDefault data.dtpForm

                        VaccineDTPStandalone ->
                            getMeasurementValueFunc assembled.measurements.dtpStandaloneImmunisation
                                |> vaccinationFormWithDefault data.dtpStandaloneForm

                        VaccineIPV ->
                            getMeasurementValueFunc assembled.measurements.ipvImmunisation
                                |> vaccinationFormWithDefault data.ipvForm

                        VaccineMR ->
                            getMeasurementValueFunc assembled.measurements.mrImmunisation
                                |> vaccinationFormWithDefault data.mrForm

                        VaccineOPV ->
                            getMeasurementValueFunc assembled.measurements.opvImmunisation
                                |> vaccinationFormWithDefault data.opvForm

                        VaccinePCV13 ->
                            getMeasurementValueFunc assembled.measurements.pcv13Immunisation
                                |> vaccinationFormWithDefault data.pcv13Form

                        VaccineRotarix ->
                            getMeasurementValueFunc assembled.measurements.rotarixImmunisation
                                |> vaccinationFormWithDefault data.rotarixForm

                        -- Vaccine type not in use.
                        VaccineHPV ->
                            emptyVaccinationForm

                ( _, tasksActive, tasksCompleted ) =
                    vaccinationFormDynamicContentAndTasks language
                        currentDate
                        site
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
            vaccinationFormDynamicContentAndTasks language currentDate site assembled vaccineType form
    in
    div [ class "ui form vaccination" ] <|
        [ h2 [] [ text <| translate language <| Translate.WellChildImmunisationHeader vaccineType ]
        , div [ class "instructions" ] <|
            [ div [ class "header icon-label" ] <|
                [ i [ class "icon-open-book" ] []
                , div []
                    [ div [ class "description" ] [ text <| translate language <| Translate.WellChildImmunisationDescription site vaccineType ]
                    , div [ class "dosage" ] [ text <| translate language <| Translate.WellChildImmunisationDosage site vaccineType ]
                    ]
                ]
            , viewLabel language (Translate.WellChildImmunizationHistory site vaccineType)
            ]
                ++ contentByViewMode
        ]


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> WellChildVaccineType
    -> ChildScoreboardVaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate site assembled vaccineType form =
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
                    , nextVaccinationDataForVaccine = nextVaccinationDataForVaccine site assembled.person.birthDate vaccineType initialOpvAdministered
                    , getIntervalForVaccine = always (getIntervalForVaccine site vaccineType)
                    , firstDoseExpectedFrom =
                        initialVaccinationDateByBirthDate site
                            birthDate
                            initialOpvAdministered
                            assembled.vaccinationProgress
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
                                wasInitialOpvAdministeredByVaccinationProgress assembled.person.birthDate assembled.vaccinationProgress
                        in
                        initialOpvAdministeredByForm || initialOpvAdministeredByProgress

                expectedDoses =
                    getAllDosesForVaccine site initialOpvAdministered vaccineType
                        |> List.filter
                            (\dose ->
                                expectVaccineDoseForPerson currentDate
                                    site
                                    assembled.person
                                    initialOpvAdministered
                                    assembled.vaccinationProgress
                                    ( vaccineType, dose )
                            )

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
            Measurement.Utils.vaccinationFormDynamicContentAndTasks language currentDate site config (WellChildVaccine vaccineType) form
        )
        assembled.person.birthDate
        |> Maybe.withDefault ( [], 0, 1 )
