module Pages.ChildScoreboard.Activity.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardActivity.Model exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNewbornExamPregnancySummary
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
        )
import Backend.Person.Model exposing (Person)
import Date
import EverySet
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model
    exposing
        ( ImmunisationTask(..)
        , NCDAData
        , VaccinationFormViewMode(..)
        , VaccinationProgressDict
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
import Pages.ChildScoreboard.Activity.Model exposing (..)
import Pages.ChildScoreboard.Activity.Utils exposing (..)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.ChildScoreboard.Utils exposing (generateVaccinationProgressDicts)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (isTaskCompleted, tasksBarId, viewLabel, viewPersonDetailsExtended, viewSaveAction)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ChildScoreboardEncounterId -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity child-scoreboard" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
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


viewContent : Language -> NominalDate -> ChildScoreboardActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> ChildScoreboardActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        ChildScoreboardNCDA ->
            viewNCDAContent language currentDate assembled db model.ncdaData

        ChildScoreboardVaccinationHistory ->
            viewImmunisationContent language currentDate assembled db model.immunisationData


viewNCDAContent :
    Language
    -> NominalDate
    -> AssembledData
    -> ModelIndexedDb
    -> NCDAData Msg
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
            , pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate personId assembled.person db
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setNumberANCVisitsMsg = SetNumberANCVisits
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
    -> AssembledData
    -> ModelIndexedDb
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

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
                        ( viewVaccinationForm language currentDate assembled vaccineType vaccinationForm
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


viewVaccinationForm : Language -> NominalDate -> AssembledData -> WellChildVaccineType -> ChildScoreboardVaccinationForm -> Html Msg
viewVaccinationForm language currentDate assembled vaccineType form =
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
                    [ div [ class "description" ] [ text <| translate language <| Translate.WellChildImmunisationDescription vaccineType ]
                    , div [ class "dosage" ] [ text <| translate language <| Translate.WellChildImmunisationDosage vaccineType ]
                    ]
                ]
            , viewLabel language (Translate.WellChildImmunisationHistory vaccineType)
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

                initialOpvAdministeredByProgress =
                    wasInitialOpvAdministeredByVaccinationProgress assembled.person assembled.vaccinationProgress

                initialOpvAdministered =
                    if form.administeredDosesDirty then
                        initialOpvAdministeredByForm

                    else
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
