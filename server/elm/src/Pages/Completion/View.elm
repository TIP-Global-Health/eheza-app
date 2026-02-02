module Pages.Completion.View exposing (view)

import App.Types exposing (Language, Site)
import Backend.Completion.Model
    exposing
        ( AcuteIllnessActivity
        , ChildScoreboardActivity
        , CompletionData
        , EncounterData
        , HIVActivity
        , HomeVisitActivity
        , NCDActivity
        , NutritionChildActivity
        , NutritionGroupEncounterData
        , NutritionMotherActivity
        , PrenatalActivity
        , SelectedEntity(..)
        , TakenBy(..)
        , TuberculosisActivity
        , WellChildActivity
        , WellChildEncounterData
        , WellChildEncounterType(..)
        )
import Backend.Completion.Utils exposing (takenByToString)
import Backend.Model exposing (ModelBackend)
import Date exposing (Date)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Completion.Model exposing (Model, Msg(..), ReportType(..))
import Pages.Completion.Utils exposing (allAcuteIllnessActivities, allHIVActivities, allHomeVisitActivities, allNCDActivities, allNutritionChildGroupActivities, allNutritionIndividualActivities, allNutritionMotherGroupActivities, allPrenatalActivities, allTuberculosisActivities, newbornExamActivities, reportTypeToString, resolveChildScoreboardActivities, resolveSPVActivities)
import Pages.Components.View exposing (viewMetricsResultsTable)
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (calculatePercentage, launchDate, viewCustomSelectListInput, viewSelectListInput, wrapSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    case modelBackend.completionData of
        Just (Ok data) ->
            viewCompletionData language currentDate data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewCompletionData : Language -> NominalDate -> CompletionData -> Model -> Html Msg
viewCompletionData language currentDate data model =
    let
        topBar =
            let
                scopeLabel =
                    case data.entityType of
                        EntityGlobal ->
                            translate language Translate.Global

                        EntityHealthCenter ->
                            data.entityName
            in
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/completion" ]
                        [ button []
                            [ text <| translate language Translate.NewScope ]
                        ]
                    ]
                , div [ class "scope" ]
                    [ text <| translate language Translate.Scope ++ ": " ++ scopeLabel ]
                ]

        takenByInput =
            Maybe.map
                (\reportType ->
                    if
                        List.member reportType
                            [ -- Exclusively CHW encounters.
                              ReportChildScoreboard
                            , ReportHIV
                            , ReportHomeVisit
                            , ReportNewbornExam
                            , ReportTuberculosis

                            -- Exclusively Nurse encounters.
                            , ReportNCD
                            ]
                    then
                        emptyNode

                    else if isJust model.reportType then
                        let
                            options =
                                List.map
                                    (\option ->
                                        ( translate language <| Translate.TakenBy option, option )
                                    )
                                    [ TakenByNurse, TakenByCHW ]
                        in
                        viewCustomSelectListInput
                            model.takenBy
                            options
                            takenByToString
                            SetTakenBy
                            "select-input"
                            (Just <| translate language Translate.Any)
                            |> wrapSelectListInput language Translate.TakenByLabel False

                    else
                        emptyNode
                )
                model.reportType
                |> Maybe.withDefault emptyNode

        dateInputs =
            Maybe.map
                (\_ ->
                    let
                        startDateInput =
                            let
                                dateSelectorConfig =
                                    { select = SetStartDate
                                    , close = SetStartDateSelectorState Nothing
                                    , dateFrom = launchDate
                                    , dateTo = currentDate
                                    , dateDefault = Just launchDate
                                    }

                                dateForView =
                                    Maybe.map formatDDMMYYYY model.startDate
                                        |> Maybe.withDefault ""
                            in
                            div
                                [ class "form-input date"
                                , onClick <| SetStartDateSelectorState (Just dateSelectorConfig)
                                ]
                                [ text dateForView ]
                                |> wrapSelectListInput language Translate.SelectStartDate False

                        limitDateInput =
                            if
                                -- Reports requires setting start date before
                                -- limit date can be shown.
                                isNothing model.startDate
                            then
                                emptyNode

                            else
                                let
                                    dateFrom =
                                        Maybe.withDefault launchDate model.startDate

                                    dateSelectorConfig =
                                        { select = SetLimitDate
                                        , close = SetLimitDateSelectorState Nothing
                                        , dateFrom = dateFrom
                                        , dateTo = currentDate
                                        , dateDefault = Just currentDate
                                        }

                                    limitDateForView =
                                        Maybe.map formatDDMMYYYY model.limitDate
                                            |> Maybe.withDefault ""
                                in
                                div
                                    [ class "form-input date"
                                    , onClick <| SetLimitDateSelectorState (Just dateSelectorConfig)
                                    ]
                                    [ text limitDateForView ]
                                    |> wrapSelectListInput language Translate.SelectLimitDate False
                    in
                    [ startDateInput, limitDateInput ]
                )
                model.reportType
                |> Maybe.withDefault []

        content =
            if
                isJust model.startDateSelectorPopupState
                    || isJust model.limitDateSelectorPopupState
            then
                -- Date selector is open, so no need to calculate
                -- intermediate results.
                emptyNode

            else
                Maybe.map3
                    (\reportType startDate limitDate ->
                        let
                            ( newbornExamData, spvData ) =
                                List.partition (.encounterType >> (==) NewbornExam) data.wellChildData
                        in
                        case reportType of
                            ReportAcuteIllness ->
                                viewAcuteIllnessReport language startDate limitDate model.takenBy data.acuteIllnessData

                            ReportChildScoreboard ->
                                viewChildScoreboardReport language data.site startDate limitDate model.takenBy data.childScoreboardData

                            ReportHIV ->
                                viewHIVReport language startDate limitDate model.takenBy data.hivData

                            ReportHomeVisit ->
                                viewHomeVisitReport language startDate limitDate model.takenBy data.homeVisitData

                            ReportNCD ->
                                viewNCDReport language startDate limitDate model.takenBy data.ncdData

                            ReportNewbornExam ->
                                viewNewbornExamReport language startDate limitDate model.takenBy newbornExamData

                            ReportNutritionGroup ->
                                viewNutritionGroupReport language startDate limitDate model.takenBy data.nutritionGroupData

                            ReportNutritionIndividual ->
                                viewNutritionIndividualReport language startDate limitDate model.takenBy data.nutritionIndividualData

                            ReportPrenatal ->
                                viewPrenatalReport language startDate limitDate model.takenBy data.prenatalData

                            ReportTuberculosis ->
                                viewTuberculosisReport language startDate limitDate model.takenBy data.tuberculosisData

                            ReportWellChild ->
                                viewSPVReport language data.site startDate limitDate model.takenBy spvData
                    )
                    model.reportType
                    model.startDate
                    model.limitDate
                    |> Maybe.withDefault emptyNode
    in
    div [ class "page-content completion" ]
        [ topBar
        , div [ class "inputs" ] <|
            [ viewSelectListInput language
                model.reportType
                [ ReportAcuteIllness
                , ReportPrenatal
                , ReportChildScoreboard
                , ReportHIV
                , ReportHomeVisit
                , ReportNCD
                , ReportNewbornExam
                , ReportNutritionGroup
                , ReportNutritionIndividual
                , ReportWellChild
                , ReportTuberculosis
                ]
                reportTypeToString
                SetReportType
                Translate.CompletionReportType
                "select-input"
                |> wrapSelectListInput language Translate.ReportTypeLabel False
            , takenByInput
            ]
                ++ dateInputs
                ++ [ content ]
        , viewModal <| viewCalendarPopup language model.startDateSelectorPopupState model.startDate
        , viewModal <| viewCalendarPopup language model.limitDateSelectorPopupState model.limitDate
        ]


viewNutritionIndividualReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData NutritionChildActivity) -> Html Msg
viewNutritionIndividualReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateNutritionIndividualReportData language
        |> viewMetricsResultsTable
        |> div [ class "report nutrition-individual" ]


viewNutritionGroupReport :
    Language
    -> NominalDate
    -> NominalDate
    -> Maybe TakenBy
    -> List (NutritionGroupEncounterData NutritionMotherActivity NutritionChildActivity)
    -> Html Msg
viewNutritionGroupReport language startDate limitDate mTakenBy reportData =
    applyFilters startDate limitDate mTakenBy reportData
        |> generateNutritionGroupReportData language
        |> viewMetricsResultsTable
        |> div [ class "report nutrition-group" ]


viewAcuteIllnessReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData AcuteIllnessActivity) -> Html Msg
viewAcuteIllnessReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateAcuteIllnessReportData language
        |> viewMetricsResultsTable
        |> div [ class "report acute-illness" ]


viewSPVReport : Language -> Site -> NominalDate -> NominalDate -> Maybe TakenBy -> List WellChildEncounterData -> Html Msg
viewSPVReport language site startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> customApplyFilters startDate
            limitDate
            (\encounter ->
                if encounter.encounterType == PediatricCare then
                    TakenByNurse

                else
                    TakenByCHW
            )
            mTakenBy
        |> generateWellChildReportData language Translate.StandardPediatricVisit (resolveSPVActivities site)
        |> viewMetricsResultsTable
        |> div [ class "report well-child" ]


viewNewbornExamReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List WellChildEncounterData -> Html Msg
viewNewbornExamReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> customApplyFilters startDate
            limitDate
            (always TakenByCHW)
            mTakenBy
        |> generateWellChildReportData language Translate.NewbornExam newbornExamActivities
        |> viewMetricsResultsTable
        |> div [ class "report well-child" ]


viewHomeVisitReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData HomeVisitActivity) -> Html Msg
viewHomeVisitReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateHomeVisitReportData language
        |> viewMetricsResultsTable
        |> div [ class "report home-visit" ]


viewChildScoreboardReport : Language -> Site -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData ChildScoreboardActivity) -> Html Msg
viewChildScoreboardReport language site startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateChildScoreboardReportData language (resolveChildScoreboardActivities site)
        |> viewMetricsResultsTable
        |> div [ class "report child-scoreboard" ]


viewNCDReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData NCDActivity) -> Html Msg
viewNCDReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateNCDReportData language
        |> viewMetricsResultsTable
        |> div [ class "report ncd" ]


viewHIVReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData HIVActivity) -> Html Msg
viewHIVReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateHIVReportData language
        |> viewMetricsResultsTable
        |> div [ class "report hiv" ]


viewTuberculosisReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData TuberculosisActivity) -> Html Msg
viewTuberculosisReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateTuberculosisReportData language
        |> viewMetricsResultsTable
        |> div [ class "report tuberculosis" ]


viewPrenatalReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData PrenatalActivity) -> Html Msg
viewPrenatalReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generatePrenatalReportData language
        |> viewMetricsResultsTable
        |> div [ class "report prenatal" ]


eliminateEmptyEncounters :
    List { c | completion : { b | completedActivities : List a } }
    -> List { c | completion : { b | completedActivities : List a } }
eliminateEmptyEncounters =
    List.filter (.completion >> .completedActivities >> List.isEmpty >> not)


applyFilters :
    NominalDate
    -> NominalDate
    -> Maybe TakenBy
    -> List { a | startDate : Date, takenBy : Maybe TakenBy }
    -> List { a | startDate : Date, takenBy : Maybe TakenBy }
applyFilters startDate limitDate mTakenBy =
    List.filter
        (\encounter ->
            let
                takenByCondition =
                    Maybe.map
                        (\takenBy ->
                            encounter.takenBy == Just takenBy
                        )
                        mTakenBy
                        |> Maybe.withDefault True
            in
            (not <| Date.compare encounter.startDate startDate == LT)
                && (not <| Date.compare encounter.startDate limitDate == GT)
                && takenByCondition
        )


customApplyFilters :
    NominalDate
    -> NominalDate
    -> ({ a | encounterType : WellChildEncounterType, startDate : Date } -> TakenBy)
    -> Maybe TakenBy
    -> List { a | encounterType : WellChildEncounterType, startDate : Date }
    -> List { a | encounterType : WellChildEncounterType, startDate : Date }
customApplyFilters startDate limitDate resolveTakenByFunc mTakenBy =
    List.filter
        (\encounter ->
            let
                takenByCondition =
                    Maybe.map
                        (\takenBy ->
                            resolveTakenByFunc encounter == takenBy
                        )
                        mTakenBy
                        |> Maybe.withDefault True
            in
            (not <| Date.compare encounter.startDate startDate == LT)
                && (not <| Date.compare encounter.startDate limitDate == GT)
                && takenByCondition
        )


generateNutritionIndividualReportData :
    Language
    -> List (EncounterData NutritionChildActivity)
    -> MetricsResultsTableData
generateNutritionIndividualReportData language records =
    { heading = translate language Translate.NutritionIndividual
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.NutritionChildActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allNutritionIndividualActivities
    }


generateNutritionGroupReportData :
    Language
    -> List (NutritionGroupEncounterData NutritionMotherActivity NutritionChildActivity)
    -> MetricsResultsTableData
generateNutritionGroupReportData language records =
    let
        motherData =
            List.filterMap .motherData records

        childrenData =
            List.concatMap .childrenData records

        generateActivityRows activityTransId data =
            List.map
                (\activity ->
                    let
                        expected =
                            countOccurrences .expectedActivities activity data

                        completed =
                            countOccurrences .completedActivities activity data
                    in
                    [ translate language <| activityTransId activity
                    , String.fromInt expected
                    , String.fromInt completed
                    , calculatePercentage completed expected
                    ]
                )

        motherActivityRows =
            generateActivityRows Translate.NutritionMotherActivity motherData allNutritionMotherGroupActivities

        childrenActivityRows =
            generateActivityRows Translate.NutritionChildActivity childrenData allNutritionChildGroupActivities
    in
    { heading = translate language Translate.NutritionGroup
    , captions = generateCaptionsList language
    , rows = motherActivityRows ++ childrenActivityRows
    }


generateAcuteIllnessReportData :
    Language
    -> List (EncounterData AcuteIllnessActivity)
    -> MetricsResultsTableData
generateAcuteIllnessReportData language records =
    { heading = translate language Translate.AcuteIllness
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.AcuteIllnessActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allAcuteIllnessActivities
    }


generateWellChildReportData :
    Language
    -> TranslationId
    -> List WellChildActivity
    -> List WellChildEncounterData
    -> MetricsResultsTableData
generateWellChildReportData language labelTransId activities records =
    { heading = translate language labelTransId
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.WellChildActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            activities
    }


generateHomeVisitReportData :
    Language
    -> List (EncounterData HomeVisitActivity)
    -> MetricsResultsTableData
generateHomeVisitReportData language records =
    { heading = translate language Translate.HomeVisit
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.HomeVisitActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allHomeVisitActivities
    }


generateChildScoreboardReportData :
    Language
    -> List ChildScoreboardActivity
    -> List (EncounterData ChildScoreboardActivity)
    -> MetricsResultsTableData
generateChildScoreboardReportData language activities records =
    { heading = translate language Translate.ChildScorecard
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.ChildScoreboardActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            activities
    }


generateNCDReportData :
    Language
    -> List (EncounterData NCDActivity)
    -> MetricsResultsTableData
generateNCDReportData language records =
    { heading = translate language Translate.NCD
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.NCDActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allNCDActivities
    }


generateHIVReportData :
    Language
    -> List (EncounterData HIVActivity)
    -> MetricsResultsTableData
generateHIVReportData language records =
    { heading = translate language Translate.HIV
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.HIVActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allHIVActivities
    }


generateTuberculosisReportData :
    Language
    -> List (EncounterData TuberculosisActivity)
    -> MetricsResultsTableData
generateTuberculosisReportData language records =
    { heading = translate language Translate.Tuberculosis
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.TuberculosisActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allTuberculosisActivities
    }


generatePrenatalReportData :
    Language
    -> List (EncounterData PrenatalActivity)
    -> MetricsResultsTableData
generatePrenatalReportData language records =
    { heading = translate language Translate.Antenatal
    , captions = generateCaptionsList language
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        countOccurrences (.completion >> .expectedActivities) activity records

                    completed =
                        countOccurrences (.completion >> .completedActivities) activity records
                in
                [ translate language <| Translate.PrenatalActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calculatePercentage completed expected
                ]
            )
            allPrenatalActivities
    }



-- Helper functions.


generateCaptionsList : Language -> List String
generateCaptionsList language =
    [ translate language Translate.Activity
    , translate language Translate.Expected
    , translate language Translate.Completed
    , "%"
    ]


countOccurrences resolveFunc activity data =
    List.filter (resolveFunc >> List.member activity) data
        |> List.length
