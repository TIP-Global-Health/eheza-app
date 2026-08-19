module Pages.Completion.View exposing (view)

import App.Types exposing (Language, Site)
import Backend.Completion.Model
    exposing
        ( ActivitiesCompletionData
        , AcuteIllnessActivity
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
        , TakenBy(..)
        , TuberculosisActivity
        , WellChildEncounterData
        , WellChildEncounterType(..)
        )
import Backend.Completion.Utils exposing (takenByToString)
import Backend.Components.Model exposing (SelectedEntity(..))
import Backend.Model exposing (ModelBackend)
import Date exposing (Date)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Completion.Model exposing (Model, Msg(..), ReportType(..))
import Pages.Completion.Utils exposing (allAcuteIllnessActivities, allHIVActivities, allHomeVisitActivities, allNCDActivities, allNutritionChildGroupActivities, allNutritionIndividualActivities, allNutritionMotherGroupActivities, allPrenatalActivities, allTuberculosisActivities, newbornExamActivities, reportTypeToString, resolveChildScoreboardActivities, resolveSPVActivities)
import Pages.Components.Utils exposing (isSyncComplete, viewSyncingPlaceholder)
import Pages.Components.View exposing (viewMetricsResultsTable, viewReportDateInputs)
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (calculatePercentage, viewBackendData, viewCustomSelectListInput, viewSelectListInput, wrapSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    viewBackendData modelBackend.completionData
        (\data -> viewCompletionData language currentDate data model)


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

                        -- Other options are not supported.
                        _ ->
                            translate language Translate.EmptyString
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

        inputsAndContent =
            if isSyncComplete data.remainingForDownload then
                let
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
                                viewReportDateInputs language
                                    currentDate
                                    model.startDate
                                    model.limitDate
                                    SetStartDate
                                    SetStartDateSelectorState
                                    SetLimitDate
                                    SetLimitDateSelectorState
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
                div [ class "inputs" ] <|
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

            else
                let
                    downloadedCount =
                        List.length data.acuteIllnessData
                            + List.length data.childScoreboardData
                            + List.length data.hivData
                            + List.length data.homeVisitData
                            + List.length data.ncdData
                            + List.length data.nutritionIndividualData
                            + List.length data.nutritionGroupData
                            + List.length data.prenatalData
                            + List.length data.tuberculosisData
                            + List.length data.wellChildData
                in
                viewSyncingPlaceholder language downloadedCount data.remainingForDownload
    in
    div [ class "page-content completion" ]
        [ topBar
        , inputsAndContent
        , viewModal <| viewCalendarPopup language model.startDateSelectorPopupState model.startDate
        , viewModal <| viewCalendarPopup language model.limitDateSelectorPopupState model.limitDate
        ]


viewNutritionIndividualReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData NutritionChildActivity) -> Html Msg
viewNutritionIndividualReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.NutritionIndividual Translate.NutritionChildActivity allNutritionIndividualActivities
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
        |> generateReportData language Translate.AcuteIllness Translate.AcuteIllnessActivity allAcuteIllnessActivities
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
        |> generateReportData language Translate.StandardPediatricVisit Translate.WellChildActivity (resolveSPVActivities site)
        |> viewMetricsResultsTable
        |> div [ class "report well-child" ]


viewNewbornExamReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List WellChildEncounterData -> Html Msg
viewNewbornExamReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> customApplyFilters startDate
            limitDate
            (always TakenByCHW)
            mTakenBy
        |> generateReportData language Translate.NewbornExam Translate.WellChildActivity newbornExamActivities
        |> viewMetricsResultsTable
        |> div [ class "report well-child" ]


viewHomeVisitReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData HomeVisitActivity) -> Html Msg
viewHomeVisitReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.HomeVisit Translate.HomeVisitActivity allHomeVisitActivities
        |> viewMetricsResultsTable
        |> div [ class "report home-visit" ]


viewChildScoreboardReport : Language -> Site -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData ChildScoreboardActivity) -> Html Msg
viewChildScoreboardReport language site startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.ChildScorecard Translate.ChildScoreboardActivity (resolveChildScoreboardActivities site)
        |> viewMetricsResultsTable
        |> div [ class "report child-scoreboard" ]


viewNCDReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData NCDActivity) -> Html Msg
viewNCDReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.NCD Translate.NCDActivity allNCDActivities
        |> viewMetricsResultsTable
        |> div [ class "report ncd" ]


viewHIVReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData HIVActivity) -> Html Msg
viewHIVReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.HIV Translate.HIVActivity allHIVActivities
        |> viewMetricsResultsTable
        |> div [ class "report hiv" ]


viewTuberculosisReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData TuberculosisActivity) -> Html Msg
viewTuberculosisReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.Tuberculosis Translate.TuberculosisActivity allTuberculosisActivities
        |> viewMetricsResultsTable
        |> div [ class "report tuberculosis" ]


viewPrenatalReport : Language -> NominalDate -> NominalDate -> Maybe TakenBy -> List (EncounterData PrenatalActivity) -> Html Msg
viewPrenatalReport language startDate limitDate mTakenBy reportData =
    eliminateEmptyEncounters reportData
        |> applyFilters startDate limitDate mTakenBy
        |> generateReportData language Translate.Antenatal Translate.PrenatalActivity allPrenatalActivities
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
applyFilters startDate limitDate =
    applyFiltersBy startDate limitDate .takenBy


customApplyFilters :
    NominalDate
    -> NominalDate
    -> ({ a | encounterType : WellChildEncounterType, startDate : Date } -> TakenBy)
    -> Maybe TakenBy
    -> List { a | encounterType : WellChildEncounterType, startDate : Date }
    -> List { a | encounterType : WellChildEncounterType, startDate : Date }
customApplyFilters startDate limitDate resolveTakenByFunc =
    applyFiltersBy startDate limitDate (resolveTakenByFunc >> Just)


applyFiltersBy :
    NominalDate
    -> NominalDate
    -> ({ a | startDate : Date } -> Maybe TakenBy)
    -> Maybe TakenBy
    -> List { a | startDate : Date }
    -> List { a | startDate : Date }
applyFiltersBy startDate limitDate resolveTakenByFunc mTakenBy =
    List.filter
        (\encounter ->
            let
                takenByCondition =
                    Maybe.map
                        (\takenBy ->
                            resolveTakenByFunc encounter == Just takenBy
                        )
                        mTakenBy
                        |> Maybe.withDefault True
            in
            (not <| Date.compare encounter.startDate startDate == LT)
                && (not <| Date.compare encounter.startDate limitDate == GT)
                && takenByCondition
        )


generateReportData :
    Language
    -> TranslationId
    -> (activity -> TranslationId)
    -> List activity
    -> List { record | completion : ActivitiesCompletionData activity }
    -> MetricsResultsTableData
generateReportData language headingTransId activityTransId activities records =
    { heading = translate language headingTransId
    , captions = generateCaptionsList language
    , rows = generateActivityRows language activityTransId (List.map .completion records) activities
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
    in
    { heading = translate language Translate.NutritionGroup
    , captions = generateCaptionsList language
    , rows =
        generateActivityRows language Translate.NutritionMotherActivity motherData allNutritionMotherGroupActivities
            ++ generateActivityRows language Translate.NutritionChildActivity childrenData allNutritionChildGroupActivities
    }


generateActivityRows :
    Language
    -> (activity -> TranslationId)
    -> List (ActivitiesCompletionData activity)
    -> List activity
    -> List (List String)
generateActivityRows language activityTransId data =
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
