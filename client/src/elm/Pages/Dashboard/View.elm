module Pages.Dashboard.View exposing (chwCard, view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model
    exposing
        ( AcuteIllnessDataItem
        , AcuteIllnessEncounterDataItem
        , AssembledData
        , CaseManagement
        , CaseNutritionTotal
        , ChildScoreboardDataItem
        , DashboardStats
        , EducationSessionData
        , NCDDataItem
        , Nutrition
        , NutritionDataItem
        , NutritionEncounterDataItem
        , NutritionPageData
        , NutritionValue
        , PMTCTDataItem
        , ParticipantStats
        , PatientDetails
        , Periods
        , PersonIdentifier
        , PrenatalDataItem
        , SPVDataItem
        , TotalBeneficiaries
        , emptyNutritionValue
        )
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..))
import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.Utils exposing (groupEducationEnabled)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), WellChildEncounterType(..))
import Color exposing (Color)
import Date exposing (Month, Unit(..), numberToMonth)
import Debug exposing (toString)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, isDiffTruthy, toLastDayOfMonth)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Utils exposing (generateFutureVaccinationsData)
import Pages.Dashboard.GraphUtils exposing (barChartHeight, barChartWidth, column, familyPlanningSignToColor, familyPlanningSignsColors, feverCauseToColor, feverCausesColors, gridXScale, gridYScale, padding, pieChartHeight, pieChartWidth, radius, xAxis, xGridLine, xScale, yAxis, yGridLine)
import Pages.Dashboard.Model exposing (BeneficiariesTableLabels(..), CardValueSeverity(..), DashboardFilter(..), DashboardSubFilter(..), FamilyPlanningSignsCounter, FeverCause(..), FilterGender(..), FilterPeriod(..), FilterProgramType(..), FilterType(..), MalnorishedNutritionData, ModalState(..), Model, MonthlyChartType(..), Msg(..), StatsCard, caseManagementFilters, caseManagementSubFilters, filterGenders, filterPeriodsForStatsPage, maxMonthGap, monthlyChartFilters)
import Pages.Dashboard.Utils exposing (applyGenderFilter, countAcuteIllnessAssessments, countAcuteIllnessCasesByPossibleDiagnosises, countAcuteIllnessCasesByTreatmentApproach, countAcuteIllnessDiagnosedCases, countComplicatedGISentToHC, countComplicatedMalariaSentToHC, countCurrentlyPregnantForSelectedMonth, countCurrentlyPregnantWithDangerSignsForSelectedMonth, countDeliveriesAtLocationForSelectedMonth, countDiagnosedWithCovidCallsTo114, countDiagnosedWithCovidManagedAtHome, countDiagnosedWithCovidSentToHC, countDiagnosedWithGI, countDiagnosedWithMalaria, countHospitalReferralsForSelectedMonth, countNewbornForSelectedMonth, countNewlyIdentifieHypertensionCasesForSelectedMonth, countNewlyIdentifiedDiabetesCasesForSelectedMonth, countNewlyIdentifiedPregananciesForSelectedMonth, countPregnanciesDueWithin4MonthsForSelectedMonth, countPregnanciesWith4VisitsOrMoreForSelectedMonth, countResolvedGICasesForSelectedMonth, countResolvedMalariaCasesForSelectedMonth, countTotalNumberOfPatientsWithDiabetes, countTotalNumberOfPatientsWithGestationalDiabetes, countTotalNumberOfPatientsWithHypertension, countUncomplicatedGIManagedByChw, countUncomplicatedMalariaAndPregnantSentToHC, countUncomplicatedMalariaManagedByChw, countUncomplicatedMalariaSentToHC, filterNewlyDiagnosesCasesForSelectedMonth, filterNewlyDiagnosesMalnutritionForSelectedMonth, filterProgramTypeToString, filterStatsByGender, filterStatsWithinPeriod, generatePatientsWithHIV, generateVaccinationProgressDict, getAcuteIllnessFollowUpsBreakdownByDiagnosis, getEncountersForSelectedMonth, getFollowUpsTotals, isAcuteIllnessNurseEncounter, isNurseEncounter, withinOrAfterSelectedMonth, withinOrBeforeSelectedMonth, withinSelectedMonth)
import Pages.Page
    exposing
        ( AcuteIllnessSubPage(..)
        , ChildWellnessSubPage(..)
        , DashboardPage(..)
        , NCDSubPage(..)
        , NutritionSubPage(..)
        , Page(..)
        , UserPage(..)
        )
import Pages.Prenatal.Utils exposing (preeclampsiaDiagnoses, severeAnemiaDiagnoses, syphilisDiagnoses)
import Pages.Utils
    exposing
        ( calculatePercentage
        , resolveSelectedDateForMonthSelector
        , viewCustomAction
        , viewCustomSelectListInput
        , viewMonthSelector
        )
import Path
import RemoteData
import Restful.Endpoint exposing (fromEntityUuid)
import Scale
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, r)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes as Explicit exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))
import Utils.Html exposing (spinner, viewModal)
import Utils.NominalDate exposing (sortByDateDesc)


view :
    Language
    -> DashboardPage
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> HealthCenterId
    -> Bool
    -> Nurse
    -> Model
    -> ModelIndexedDb
    -> Html Msg
view language page currentDate site features healthCenterId isChw nurse model db =
    let
        header =
            case page of
                PageMain ->
                    let
                        label =
                            if isChw then
                                Translate.ChwDashboardLabel

                            else
                                Translate.DashboardLabel
                    in
                    viewHeader language label (SetActivePage PinCodePage)

                PageAcuteIllness _ ->
                    viewHeader language Translate.AcuteIllness (SetActivePage <| UserPage <| DashboardPage PageMain)

                PageNutrition subPage ->
                    let
                        label =
                            if isChw then
                                Translate.HomeVisit

                            else
                                Translate.ChildNutrition

                        goBackAction =
                            case subPage of
                                PageCharts ->
                                    SetActivePage <| UserPage <| DashboardPage PageMain

                                PageStats ->
                                    SetActivePage <| UserPage <| DashboardPage <| PageNutrition PageCharts

                                PageCaseManagement ->
                                    SetActivePage <| UserPage <| DashboardPage model.latestPage
                    in
                    viewHeader language label goBackAction

                PagePrenatal ->
                    viewHeader language Translate.AntenatalCare (SetActivePage <| UserPage <| DashboardPage PageMain)

                PageNCD _ ->
                    viewHeader language Translate.NCDs (SetActivePage <| UserPage <| DashboardPage PageMain)

                PageChildWellness _ ->
                    viewHeader language Translate.Pediatrics (SetActivePage <| UserPage <| DashboardPage PageMain)

                PageGroupEducation ->
                    let
                        goBackAction =
                            if isJust model.educationSessionDrillIn then
                                SetEducationSessionDrillIn Nothing

                            else
                                SetActivePage <| UserPage <| DashboardPage PageMain
                    in
                    viewHeader language Translate.GroupEducation goBackAction

        content =
            Dict.get healthCenterId db.computedDashboards
                |> Maybe.andThen (.assembledPermutations >> Dict.get ( model.programTypeFilter, model.selectedVillageFilter ))
                |> Maybe.map
                    (\assembled ->
                        let
                            ( pageContent, pageClass ) =
                                case page of
                                    PageMain ->
                                        ( viewPageMain language currentDate features healthCenterId isChw assembled db model
                                        , "main"
                                        )

                                    PageAcuteIllness subPage ->
                                        ( viewAcuteIllnessPage language currentDate healthCenterId isChw subPage assembled db model
                                        , "acute-illness"
                                        )

                                    PageNutrition subPage ->
                                        ( viewNutritionPage language
                                            currentDate
                                            subPage
                                            assembled.stats
                                            assembled.patientsDetails
                                            assembled.nutritionPageData
                                            model
                                        , "nutrition"
                                        )

                                    PagePrenatal ->
                                        ( viewPrenatalPage language currentDate isChw assembled model, "prenatal" )

                                    PageNCD subPage ->
                                        ( viewNCDPage language currentDate subPage assembled model, "ncd" )

                                    PageChildWellness subPage ->
                                        ( viewChildWellnessPage language currentDate site subPage assembled model, "child-wellness" )

                                    PageGroupEducation ->
                                        ( viewGroupEducationPage language currentDate assembled model, "group-education" )
                        in
                        div [ class <| "dashboard " ++ pageClass ] <|
                            (viewFiltersPane language page db model
                                |> showIf (isNothing model.educationSessionDrillIn)
                            )
                                :: pageContent
                                ++ [ viewCustomModal language page isChw nurse assembled db model
                                   , div [ class "timestamp" ]
                                        [ text <| (translate language <| Translate.Dashboard Translate.LastUpdated) ++ ": " ++ assembled.stats.timestamp ++ " UTC" ]
                                   ]
                    )
                |> Maybe.withDefault spinner
    in
    div
        [ class "wrap" ]
        [ header
        , content
        ]


viewHeader : Language -> TranslationId -> Msg -> Html Msg
viewHeader language label goBackAction =
    div [ class "ui basic head segment" ]
        [ h1 [ class "ui header" ]
            [ translateText language label ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewPageMain : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> Bool -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewPageMain language currentDate features healthCenterId isChw assembled db model =
    if isChw then
        viewPageMainForChw language currentDate features healthCenterId assembled db model

    else
        viewPageMainForNurse language features


viewPageMainForNurse : Language -> EverySet SiteFeature -> List (Html Msg)
viewPageMainForNurse language features =
    [ viewMenuForNurse language features ]


viewPageMainForChw : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewPageMainForChw language currentDate features healthCenterId assembled db model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        -- ANC
        encountersForSelectedMonth =
            getEncountersForSelectedMonth dateLastDayOfSelectedMonth assembled.acuteIllnessData

        diagnosedCases =
            countAcuteIllnessDiagnosedCases encountersForSelectedMonth

        -- Prenatal
        currentlyPregnant =
            countCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth True assembled.prenatalData

        totalNewborn =
            countNewbornForSelectedMonth dateLastDayOfSelectedMonth assembled.prenatalData

        limitDate =
            Date.ceiling Date.Month dateLastDayOfSelectedMonth

        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        -- Case Management
        ( totalNutritionFollowUps, totalAcuteIllnessFollowUps, totalPrenatalFollowUps ) =
            Maybe.map2 (getFollowUpsTotals currentDate limitDate db)
                model.selectedVillageFilter
                followUps
                |> Maybe.withDefault ( 0, 0, 0 )
    in
    [ viewMenuForChw language features
    , monthSelector language dateLastDayOfSelectedMonth model
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.AcuteIllnessDiagnosed) (String.fromInt diagnosedCases)
            , chwCard language (Translate.Dashboard Translate.MothersInANC) (String.fromInt currentlyPregnant)
            , chwCard language (Translate.Dashboard Translate.NewbornsInCare) (String.fromInt totalNewborn)
            ]
        ]
    , div [ class "case-management-label" ] [ text <| translate language <| Translate.CaseManagement ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.AcuteIllness (String.fromInt totalAcuteIllnessFollowUps)
            , chwCard language Translate.HomeVisit (String.fromInt totalNutritionFollowUps)
            , chwCard language Translate.AntenatalCare (String.fromInt totalPrenatalFollowUps)
            ]
        ]
    ]


viewNutritionPage :
    Language
    -> NominalDate
    -> NutritionSubPage
    -> DashboardStats
    -> Dict PersonIdentifier PatientDetails
    -> NutritionPageData
    -> Model
    -> List (Html Msg)
viewNutritionPage language currentDate activePage stats patientsDetails data model =
    case activePage of
        PageCharts ->
            viewNutritionChartsPage language currentDate data model

        PageStats ->
            viewStatsPage language currentDate stats model

        PageCaseManagement ->
            viewCaseManagementPage language currentDate stats patientsDetails model


viewStatsPage : Language -> NominalDate -> DashboardStats -> Model -> List (Html Msg)
viewStatsPage language currentDate stats model =
    if model.programTypeFilter /= FilterProgramFbf then
        []

    else
        let
            currentMonth =
                Date.month currentDate
                    |> Date.monthToNumber

            ( modelWithLastMonth, displayedMonth ) =
                if model.period == ThisMonth then
                    ( { model | period = LastMonth }, currentMonth )

                else
                    ( { model | period = ThreeMonthsAgo }, resolvePreviousMonth currentMonth )

            currentPeriodStats =
                filterStatsWithinPeriod currentDate model.period stats

            monthBeforeStats =
                filterStatsWithinPeriod currentDate modelWithLastMonth.period stats

            malnourishedCurrentMonth =
                mapMalnorishedByMonth displayedMonth currentPeriodStats.caseManagement.thisYear

            malnourishedPreviousMonth =
                mapMalnorishedByMonth (resolvePreviousMonth displayedMonth) currentPeriodStats.caseManagement.thisYear
        in
        [ div [ class "ui equal width grid" ]
            [ viewMalnourishedCards language malnourishedCurrentMonth malnourishedPreviousMonth
            , viewMiscCards language currentDate currentPeriodStats monthBeforeStats
            ]
        , viewBeneficiariesTable language currentDate stats currentPeriodStats malnourishedCurrentMonth model
        , viewFamilyPlanning language currentPeriodStats
        ]


mapMalnorishedByMonth : Int -> List CaseManagement -> List MalnorishedNutritionData
mapMalnorishedByMonth mappedMonth caseManagement =
    caseManagement
        |> List.foldl
            (\caseNutrition accum ->
                let
                    nutrition =
                        caseNutrition.nutrition.stunting
                            |> Dict.toList
                            |> List.filterMap
                                (\( month, stuntingValue ) ->
                                    if month /= mappedMonth then
                                        Nothing

                                    else
                                        let
                                            resolveValueClass func =
                                                Dict.get month (func caseNutrition.nutrition)
                                                    |> Maybe.withDefault emptyNutritionValue
                                                    |> .class

                                            values =
                                                [ stuntingValue.class
                                                , resolveValueClass .underweight
                                                , resolveValueClass .wasting
                                                , resolveValueClass .muac
                                                ]
                                        in
                                        if List.member Backend.Dashboard.Model.Severe values then
                                            MalnorishedNutritionData caseNutrition.identifier
                                                caseNutrition.birthDate
                                                caseNutrition.gender
                                                Backend.Dashboard.Model.Severe
                                                |> Just

                                        else if List.member Backend.Dashboard.Model.Moderate values then
                                            MalnorishedNutritionData caseNutrition.identifier
                                                caseNutrition.birthDate
                                                caseNutrition.gender
                                                Backend.Dashboard.Model.Moderate
                                                |> Just

                                        else
                                            Nothing
                                )
                in
                nutrition ++ accum
            )
            []


viewCaseManagementPage :
    Language
    -> NominalDate
    -> DashboardStats
    -> Dict PersonIdentifier PatientDetails
    -> Model
    -> List (Html Msg)
viewCaseManagementPage language currentDate stats patientsDetails model =
    if model.programTypeFilter /= FilterProgramFbf then
        []

    else
        let
            currentMonth =
                Date.month currentDate
                    |> Date.monthToNumber

            tableDataUnsorted =
                case model.currentCaseManagementFilter of
                    -- We consider session as missed, when all 4 values for the
                    -- month are Neutral.
                    MissedSession ->
                        List.foldl
                            (\caseNutrition accum ->
                                let
                                    nutrition =
                                        caseNutrition.nutrition.stunting
                                            |> Dict.toList
                                            |> List.filterMap
                                                (\( month, stuntingValue ) ->
                                                    if withinThreePreviousMonths currentMonth month then
                                                        let
                                                            resolveValueClass func =
                                                                Dict.get month (func caseNutrition.nutrition)
                                                                    |> Maybe.withDefault emptyNutritionValue
                                                                    |> .class
                                                        in
                                                        if
                                                            List.all ((==) Backend.Dashboard.Model.Neutral)
                                                                [ stuntingValue.class
                                                                , resolveValueClass .underweight
                                                                , resolveValueClass .wasting
                                                                , resolveValueClass .muac
                                                                ]
                                                        then
                                                            Just ( month, emptyNutritionValue )

                                                        else
                                                            Just ( month, NutritionValue Backend.Dashboard.Model.Good "V" )

                                                    else
                                                        Nothing
                                                )
                                            |> List.sortBy Tuple.first
                                            |> List.reverse

                                    name =
                                        Dict.get caseNutrition.identifier patientsDetails
                                            |> Maybe.map .name
                                            |> Maybe.withDefault ""
                                in
                                { name = name, nutrition = nutrition } :: accum
                            )
                            []
                            stats.caseManagement.thisYear
                            |> List.filter (.nutrition >> List.all (Tuple.second >> .class >> (==) Backend.Dashboard.Model.Good) >> not)

                    _ ->
                        List.foldl
                            (\caseNutrition accum ->
                                let
                                    name =
                                        Dict.get caseNutrition.identifier patientsDetails
                                            |> Maybe.map .name
                                            |> Maybe.withDefault ""
                                in
                                case model.currentCaseManagementFilter of
                                    Stunting ->
                                        { name = name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.stunting } :: accum

                                    Underweight ->
                                        { name = name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.underweight } :: accum

                                    Wasting ->
                                        { name = name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.wasting } :: accum

                                    MUAC ->
                                        { name = name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.muac } :: accum

                                    -- We'll never get here - need to list it to satisfy compiler.
                                    MissedSession ->
                                        accum
                            )
                            []
                            stats.caseManagement.thisYear
                            |> List.filter
                                (.nutrition
                                    >> List.any
                                        (Tuple.second
                                            >> .class
                                            >> (\class ->
                                                    case model.currentCaseManagementSubFilter of
                                                        FilterTotal ->
                                                            (class == Backend.Dashboard.Model.Moderate) || (class == Backend.Dashboard.Model.Severe)

                                                        FilterModerate ->
                                                            class == Backend.Dashboard.Model.Moderate

                                                        FilterSevere ->
                                                            class == Backend.Dashboard.Model.Severe
                                               )
                                        )
                                )

            tableData =
                tableDataUnsorted
                    -- Sort table by person's (lowercase) name.
                    |> List.sortWith (\p1 p2 -> compare (String.toLower p1.name) (String.toLower p2.name))

            filterForCaseManagementTableFunc nutritionDict =
                nutritionDict
                    |> Dict.toList
                    |> List.filter (Tuple.first >> withinThreePreviousMonths currentMonth)
                    |> List.sortBy Tuple.first
                    |> List.reverse
        in
        [ div [ class "ui segment blue" ]
            [ div [ class "case-management" ]
                [ div [ class "header" ]
                    [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.CaseManagementLabel ]
                    , List.map (viewFilter language FilterCaseManagement model.currentCaseManagementFilter) caseManagementFilters
                        |> div [ class "filters" ]
                    , List.map (viewSubFilter language model.currentCaseManagementSubFilter) (caseManagementSubFilters model.currentCaseManagementFilter)
                        |> div [ class "filters secondary" ]
                    ]
                , div [ class "content" ]
                    [ viewCaseManagementTable language tableData ]
                ]
            ]
        ]


viewCaseManagementTable : Language -> List { name : String, nutrition : List ( Int, NutritionValue ) } -> Html Msg
viewCaseManagementTable language tableData =
    let
        monthLabels =
            tableData
                |> List.head
                |> Maybe.map
                    (.nutrition
                        >> List.map
                            (Tuple.first
                                >> Date.numberToMonth
                                >> Translate.ResolveMonth True
                            )
                    )
                |> Maybe.withDefault []
    in
    table [ class "ui very basic collapsing celled table" ]
        [ thead []
            [ tr []
                (th [ class "name" ] [ translateText language <| Translate.Name ]
                    :: List.map (\month -> th [] [ span [] [ translateText language month ] ]) monthLabels
                )
            ]
        , tbody []
            (List.map viewCaseManagementTableRow tableData)
        ]


viewCaseManagementTableRow : { name : String, nutrition : List ( Int, NutritionValue ) } -> Html Msg
viewCaseManagementTableRow rowData =
    tr []
        (td [ class "name" ] [ text rowData.name ]
            :: List.map viewMonthCell rowData.nutrition
        )


viewMonthCell : ( Int, NutritionValue ) -> Html Msg
viewMonthCell ( month, cellData ) =
    let
        class =
            classList
                [ ( String.toLower <| Debug.toString cellData.class, True )
                , ( String.fromInt month, True )
                ]
    in
    td [ class ] [ span [] [ text cellData.value ] ]


viewFiltersPane : Language -> DashboardPage -> ModelIndexedDb -> Model -> Html Msg
viewFiltersPane language page db model =
    let
        filters =
            case page of
                PageNutrition subPage ->
                    case subPage of
                        PageCharts ->
                            [ labelSelected, programTypeFilterButton ]

                        PageStats ->
                            let
                                periodButton period =
                                    button
                                        [ classList
                                            [ ( "inactive", model.period /= period )
                                            , ( "primary ui button", True )
                                            ]
                                        , onClick <| SetFilterPeriod period
                                        ]
                                        [ translateText language <| Translate.Dashboard <| Translate.PeriodFilter period
                                        ]
                            in
                            List.map periodButton filterPeriodsForStatsPage

                        PageCaseManagement ->
                            []

                _ ->
                    [ labelSelected, programTypeFilterButton ]

        programTypeFilterButton =
            div
                [ class "primary ui button program-type-filter"
                , onClick <| SetModalState <| Just FiltersModal
                ]
                [ span [] [ translateText language <| Translate.Dashboard Translate.Filters ]
                , span [ class "icon-settings" ] []
                ]

        labelSelected =
            if model.programTypeFilter == FilterProgramCommunity then
                db.villages
                    |> RemoteData.toMaybe
                    |> Maybe.andThen
                        (\villages ->
                            model.selectedVillageFilter
                                |> Maybe.andThen
                                    (\villageId ->
                                        Dict.get villageId villages
                                            |> Maybe.map
                                                (\village ->
                                                    span [ class "label" ]
                                                        [ text <| translate language Translate.SelectedVillage ++ ": "
                                                        , text village.name
                                                        ]
                                                )
                                    )
                        )
                    |> Maybe.withDefault emptyNode

            else
                span [ class "label" ]
                    [ text <| translate language Translate.SelectedProgram ++ ": "
                    , translateText language <| Translate.Dashboard <| Translate.FilterProgramType model.programTypeFilter
                    ]
    in
    div [ class "ui segment filters" ]
        filters


viewMenuForNurse : Language -> EverySet SiteFeature -> Html Msg
viewMenuForNurse language features =
    let
        groupEducationButton =
            if groupEducationEnabled features then
                viewMenuButton language PageGroupEducation Nothing

            else
                emptyNode
    in
    div []
        [ div [ class "ui segment page-filters top-row" ]
            [ viewMenuButton language PagePrenatal Nothing
            , viewMenuButton language (PageNutrition PageCharts) Nothing
            , viewMenuButton language (PageAcuteIllness PageAcuteIllnessOverview) Nothing
            ]
        , div [ class "ui segment page-filters center" ]
            [ viewMenuButton language (PageNCD PageHypertension) Nothing
            , viewMenuButton language (PageChildWellness PageChildWellnessOverview) Nothing
            , groupEducationButton
            ]
        ]


viewMenuForChw : Language -> EverySet SiteFeature -> Html Msg
viewMenuForChw language features =
    let
        secondRow =
            if groupEducationEnabled features then
                div [ class "ui segment page-filters center" ]
                    [ viewMenuButton language PageGroupEducation Nothing ]

            else
                emptyNode
    in
    div []
        [ div [ class "ui segment page-filters top-row" ]
            [ viewMenuButton language PagePrenatal Nothing
            , viewMenuButton language (PageNutrition PageCharts) Nothing
            , viewMenuButton language (PageAcuteIllness PageAcuteIllnessOverview) Nothing
            ]
        , secondRow
        ]


viewAcuteIllnessMenu : Language -> AcuteIllnessSubPage -> Html Msg
viewAcuteIllnessMenu language activePage =
    div [ class "ui segment page-filters" ]
        [ viewMenuButton language (PageAcuteIllness PageAcuteIllnessOverview) (Just <| PageAcuteIllness activePage)
        , viewMenuButton language (PageAcuteIllness PageCovid19) (Just <| PageAcuteIllness activePage)
        , viewMenuButton language (PageAcuteIllness PageMalaria) (Just <| PageAcuteIllness activePage)
        , viewMenuButton language (PageAcuteIllness PageGastro) (Just <| PageAcuteIllness activePage)
        ]


viewNCDMenu : Language -> NCDSubPage -> Html Msg
viewNCDMenu language activePage =
    div [ class "ui segment page-filters" ]
        [ viewMenuButton language (PageNCD PageHypertension) (Just <| PageNCD activePage)
        , viewMenuButton language (PageNCD PageHIV) (Just <| PageNCD activePage)
        , viewMenuButton language (PageNCD PageDiabetes) (Just <| PageNCD activePage)
        ]


viewChildWellnessMenu : Language -> ChildWellnessSubPage -> Html Msg
viewChildWellnessMenu language activePage =
    div [ class "ui segment page-filters center" ]
        [ viewMenuButton language (PageChildWellness PageChildWellnessOverview) (Just <| PageChildWellness activePage)
        , viewMenuButton language (PageChildWellness PageChildWellnessNutrition) (Just <| PageChildWellness activePage)
        ]


viewMenuButton : Language -> DashboardPage -> Maybe DashboardPage -> Html Msg
viewMenuButton language targetPage activePage =
    let
        label =
            case ( activePage, targetPage ) of
                ( Nothing, PageAcuteIllness PageAcuteIllnessOverview ) ->
                    -- On Main page, and target is Acute Illness page.
                    Translate.AcuteIllness

                ( Nothing, PageNCD PageHypertension ) ->
                    -- On Main page, and target is Acute Illness page.
                    Translate.NCDs

                ( Nothing, PageChildWellness PageChildWellnessOverview ) ->
                    -- On Main page, and target is Acute Illness page.
                    Translate.ChildWellness

                _ ->
                    Translate.EncounterTypePageLabel targetPage
    in
    button
        [ classList
            [ ( "active", activePage == Just targetPage )
            , ( "primary ui button", True )
            ]
        , DashboardPage targetPage
            |> UserPage
            |> SetActivePage
            |> onClick
        ]
        [ translateText language label ]


viewAcuteIllnessPage :
    Language
    -> NominalDate
    -> HealthCenterId
    -> Bool
    -> AcuteIllnessSubPage
    -> AssembledData
    -> ModelIndexedDb
    -> Model
    -> List (Html Msg)
viewAcuteIllnessPage language currentDate healthCenterId isChw activePage assembled db model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        encountersForSelectedMonth =
            if isChw then
                List.filterMap
                    (\illness ->
                        let
                            chwEncounters =
                                List.filter (isAcuteIllnessNurseEncounter >> not) illness.encounters
                        in
                        if List.isEmpty chwEncounters then
                            Nothing

                        else
                            Just { illness | encounters = chwEncounters }
                    )
                    assembled.acuteIllnessData
                    |> getEncountersForSelectedMonth dateLastDayOfSelectedMonth

            else
                List.filterMap
                    (\illness ->
                        let
                            nurseEncounters =
                                List.filter isAcuteIllnessNurseEncounter illness.encounters
                        in
                        if List.isEmpty nurseEncounters then
                            Nothing

                        else
                            Just { illness | encounters = nurseEncounters }
                    )
                    assembled.acuteIllnessData
                    |> getEncountersForSelectedMonth dateLastDayOfSelectedMonth

        limitDate =
            Date.ceiling Date.Month dateLastDayOfSelectedMonth

        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        ( managedCovid, managedMalaria, managedGI ) =
            Maybe.map2 (getAcuteIllnessFollowUpsBreakdownByDiagnosis currentDate limitDate db)
                model.selectedVillageFilter
                followUps
                |> Maybe.withDefault ( 0, 0, 0 )

        pageContent =
            case activePage of
                PageAcuteIllnessOverview ->
                    viewAcuteIllnessOverviewPage language isChw encountersForSelectedMonth

                PageCovid19 ->
                    viewCovid19Page language isChw encountersForSelectedMonth managedCovid

                PageMalaria ->
                    viewMalariaPage language isChw dateLastDayOfSelectedMonth assembled.acuteIllnessData encountersForSelectedMonth managedMalaria

                PageGastro ->
                    viewGastroPage language isChw dateLastDayOfSelectedMonth assembled.acuteIllnessData encountersForSelectedMonth managedGI
    in
    [ viewAcuteIllnessMenu language activePage
    , monthSelector language dateLastDayOfSelectedMonth model
    ]
        ++ pageContent


viewAcuteIllnessOverviewPage : Language -> Bool -> List AcuteIllnessEncounterDataItem -> List (Html Msg)
viewAcuteIllnessOverviewPage language isChw encounters =
    let
        totalAssesments =
            countAcuteIllnessAssessments encounters

        ( sentToHC, managedLocally ) =
            countAcuteIllnessCasesByTreatmentApproach encounters

        secondRow =
            if isChw then
                let
                    undeterminedCases =
                        countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisUndeterminedMoreEvaluationNeeded ] False encounters
                in
                div [ class "ui centered grid" ]
                    [ div [ class "three column row" ]
                        [ chwCard language (Translate.Dashboard Translate.DiagnosisUndetermined) (String.fromInt undeterminedCases)
                        , chwCard language (Translate.Dashboard Translate.FeverOfUnknownOrigin) (String.fromInt feverOfUnknownOriginCases)
                        ]
                    ]

            else
                emptyNode

        feverOfUnknownOriginCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisFeverOfUnknownOrigin ] False encounters

        covidCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisCovid19Suspect ] True encounters

        malariaCases =
            countAcuteIllnessCasesByPossibleDiagnosises
                [ DiagnosisMalariaComplicated
                , DiagnosisMalariaUncomplicated
                , DiagnosisMalariaUncomplicatedAndPregnant
                ]
                True
                encounters

        respiratoryCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisRespiratoryInfectionComplicated ] True encounters

        giCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisGastrointestinalInfectionComplicated ] True encounters

        chartData =
            List.filter (Tuple.second >> (/=) 0)
                [ ( FeverCauseCovid19, covidCases )
                , ( FeverCauseMalaria, malariaCases )
                , ( FeverCauseRespiratory, respiratoryCases )
                , ( FeverCauseGI, giCases )
                , ( FeverCauseUnknown, feverOfUnknownOriginCases )
                ]

        ( levelCases, referrals ) =
            if isChw then
                ( Translate.CommunityLevelCases, Translate.HealthCenterReferrals )

            else
                ( Translate.HealthCenterLevelCases, Translate.HospitalReferrals )
    in
    [ div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.TotalAssessment) (String.fromInt totalAssesments)
            , chwCard language (Translate.Dashboard levelCases) (String.fromInt managedLocally)
            , chwCard language (Translate.Dashboard referrals) (String.fromInt sentToHC)
            ]
        ]
    , secondRow
    , div [ class "ui blue segment donut-chart left" ]
        [ viewDonutChart language
            (Translate.Dashboard Translate.FeversByCause)
            (Translate.FeverCause >> Translate.Dashboard)
            feverCauseToColor
            feverCausesColors
            chartData
        ]
    ]


viewDonutChart : Language -> TranslationId -> (a -> TranslationId) -> (a -> Color) -> Dict a Color -> List ( a, Int ) -> Html Msg
viewDonutChart language label translationFunc toColorFunc colors data =
    if List.isEmpty data then
        div [ class "no-data-message" ] [ translateText language <| Translate.Dashboard Translate.NoDataForPeriod ]

    else
        div [ class "ui center aligned grid" ]
            [ div [ class "middle aligned row" ]
                [ div [ class "content" ]
                    [ viewPieChart colors data
                    , div [ class "in-chart" ]
                        [ translateText language label ]
                    , viewPieChartLegend language translationFunc toColorFunc data
                    ]
                ]
            ]


viewCovid19Page : Language -> Bool -> List AcuteIllnessEncounterDataItem -> Int -> List (Html Msg)
viewCovid19Page language isChw encounters managedCovid =
    let
        managedAtHome =
            countDiagnosedWithCovidManagedAtHome encounters
    in
    if isChw then
        let
            callsTo114 =
                countDiagnosedWithCovidCallsTo114 encounters

            sentToHC =
                countDiagnosedWithCovidSentToHC encounters
        in
        [ div [ class "ui grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.CallsTo114) (String.fromInt callsTo114)
                , chwCard language (Translate.Dashboard Translate.HealthCenterReferrals) (String.fromInt sentToHC)
                , chwCard language (Translate.Dashboard Translate.PatientsManagedAtHome) (String.fromInt managedAtHome)
                ]
            ]
        , div [ class "ui centered grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.PatientCurrentlyUnderCare) (String.fromInt managedCovid) ]
            ]
        ]

    else
        [ div [ class "ui grid" ]
            [ div [ class "two column row center" ]
                [ chwCard language (Translate.Dashboard Translate.PatientsManagedAtHome) (String.fromInt managedAtHome)
                ]
            ]
        ]


viewMalariaPage : Language -> Bool -> NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem -> Int -> List (Html Msg)
viewMalariaPage language isChw dateLastDayOfSelectedMonth acuteIllnessData encountersForSelectedMonth managedMalaria =
    let
        totalDaignosed =
            countDiagnosedWithMalaria encountersForSelectedMonth

        uncomplicatedMalariaAndPregnantSentToHC =
            countUncomplicatedMalariaAndPregnantSentToHC encountersForSelectedMonth

        complicatedMalariaSentToHC =
            countComplicatedMalariaSentToHC encountersForSelectedMonth
    in
    if isChw then
        let
            uncomplicatedMalariaManagedByChw =
                countUncomplicatedMalariaManagedByChw encountersForSelectedMonth

            resolvedMalariaCases =
                countResolvedMalariaCasesForSelectedMonth dateLastDayOfSelectedMonth acuteIllnessData
        in
        [ div [ class "ui grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.DiagnosedCases) (String.fromInt totalDaignosed)
                , chwCard language (Translate.Dashboard Translate.UncomplicatedMalariaByChws) (String.fromInt uncomplicatedMalariaManagedByChw)
                , chwCard language (Translate.Dashboard Translate.UncomplicatedMalariaInPregnancyReferredToHc) (String.fromInt uncomplicatedMalariaAndPregnantSentToHC)
                ]
            ]
        , div [ class "ui centered grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.ComplicatedMalariaReferredToHC) (String.fromInt complicatedMalariaSentToHC)
                , chwCard language (Translate.Dashboard Translate.ResolvedCases) (String.fromInt resolvedMalariaCases ++ " : " ++ String.fromInt managedMalaria)
                ]
            ]
        ]

    else
        let
            sentToHospital =
                countUncomplicatedMalariaSentToHC encountersForSelectedMonth
                    + uncomplicatedMalariaAndPregnantSentToHC
                    + complicatedMalariaSentToHC
        in
        [ div [ class "ui grid" ]
            [ div [ class "two column row" ]
                [ chwCard language (Translate.Dashboard Translate.DiagnosedCases) (String.fromInt totalDaignosed)
                , chwCard language (Translate.Dashboard Translate.HospitalReferrals) (String.fromInt sentToHospital)
                ]
            ]
        ]


viewGastroPage : Language -> Bool -> NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem -> Int -> List (Html Msg)
viewGastroPage language isChw dateLastDayOfSelectedMonth acuteIllnessData encountersForSelectedMonth managedGI =
    let
        totalDaignosed =
            countDiagnosedWithGI encountersForSelectedMonth

        complicatedGISentToHC =
            countComplicatedGISentToHC encountersForSelectedMonth
    in
    if isChw then
        let
            uncomplicatedGIManagedByChw =
                countUncomplicatedGIManagedByChw encountersForSelectedMonth

            resolvedGICases =
                countResolvedGICasesForSelectedMonth dateLastDayOfSelectedMonth acuteIllnessData
        in
        [ div [ class "ui grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.DiagnosedCases) (String.fromInt totalDaignosed)
                , chwCard language (Translate.Dashboard Translate.UncomplicatedGIInfectionByCHWS) (String.fromInt uncomplicatedGIManagedByChw)
                , chwCard language (Translate.Dashboard Translate.ComplicatedGIInfectionsReferredToHc) (String.fromInt complicatedGISentToHC)
                ]
            ]
        , div [ class "ui centered grid" ]
            [ div [ class "three column row" ]
                [ chwCard language (Translate.Dashboard Translate.ResolvedCases) (String.fromInt resolvedGICases ++ " : " ++ String.fromInt managedGI)
                ]
            ]
        ]

    else
        [ div [ class "ui grid" ]
            [ div [ class "two column row" ]
                [ chwCard language (Translate.Dashboard Translate.DiagnosedCases) (String.fromInt totalDaignosed)
                , chwCard language (Translate.Dashboard Translate.HospitalReferrals) (String.fromInt complicatedGISentToHC)
                ]
            ]
        ]


viewNutritionChartsPage : Language -> NominalDate -> NutritionPageData -> Model -> List (Html Msg)
viewNutritionChartsPage language currentDate data model =
    let
        links =
            case model.programTypeFilter of
                FilterProgramFbf ->
                    div [ class "sixteen wide column" ]
                        [ viewDashboardPagesLinks language
                        ]

                _ ->
                    emptyNode
    in
    [ div [ class "ui grid" ]
        [ div [ class "eight wide column" ]
            [ viewGoodNutrition language data.caseNutritionTotalsThisYear data.caseNutritionTotalsLastYear
            ]
        , div [ class "eight wide column" ]
            [ viewTotalEncounters language data.totalEncounters
            ]
        , div [ class "sixteen wide column" ]
            [ viewMonthlyChart language currentDate MonthlyChartTotals FilterBeneficiariesChart data.totalsGraphData model.currentBeneficiariesChartsFilter
            ]
        , div [ class "sixteen wide column" ]
            [ viewMonthlyChart language currentDate MonthlyChartIncidence FilterBeneficiariesIncidenceChart data.newCasesGraphData model.currentBeneficiariesIncidenceChartsFilter
            ]
        , links
        ]
    ]


viewGroupEducationPage : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewGroupEducationPage language currentDate assembled model =
    Maybe.map (viewGroupEducationDrillIn language assembled.patientsDetails)
        model.educationSessionDrillIn
        |> Maybe.withDefault (viewGroupEducationStandard language currentDate assembled model)


viewGroupEducationDrillIn : Language -> Dict PersonIdentifier PatientDetails -> EducationSessionData -> List (Html Msg)
viewGroupEducationDrillIn language patientsDetails session =
    let
        topics =
            EverySet.toList session.topics
                |> List.map (\topic -> li [] [ text <| translate language <| Translate.EducationTopic topic ])
                |> ul [ class "session-topics" ]

        tableContent =
            headerRow
                :: (List.map viewPatientEntry <|
                        EverySet.toList session.participants
                   )

        headerRow =
            div [ class "entry header" ]
                [ div [ class "name" ] [ text <| translate language Translate.Name ]
                , div [ class "gender" ] [ text <| translate language Translate.GenderLabel ]
                ]

        viewPatientEntry patientId =
            Dict.get patientId patientsDetails
                |> Maybe.map
                    (\patient ->
                        div [ class "entry" ]
                            [ div [ class "name" ] [ text patient.name ]
                            , div [ class "gender" ] [ text <| translate language <| Translate.Gender patient.gender ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    [ topics
    , div [ class "ui grid" ]
        [ div [ class "patients-table" ]
            tableContent
        ]
    ]


viewGroupEducationStandard : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewGroupEducationStandard language currentDate assembled model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        sessionsDuringSelectedMonth =
            List.filter (.startDate >> withinSelectedMonth dateLastDayOfSelectedMonth) assembled.groupEducationData

        attendeesDuringSelectedMonth =
            List.concatMap (.participants >> EverySet.toList) sessionsDuringSelectedMonth

        uniqueAttendeesDuringSelectedMonth =
            EverySet.fromList attendeesDuringSelectedMonth

        tableContent =
            headerRow
                :: List.map viewSessionEntry sessionsDuringSelectedMonth

        headerRow =
            div [ class "entry header" ]
                [ div [ class "topics" ] [ text <| translate language Translate.GroupEducation ]
                , div [ class "date" ] [ text <| translate language Translate.StartDate ]
                , div [ class "attendance" ] [ text <| translate language Translate.Attendance ]
                ]

        viewSessionEntry session =
            let
                drillInArrributes =
                    if isJust model.selectedVillageFilter then
                        [ class "icon-forward"
                        , onClick <| SetEducationSessionDrillIn (Just session)
                        ]

                    else
                        [ class "icon-forward hidden" ]
            in
            div [ class "entry" ]
                [ EverySet.toList session.topics
                    |> List.map (\topic -> li [] [ text <| translate language <| Translate.EducationTopic topic ])
                    |> ul [ class "topics" ]
                , div [ class "date" ]
                    [ text <| formatDDMMYYYY session.startDate ]
                , div [ class "attendance" ]
                    [ text <| String.fromInt <| EverySet.size session.participants ]
                , div drillInArrributes []
                ]
    in
    [ monthSelector language dateLastDayOfSelectedMonth model
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.NumberOfGroupSessions) (String.fromInt <| List.length sessionsDuringSelectedMonth)
            , chwCard language (Translate.Dashboard Translate.TotalAttendees) (String.fromInt <| List.length attendeesDuringSelectedMonth)
            , chwCard language (Translate.Dashboard Translate.UniquePatients) (String.fromInt <| EverySet.size uniqueAttendeesDuringSelectedMonth)
            ]
        ]
    , div [ class "ui grid" ]
        [ div [ class "sessions-table" ]
            tableContent
        ]
    ]


viewPrenatalPage : Language -> NominalDate -> Bool -> AssembledData -> Model -> List (Html Msg)
viewPrenatalPage language currentDate isChw assembled model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        newlyIdentifiedPreganancies =
            countNewlyIdentifiedPregananciesForSelectedMonth dateLastDayOfSelectedMonth isChw assembled.prenatalData

        currentlyPregnant =
            countCurrentlyPregnantForSelectedMonth dateLastDayOfSelectedMonth isChw assembled.prenatalData

        pregnanciesDueWithin4Months =
            countPregnanciesDueWithin4MonthsForSelectedMonth dateLastDayOfSelectedMonth isChw assembled.prenatalData

        currentlyPregnantWithDangerSigns =
            countCurrentlyPregnantWithDangerSignsForSelectedMonth dateLastDayOfSelectedMonth isChw assembled.prenatalData

        secondRow =
            let
                deliveriesAtFacility =
                    countDeliveriesAtLocationForSelectedMonth dateLastDayOfSelectedMonth FacilityDelivery assembled.prenatalData
            in
            if isChw then
                let
                    deliveriesAtHome =
                        countDeliveriesAtLocationForSelectedMonth dateLastDayOfSelectedMonth HomeDelivery assembled.prenatalData
                in
                [ chwCard language (Translate.Dashboard Translate.WithDangerSigns) (String.fromInt currentlyPregnantWithDangerSigns)
                , chwCard language (Translate.Dashboard Translate.HomeDeliveries) (String.fromInt deliveriesAtHome)
                , chwCard language (Translate.Dashboard Translate.HealthFacilityDeliveries) (String.fromInt deliveriesAtFacility)
                ]

            else
                let
                    pregnanciesWith4VisitsOrMore =
                        countPregnanciesWith4VisitsOrMoreForSelectedMonth dateLastDayOfSelectedMonth assembled.prenatalData

                    hospitalReferrals =
                        countHospitalReferralsForSelectedMonth dateLastDayOfSelectedMonth assembled.prenatalData
                in
                [ chwCard language (Translate.Dashboard Translate.PregnanciesWith4VisitsOrMore) (String.fromInt pregnanciesWith4VisitsOrMore)
                , chwCard language (Translate.Dashboard Translate.HealthCenterDeliveries) (String.fromInt deliveriesAtFacility)
                , chwCard language (Translate.Dashboard Translate.HospitalReferrals) (String.fromInt hospitalReferrals)
                ]

        prenatalDiagnosesSection =
            if isChw then
                []

            else
                List.filterMap
                    (\pregnancy ->
                        let
                            nurseEncounters =
                                List.filter isNurseEncounter pregnancy.encounters
                        in
                        if List.isEmpty nurseEncounters then
                            Nothing

                        else
                            Just { pregnancy | encounters = nurseEncounters }
                    )
                    assembled.prenatalData
                    |> viewPrenatalDiagnosesSection language dateLastDayOfSelectedMonth currentlyPregnantWithDangerSigns
    in
    [ monthSelector language dateLastDayOfSelectedMonth model
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.NewPregnancy) (String.fromInt newlyIdentifiedPreganancies)
            , chwCard language (Translate.Dashboard Translate.CurrentPregnancies) (String.fromInt currentlyPregnant)
            , chwCard language (Translate.Dashboard Translate.Within4MonthsOfDueDate) (String.fromInt pregnanciesDueWithin4Months)
            ]
        ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            secondRow
        ]
    ]
        ++ prenatalDiagnosesSection


viewPrenatalDiagnosesSection : Language -> NominalDate -> Int -> List PrenatalDataItem -> List (Html Msg)
viewPrenatalDiagnosesSection language currentDate currentlyPregnantWithDangerSigns data =
    let
        totalHighRiskPregnancies =
            newlyDiagnosesSyphilisPregnancies
                ++ newlyDiagnosesPreeclampsiaPregnancies
                ++ newlyDiagnosesEclampsiaPregnancies
                ++ newlyDiagnosesSevereAnemiaPregnancies
                ++ newlyDiagnosesAcuteMalnutritionPregnancies
                ++ newlyDiagnosesGestationalDiabetesPregnancies
                ++ newlyDiagnosesHIVPregnancies
                |> List.map .identifier
                |> EverySet.fromList
                |> EverySet.size
                |> (+) currentlyPregnantWithDangerSigns

        newlyDiagnosesSyphilisPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate syphilisDiagnoses data

        newlyDiagnosesPreeclampsiaPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate preeclampsiaDiagnoses data

        newlyDiagnosesEclampsiaPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate [ DiagnosisEclampsia ] data

        newlyDiagnosesSevereAnemiaPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate severeAnemiaDiagnoses data

        newlyDiagnosesAcuteMalnutritionPregnancies =
            filterNewlyDiagnosesMalnutritionForSelectedMonth currentDate data

        newlyDiagnosesGestationalDiabetesPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate
                [ DiagnosisGestationalDiabetesInitialPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                data

        newlyDiagnosesHIVPregnancies =
            filterNewlyDiagnosesCasesForSelectedMonth currentDate
                [ DiagnosisHIVInitialPhase, DiagnosisHIVRecurrentPhase ]
                data
    in
    [ div [ class "separator" ] []
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.TotalHighRiskPregnancies (String.fromInt totalHighRiskPregnancies) ]
        ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.Syphilis (String.fromInt <| List.length newlyDiagnosesSyphilisPregnancies)
            , chwCard language Translate.Preeclampsia (String.fromInt <| List.length newlyDiagnosesPreeclampsiaPregnancies)
            , chwCard language Translate.Eclampsia (String.fromInt <| List.length newlyDiagnosesEclampsiaPregnancies)
            ]
        ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.SevereAnemia (String.fromInt <| List.length newlyDiagnosesSevereAnemiaPregnancies)
            , chwCard language Translate.AcuteMalnutrition (String.fromInt <| List.length newlyDiagnosesAcuteMalnutritionPregnancies)
            , chwCard language Translate.DangerSigns (String.fromInt currentlyPregnantWithDangerSigns)
            ]
        ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language Translate.GestationalDiabetes (String.fromInt <| List.length newlyDiagnosesGestationalDiabetesPregnancies)
            , chwCard language Translate.HIV (String.fromInt <| List.length newlyDiagnosesHIVPregnancies)
            ]
        ]
    ]


chwCard : Language -> TranslationId -> String -> Html any
chwCard language titleTransId value =
    div [ class "column" ]
        [ viewChwCard language titleTransId value ]


viewChwCard : Language -> TranslationId -> String -> Html any
viewChwCard language titleTransId value =
    div [ class "ui segment dashboard-card chw" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ text <| translate language titleTransId ]
            , div [ class "value this-year" ] [ text value ]
            ]
        ]


viewGoodNutrition : Language -> List CaseNutritionTotal -> List CaseNutritionTotal -> Html Msg
viewGoodNutrition language caseNutritionTotalsThisYear caseNutritionTotalsLastYear =
    let
        allThisYear =
            List.length caseNutritionTotalsThisYear

        goodThisYear =
            countGoodNutrition caseNutritionTotalsThisYear

        goodLastYear =
            countGoodNutrition caseNutritionTotalsLastYear

        countGoodNutrition : List CaseNutritionTotal -> Int
        countGoodNutrition totalsList =
            totalsList
                |> List.map
                    (\totals ->
                        if
                            (countAbnormal totals.stunting
                                + countAbnormal totals.underweight
                                + countAbnormal totals.wasting
                                + countAbnormal totals.muac
                                + countAbnormal totals.nutritionSigns
                            )
                                == 0
                        then
                            1

                        else
                            0
                    )
                |> List.sum

        countAbnormal : Dict Int Nutrition -> Int
        countAbnormal dict =
            Dict.values dict
                |> List.map (\abnormal -> abnormal.severeNutrition + abnormal.moderateNutrition)
                |> List.sum

        percentageThisYear =
            (toFloat goodThisYear / toFloat allThisYear)
                * 100
                |> round

        percentageLastYear =
            calculatePercentage goodThisYear goodLastYear
                |> round

        statsCard =
            { title = translate language <| Translate.Dashboard Translate.GoodNutritionLabel
            , cardClasses = "good-nutrition"
            , cardAction = Nothing
            , value = percentageThisYear
            , valueSeverity = Neutral
            , valueIsPercentage = True
            , previousPercentage = percentageLastYear
            , previousPercentageLabel = OneYear
            , newCases = Nothing
            }
    in
    viewCard language statsCard


viewTotalEncounters : Language -> Periods -> Html Msg
viewTotalEncounters language encounters =
    let
        percentageDiff =
            calculatePercentage encounters.thisYear encounters.lastYear
                |> round

        statsCard =
            { title = translate language <| Translate.Dashboard Translate.TotalEncountersLabel
            , cardClasses = "total-encounters"
            , cardAction = Nothing
            , value = encounters.thisYear
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = percentageDiff
            , previousPercentageLabel = OneYear
            , newCases = Nothing
            }
    in
    viewCard language statsCard


viewMalnourishedCards : Language -> List MalnorishedNutritionData -> List MalnorishedNutritionData -> Html Msg
viewMalnourishedCards language malnourishedCurrentMonth malnourishedPreviousMonth =
    let
        total =
            List.length malnourishedCurrentMonth

        totalBefore =
            List.length malnourishedPreviousMonth

        totalPercentage =
            calculatePercentage total totalBefore
                |> round

        malnourishedBeforeIdentifiers =
            malnourishedPreviousMonth
                |> List.map .identifier

        malnourishedNewCases =
            malnourishedCurrentMonth
                |> List.filter (\item -> List.member item.identifier malnourishedBeforeIdentifiers |> not)
                |> List.length

        totalCard =
            { title = translate language <| Translate.Dashboard Translate.TotalMalnourished
            , cardClasses = "stats-card total-malnourished"
            , cardAction = Just (NavigateToStuntingTable FilterTotal)
            , value = total
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = totalPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just malnourishedNewCases
            }

        severe =
            malnourishedCurrentMonth
                |> List.filter (.nutritionStatus >> (==) Backend.Dashboard.Model.Severe)

        severeBefore =
            malnourishedPreviousMonth
                |> List.filter (.nutritionStatus >> (==) Backend.Dashboard.Model.Severe)

        severePercentage =
            calculatePercentage (List.length severe) (List.length severeBefore)
                |> round

        severeBeforeIdentifiers =
            severeBefore
                |> List.map .identifier

        severeNewCases =
            severe
                |> List.filter (\item -> List.member item.identifier severeBeforeIdentifiers |> not)
                |> List.length

        severeCard =
            { title = translate language <| Translate.Dashboard Translate.SeverelyMalnourished
            , cardClasses = "stats-card severely-malnourished"
            , cardAction = Just (NavigateToStuntingTable FilterSevere)
            , value = List.length severe
            , valueSeverity = Severe
            , valueIsPercentage = False
            , previousPercentage = severePercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just severeNewCases
            }

        moderate =
            malnourishedCurrentMonth
                |> List.filter (.nutritionStatus >> (==) Backend.Dashboard.Model.Moderate)

        moderateBefore =
            malnourishedPreviousMonth
                |> List.filter (.nutritionStatus >> (==) Backend.Dashboard.Model.Moderate)

        moderatePercentage =
            calculatePercentage (List.length moderate) (List.length moderateBefore)
                |> round

        moderateBeforeIdentifiers =
            moderateBefore
                |> List.map .identifier

        moderateNewCases =
            moderate
                |> List.filter (\item -> List.member item.identifier moderateBeforeIdentifiers |> not)
                |> List.length

        moderateCard =
            { title = translate language <| Translate.Dashboard Translate.ModeratelyMalnourished
            , cardClasses = "stats-card moderately-malnourished"
            , cardAction = Just (NavigateToStuntingTable FilterModerate)
            , value = List.length moderate
            , valueSeverity = Moderate
            , valueIsPercentage = False
            , previousPercentage = moderatePercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just moderateNewCases
            }
    in
    div [ class "row" ]
        [ div [ class "column" ] [ viewCard language totalCard ]
        , div [ class "column" ] [ viewCard language severeCard ]
        , div [ class "column" ] [ viewCard language moderateCard ]
        ]


viewMiscCards : Language -> NominalDate -> DashboardStats -> DashboardStats -> Html Msg
viewMiscCards language currentDate stats monthBeforeStats =
    let
        totalNewBeneficiaries =
            stats.childrenBeneficiaries
                |> List.length

        totalNewBeneficiariesBefore =
            monthBeforeStats.childrenBeneficiaries
                |> List.length

        totalNewBeneficiariesTable =
            List.foldl
                (\childrenBeneficiaries accum ->
                    { identifier = childrenBeneficiaries.identifier
                    , gender = childrenBeneficiaries.gender
                    , birthDate = childrenBeneficiaries.birthDate
                    , motherIdentifier = childrenBeneficiaries.motherIdentifier
                    , expectedDate = currentDate
                    }
                        :: accum
                )
                []
                stats.childrenBeneficiaries

        totalNewBeneficiariesPercentage =
            calculatePercentage totalNewBeneficiaries totalNewBeneficiariesBefore
                |> round

        totalNewBeneficiariesTitle =
            translate language <| Translate.Dashboard Translate.NewBeneficiaries

        totalNewBeneficiariesCard =
            { title = totalNewBeneficiariesTitle
            , cardClasses = "stats-card new-beneficiaries"
            , cardAction = Just (SetModalState <| Just (StatisticsModal totalNewBeneficiariesTitle totalNewBeneficiariesTable))
            , value = totalNewBeneficiaries
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = totalNewBeneficiariesPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Nothing
            }

        completedProgramCount =
            List.length stats.completedPrograms

        completedProgramBeforeCount =
            List.length monthBeforeStats.completedPrograms

        completedProgramPercentage =
            calculatePercentage completedProgramCount completedProgramBeforeCount
                |> round

        completedProgramTitle =
            translate language <| Translate.Dashboard Translate.CompletedProgramLabel

        completedProgramCard =
            { title = completedProgramTitle
            , cardClasses = "stats-card completed-program"
            , cardAction = Just (SetModalState <| Just (StatisticsModal completedProgramTitle stats.completedPrograms))
            , value = completedProgramCount
            , valueSeverity = Good
            , valueIsPercentage = False
            , previousPercentage = completedProgramPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Nothing
            }

        missedSessionsCount =
            List.length stats.missedSessions

        missedSessionsBeforeCount =
            List.length monthBeforeStats.missedSessions

        missedSessionsPercentage =
            calculatePercentage missedSessionsCount missedSessionsBeforeCount
                |> round

        missedSessionsTitle =
            translate language <| Translate.Dashboard Translate.MissedSessionsLabel

        missedSessionsCard =
            { title = missedSessionsTitle
            , cardClasses = "stats-card missed-sessions"
            , cardAction = Just (SetModalState <| Just (StatisticsModal missedSessionsTitle stats.missedSessions))
            , value = missedSessionsCount
            , valueSeverity = Severe
            , valueIsPercentage = False
            , previousPercentage = missedSessionsPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Nothing
            }
    in
    div [ class "row" ]
        [ div [ class "column" ] [ viewCard language totalNewBeneficiariesCard ]
        , div [ class "column" ] [ viewCard language completedProgramCard ]
        , div [ class "column" ] [ viewCard language missedSessionsCard ]
        ]


viewCard : Language -> StatsCard -> Html Msg
viewCard language statsCard =
    let
        ( cardAction, cardLinkClass ) =
            case statsCard.cardAction of
                Nothing ->
                    ( []
                    , ""
                    )

                Just action ->
                    ( [ onClick action ]
                    , "link"
                    )

        cardAttributes =
            (class <| "ui segment blue dashboard-card " ++ statsCard.cardClasses ++ " " ++ cardLinkClass) :: cardAction

        severityClass =
            case statsCard.valueSeverity of
                Neutral ->
                    "neutral"

                Good ->
                    "good"

                Moderate ->
                    "moderate"

                Severe ->
                    "severe"

        valueSuffix =
            if statsCard.valueIsPercentage then
                "%"

            else
                ""

        viewPercentageArrow icon =
            img
                [ class "arrow"
                , src <| "assets/images/" ++ icon ++ ".svg"
                ]
                []

        percentageArrow =
            if statsCard.previousPercentage > 0 then
                viewPercentageArrow "icon-up"

            else if statsCard.previousPercentage < 0 then
                viewPercentageArrow "icon-down"

            else
                emptyNode
    in
    div
        cardAttributes
        [ div [ class "content" ]
            [ div [ class "header" ] [ text statsCard.title ]
            , div [ class <| "percentage this-year severity severity-" ++ severityClass ] [ text <| String.fromInt statsCard.value ++ valueSuffix ]
            , div [ class "total last-year" ]
                [ span [ class "percentage" ]
                    [ percentageArrow
                    , i [] [ text <| String.fromInt statsCard.previousPercentage ++ "%" ]
                    ]
                , span [ class "percentage-label" ] [ translateText language <| Translate.Dashboard <| Translate.PercentageLabel statsCard.previousPercentageLabel ]
                ]
            , statsCard.newCases
                |> Maybe.map
                    (\newCases ->
                        div [ class "new-cases" ]
                            [ span [ class "label" ] [ translateText language <| Translate.Dashboard Translate.NewCasesLabel ]
                            , span [ class "new-cases-value" ] [ text <| String.fromInt newCases ]
                            ]
                    )
                |> showMaybe
            ]
        ]


viewBeneficiariesGenderFilter : Language -> Model -> Html Msg
viewBeneficiariesGenderFilter language model =
    let
        renderButton gender =
            let
                genderTranslated =
                    case gender of
                        Boys ->
                            translateText language <| Translate.Dashboard Translate.BoysFilterLabel

                        Girls ->
                            translateText language <| Translate.Dashboard Translate.GirlsFilterLabel
            in
            span
                [ classList
                    [ ( "active", model.beneficiariesGender == gender )
                    , ( "dashboard-filter", True )
                    ]
                , onClick <| SetFilterGender gender
                ]
                [ genderTranslated
                ]
    in
    div [ class "filters" ]
        (List.map renderButton filterGenders)


viewBeneficiariesTable : Language -> NominalDate -> DashboardStats -> DashboardStats -> List MalnorishedNutritionData -> Model -> Html Msg
viewBeneficiariesTable language currentDate stats currentPeriodStats malnourished model =
    let
        currentPeriodTotalBeneficiaries =
            case model.period of
                ThisMonth ->
                    let
                        minGraduationDate =
                            Date.floor Date.Month currentDate
                    in
                    stats.childrenBeneficiaries
                        |> List.filter (\child -> Date.compare minGraduationDate child.graduationDate == LT)

                LastMonth ->
                    let
                        maxJoinDate =
                            Date.add Months -1 currentDate
                                |> toLastDayOfMonth

                        minGraduationDate =
                            Date.add Months -1 currentDate
                                |> Date.floor Date.Month
                    in
                    stats.childrenBeneficiaries
                        |> List.filter
                            (\child ->
                                (Date.compare child.memberSince maxJoinDate == LT)
                                    && (Date.compare minGraduationDate child.graduationDate == LT)
                            )

                _ ->
                    []

        currentPeriodTotalBeneficiariesByGender =
            applyGenderFilter model currentPeriodTotalBeneficiaries

        currentPeriodTotalBeneficiaries0_5 =
            applyAgeFilter currentDate filter0_5Func currentPeriodTotalBeneficiariesByGender

        currentPeriodTotalBeneficiaries6_8 =
            applyAgeFilter currentDate filter6_8Func currentPeriodTotalBeneficiariesByGender

        currentPeriodTotalBeneficiaries9_11 =
            applyAgeFilter currentDate filter9_11Func currentPeriodTotalBeneficiariesByGender

        currentPeriodTotalBeneficiaries12_25 =
            applyAgeFilter currentDate filter12_25Func currentPeriodTotalBeneficiariesByGender

        currentPeriodStatsFilteredByGender =
            filterStatsByGender model currentPeriodStats

        filterByAge filterFunc statsToFilter =
            filterStatsByAge
                currentDate
                filterFunc
                statsToFilter

        currentPeriodStats0_5 =
            filterByAge filter0_5Func currentPeriodStatsFilteredByGender

        currentPeriodStats6_8 =
            filterByAge filter6_8Func currentPeriodStatsFilteredByGender

        currentPeriodStats9_11 =
            filterByAge filter9_11Func currentPeriodStatsFilteredByGender

        currentPeriodStats12_25 =
            filterByAge filter12_25Func currentPeriodStatsFilteredByGender

        filter0_5Func =
            \{ months } -> months <= 5

        filter6_8Func =
            \{ months } -> months >= 6 && months <= 8

        filter9_11Func =
            \{ months } -> months >= 9 && months <= 11

        filter12_25Func =
            \{ months } -> months >= 12

        getBeneficiariesCount stats_ =
            lengthAsString stats_.childrenBeneficiaries

        getMissedSessionBeneficiariesCount stats_ =
            lengthAsString stats_.missedSessions

        malnourishedFilteredByGender =
            applyGenderFilter model malnourished

        malnourished0_5 =
            applyAgeFilter currentDate filter0_5Func malnourishedFilteredByGender |> lengthAsString

        malnourished6_8 =
            applyAgeFilter currentDate filter6_8Func malnourishedFilteredByGender |> lengthAsString

        malnourished9_11 =
            applyAgeFilter currentDate filter9_11Func malnourishedFilteredByGender |> lengthAsString

        malnourished12_25 =
            applyAgeFilter currentDate filter12_25Func malnourishedFilteredByGender |> lengthAsString

        lengthAsString list =
            List.length list
                |> String.fromInt
    in
    div [ class "ui blue segment fbf-beneficiaries" ]
        [ div [ class "header" ]
            [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.BeneficiariesLabel ]
            , viewBeneficiariesGenderFilter language model
            ]
        , div
            [ class "content" ]
            [ table [ class "ui very basic collapsing table" ]
                [ thead []
                    [ tr []
                        [ th [ class "label" ] [ translateText language <| Translate.Dashboard Translate.BeneficiariesTableLabel ]
                        , th [] [ text "0-5" ]
                        , th [] [ text "6-8" ]
                        , th [] [ text "9-11" ]
                        , th [] [ text "12-25" ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel Total ]
                        , td [] [ text <| lengthAsString currentPeriodTotalBeneficiaries0_5 ]
                        , td [] [ text <| lengthAsString currentPeriodTotalBeneficiaries6_8 ]
                        , td [] [ text <| lengthAsString currentPeriodTotalBeneficiaries9_11 ]
                        , td [] [ text <| lengthAsString currentPeriodTotalBeneficiaries12_25 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel New ]
                        , td [] [ text <| getBeneficiariesCount currentPeriodStats0_5 ]
                        , td [] [ text <| getBeneficiariesCount currentPeriodStats6_8 ]
                        , td [] [ text <| getBeneficiariesCount currentPeriodStats9_11 ]
                        , td [] [ text <| getBeneficiariesCount currentPeriodStats12_25 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel Missed ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount currentPeriodStats0_5 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount currentPeriodStats6_8 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount currentPeriodStats9_11 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount currentPeriodStats12_25 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel Malnourished ]
                        , td [] [ text malnourished0_5 ]
                        , td [] [ text malnourished6_8 ]
                        , td [] [ text malnourished9_11 ]
                        , td [] [ text malnourished12_25 ]
                        ]
                    ]
                ]
            ]
        ]


viewDashboardPagesLinks : Language -> Html Msg
viewDashboardPagesLinks language =
    div [ class "dashboards-links" ]
        [ div
            [ class "ui segment stats"
            , DashboardPage (PageNutrition PageStats)
                |> UserPage
                |> SetActivePage
                |> onClick
            ]
            [ i [ class "icon" ] []
            , span
                []
                [ span [ class "bold" ] [ translateText language <| Translate.Dashboard Translate.StatisticsFirstWordHelper ]
                , translateText language <| Translate.Dashboard Translate.StatisticsHelper
                ]
            , i [ class "arrow" ] []
            ]
        , div
            [ class "ui segment case"
            , DashboardPage (PageNutrition PageCaseManagement)
                |> UserPage
                |> SetActivePage
                |> onClick
            ]
            [ i [ class "icon" ] []
            , span
                []
                [ span [ class "bold" ] [ translateText language <| Translate.Dashboard Translate.CaseManagementFirstWordHelper ]
                , translateText language <| Translate.Dashboard Translate.CaseManagementHelper
                ]
            , i [ class "arrow" ] []
            ]
        ]


viewFamilyPlanning : Language -> DashboardStats -> Html Msg
viewFamilyPlanning language stats =
    div
        [ class "ui blue segment donut-chart family-planning" ]
        [ div [ class "header" ]
            [ h3 [ class "title" ] [ translateText language <| Translate.Dashboard Translate.FamilyPlanningLabel ]
            ]
        , div [ class "ui center aligned grid" ]
            [ div [ class "middle aligned row" ]
                [ viewFamilyPlanningDonutChart language stats ]
            ]
        ]


viewMonthlyChart : Language -> NominalDate -> MonthlyChartType -> FilterType -> Dict Int TotalBeneficiaries -> DashboardFilter -> Html Msg
viewMonthlyChart language currentDate chartType filterType data currentFilter =
    let
        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        ( thisYear, lastYear ) =
            data
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.Extra.splitAt currentMonth

        orderedData =
            (lastYear ++ thisYear)
                |> Dict.fromList

        caption =
            case chartType of
                MonthlyChartTotals ->
                    div [ class "title left floated column" ]
                        [ text <|
                            translate language (Translate.Dashboard Translate.TotalBeneficiaries)
                                ++ " "
                                ++ translate language (Translate.Dashboard <| Translate.Filter currentFilter)
                                ++ " (%)"
                        ]

                MonthlyChartIncidence ->
                    div [ class "title left floated column" ]
                        [ div []
                            [ text <|
                                translate language (Translate.Dashboard Translate.IncidenceOf)
                                    ++ " "
                                    ++ translate language (Translate.Dashboard <| Translate.Filter currentFilter)
                                    ++ " (%)"
                            ]
                        , div [ class "helper" ]
                            [ text <| "(" ++ translate language (Translate.Dashboard Translate.NewCasesPerMonth) ++ ")"
                            ]
                        ]

        chartData =
            Dict.foldl
                (\key totalBeneficiaries accum ->
                    let
                        month =
                            numberToMonth key
                    in
                    case currentFilter of
                        Stunting ->
                            Dict.insert month totalBeneficiaries.stunting accum

                        Underweight ->
                            Dict.insert month totalBeneficiaries.underweight accum

                        Wasting ->
                            Dict.insert month totalBeneficiaries.wasting accum

                        MUAC ->
                            Dict.insert month totalBeneficiaries.muac accum

                        MissedSession ->
                            accum
                )
                Dict.empty
                orderedData
                |> Dict.toList

        yScaleMaxList =
            let
                choose x y =
                    let
                        chosenX =
                            if x.moderateNutrition > x.severeNutrition then
                                x.moderateNutrition

                            else
                                x.severeNutrition
                    in
                    if chosenX > y then
                        chosenX

                    else
                        y
            in
            List.map (\( _, value ) -> choose value 0) chartData

        maybeScaleMax =
            List.maximum yScaleMaxList

        yScaleMax =
            maybeScaleMax
                -- Don't allow the y access to be less than 3.
                |> Maybe.map
                    (\max ->
                        if max < 3 then
                            3

                        else
                            max
                    )
                |> Maybe.withDefault 1

        -- Add 20% to the top of the graph above the max
        yScaleMaxEnhanced =
            toFloat yScaleMax + (toFloat yScaleMax * 0.2)
    in
    div [ class "ui segment blue dashboards-monthly-chart" ]
        [ div [ class "header" ]
            [ caption
            , List.map (viewFilter language filterType currentFilter) monthlyChartFilters
                |> div [ class "filters" ]
            ]
        , div [ class "content" ]
            [ viewBarsChartLegend language
            , viewBarChart chartData yScaleMaxEnhanced
            ]
        ]


viewBarChart : List ( Month, Nutrition ) -> Float -> Html Msg
viewBarChart data yScaleMax =
    svg [ viewBox 0 0 barChartWidth barChartHeight ]
        [ g [ Explicit.class [ "grid gird-y" ] ] <| List.indexedMap yGridLine <| Scale.ticks gridYScale 4
        , g [ Explicit.class [ "grid gird-x" ] ] <| List.indexedMap xGridLine <| Scale.ticks gridXScale 12
        , g [ transform [ Translate (padding - 1) (barChartHeight - padding) ] ]
            [ xAxis data ]
        , g [ transform [ Translate (padding + 1) padding ] ]
            [ yAxis yScaleMax ]
        , g [ transform [ Translate padding padding ], Explicit.class [ "data" ] ] <|
            List.map (column (xScale data) yScaleMax) data
        ]


viewBarsChartLegend : Language -> Html Msg
viewBarsChartLegend language =
    div [ class "legend" ]
        [ div []
            [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                [ Svg.circle [ cx "50", cy "50", r "50", Explicit.class [ "moderate" ] ] []
                ]
            , span [] [ translateText language <| Translate.Dashboard Translate.Moderate ]
            ]
        , div []
            [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                [ Svg.circle [ cx "50", cy "50", r "50", Explicit.class [ "severe" ] ] []
                ]
            , span [] [ translateText language <| Translate.Dashboard Translate.Severe ]
            ]
        ]


viewFilter : Language -> FilterType -> DashboardFilter -> DashboardFilter -> Html Msg
viewFilter language filterType currentChartFilter filter =
    let
        filterAction =
            case filterType of
                FilterBeneficiariesChart ->
                    SetFilterBeneficiariesChart filter FilterBeneficiariesChart

                FilterBeneficiariesIncidenceChart ->
                    SetFilterBeneficiariesChart filter FilterBeneficiariesIncidenceChart

                FilterCaseManagement ->
                    SetFilterCaseManagement filter
    in
    span
        [ classList
            [ ( "dashboard-filter", True )
            , ( "active", filter == currentChartFilter )
            ]
        , onClick <| filterAction
        ]
        [ translateText language <| Translate.Dashboard <| Translate.Filter filter ]


viewSubFilter : Language -> DashboardSubFilter -> DashboardSubFilter -> Html Msg
viewSubFilter language currentSubFilter filter =
    span
        [ classList
            [ ( "dashboard-filter", True )
            , ( "active", filter == currentSubFilter )
            ]
        , onClick <| SetSubFilterCaseManagement filter
        ]
        [ translateText language <| Translate.Dashboard <| Translate.SubFilter filter ]


viewFamilyPlanningDonutChart : Language -> DashboardStats -> Html Msg
viewFamilyPlanningDonutChart language stats =
    let
        dict =
            getFamilyPlanningSignsCounter stats
    in
    if Dict.isEmpty dict then
        div [ class "no-data-message" ] [ translateText language <| Translate.Dashboard Translate.NoDataForPeriod ]

    else
        let
            totalWomen =
                stats.familyPlanning
                    |> List.length

            totalNoFamilyPlanning =
                Dict.get NoFamilyPlanning dict
                    |> Maybe.withDefault 0

            useFamilyPlanning =
                totalWomen - totalNoFamilyPlanning

            totalPercent =
                useFamilyPlanning * 100 // totalWomen

            signs =
                dict
                    |> Dict.toList
                    |> List.filter (\( sign, _ ) -> sign /= NoFamilyPlanning)
                    |> List.sortBy (\( name, _ ) -> Debug.toString name)
        in
        div [ class "content" ]
            [ viewPieChart familyPlanningSignsColors signs
            , div [ class "in-chart" ]
                [ div [ class "stats" ]
                    [ span [ class "percentage neutral" ] [ text <| String.fromInt totalPercent ++ "%" ]
                    , text " "
                    , span [ class "use-label" ] [ translateText language <| Translate.Dashboard Translate.UseFamilyPlanning ]
                    , div [ class "count" ]
                        [ translateText language <|
                            Translate.Dashboard <|
                                Translate.FamilyPlanningOutOfWomen
                                    { total = totalWomen
                                    , useFamilyPlanning = useFamilyPlanning
                                    }
                        ]
                    ]
                ]
            , viewPieChartLegend language Translate.FamilyPlanningSignLabel familyPlanningSignToColor signs
            ]


viewPieChart : Dict a Color -> List ( a, Int ) -> Svg msg
viewPieChart colors values =
    let
        arcs =
            List.map (Tuple.second >> toFloat) values

        signs =
            List.map Tuple.first values

        pieData =
            arcs
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , padAngle = 0
                        , cornerRadius = 0
                    }
    in
    svg [ Explicit.class [ "pie-chart" ], viewBox 0 0 pieChartWidth pieChartHeight ]
        [ annular colors signs pieData ]


annular : Dict a Color -> List a -> List Arc -> Svg msg
annular colors signs pieData =
    let
        getColor index =
            List.Extra.getAt index signs
                |> Maybe.andThen (\sign -> Dict.get sign colors)
                |> Maybe.withDefault Color.black

        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius - 60 })
                [ fill <| Paint <| getColor index ]
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice pieData
        ]


viewPieChartLegend : Language -> (a -> TranslationId) -> (a -> Color) -> List ( a, Int ) -> Html Msg
viewPieChartLegend language translateFunc colorFunc signs =
    let
        totalSigns =
            List.map Tuple.second signs
                |> List.sum
                |> toFloat
    in
    div [ class "legend" ]
        (List.map
            (\( sign, usage ) ->
                let
                    label =
                        translate language <| translateFunc sign

                    percentage =
                        round (100 * toFloat usage / totalSigns)

                    -- We want to prevent displaying 0% in case there was usage.
                    normalizedPercentage =
                        if usage > 0 && percentage == 0 then
                            "1"

                        else
                            toString percentage
                in
                div [ class "legend-item" ]
                    [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                        [ Svg.circle [ cx "50", cy "50", r "40", fill <| Paint <| colorFunc sign ] []
                        ]
                    , span [] [ text <| label ++ " (" ++ normalizedPercentage ++ "%)" ]
                    ]
            )
            signs
        )


viewCustomModal : Language -> DashboardPage -> Bool -> Nurse -> AssembledData -> ModelIndexedDb -> Model -> Html Msg
viewCustomModal language page isChw nurse assembled db model =
    model.modalState
        |> Maybe.map
            (\state ->
                case state of
                    StatisticsModal title data ->
                        viewStatsTableModal language title assembled.patientsDetails data

                    FiltersModal ->
                        viewFiltersModal language page isChw nurse assembled.healthCenterVillages db model
            )
        |> viewModal


viewStatsTableModal : Language -> String -> Dict PersonIdentifier PatientDetails -> List ParticipantStats -> Html Msg
viewStatsTableModal language title patientsDetails data =
    div [ class "ui tiny active modal segment blue" ]
        [ div
            [ class "header" ]
            [ div [ class "title left floated column" ] [ text title ]
            , span
                [ class "overlay-close right floated column"
                , onClick <| SetModalState Nothing
                ]
                [ text "X" ]
            ]
        , div
            [ class "content" ]
            [ table [ class "ui very basic collapsing celled table" ]
                [ thead []
                    [ tr []
                        [ th [ class "name" ] [ translateText language <| Translate.Name ]
                        , th [ class "mother-name" ] [ translateText language <| Translate.MotherNameLabel ]
                        , th [ class "phone-number" ] [ translateText language <| Translate.TelephoneNumber ]
                        ]
                    ]
                , tbody []
                    (List.map (viewModalTableRow patientsDetails) data)
                ]
            ]
        ]


viewFiltersModal : Language -> DashboardPage -> Bool -> Nurse -> List VillageId -> ModelIndexedDb -> Model -> Html Msg
viewFiltersModal language page isChw nurse healthCenterVillages db model =
    let
        programTypeFilterInputSection =
            if isChw then
                -- For CHW nurses, program type is always set to FilterProgramCommunity.
                []

            else
                let
                    options =
                        case page of
                            -- Nutrition group types filters are only relevant
                            -- on Nutrition page. For all others It's either
                            -- All Programs, or selected village.
                            PageNutrition PageCharts ->
                                [ FilterAllPrograms
                                , FilterProgramFbf
                                , FilterProgramPmtct
                                , FilterProgramSorwathe
                                , FilterProgramAchi
                                , FilterProgramCommunity
                                ]

                            _ ->
                                [ FilterAllPrograms
                                , FilterProgramCommunity
                                ]

                    programTypeFilterInput =
                        viewCustomSelectListInput (Just model.programTypeFilter)
                            options
                            filterProgramTypeToString
                            SetFilterProgramType
                            (Translate.FilterProgramType >> Translate.Dashboard >> translate language)
                            "select-input"
                            False
                in
                [ div [ class "helper" ] [ text <| translate language <| Translate.Dashboard Translate.ProgramType ]
                , programTypeFilterInput
                ]

        villageInputSection =
            if model.programTypeFilter /= FilterProgramCommunity then
                []

            else
                RemoteData.toMaybe db.villages
                    |> Maybe.map
                        (\villages ->
                            let
                                authorizedVillages =
                                    if isChw then
                                        Dict.filter
                                            (\villageId _ ->
                                                EverySet.member villageId nurse.villages
                                            )
                                            villages

                                    else
                                        villages

                                allOptions =
                                    if isChw then
                                        options

                                    else
                                        option [ value "", selected (model.selectedVillageFilter == Nothing) ] [ text "" ]
                                            :: options

                                options =
                                    List.filterMap
                                        (\villageId ->
                                            Dict.get villageId authorizedVillages
                                                |> Maybe.map
                                                    (\village ->
                                                        option
                                                            [ value (fromEntityUuid villageId)
                                                            , selected (model.selectedVillageFilter == Just villageId)
                                                            ]
                                                            [ text village.name ]
                                                    )
                                        )
                                        healthCenterVillages

                                villageInput =
                                    select
                                        [ onInput SetSelectedVillage
                                        , class "select-input"
                                        ]
                                        allOptions
                            in
                            [ div [ class "helper" ] [ text <| translate language Translate.Village ]
                            , villageInput
                            ]
                        )
                    |> Maybe.withDefault []

        disabled =
            (model.programTypeFilter == FilterProgramCommunity) && isNothing model.selectedVillageFilter
    in
    div [ class "ui active modal" ]
        [ div [ class "header" ]
            [ text <| translate language <| Translate.Dashboard Translate.Filters ]
        , div [ class "content" ] <|
            programTypeFilterInputSection
                ++ villageInputSection
        , viewCustomAction language (SetModalState Nothing) disabled Translate.Close
        ]


viewModalTableRow : Dict PersonIdentifier PatientDetails -> ParticipantStats -> Html Msg
viewModalTableRow patientsDetails rowData =
    let
        name =
            Dict.get rowData.identifier patientsDetails
                |> Maybe.map .name
                |> Maybe.withDefault ""

        ( motherName, phoneNumber ) =
            Maybe.andThen
                (\motherIdentifier ->
                    Dict.get motherIdentifier patientsDetails
                        |> Maybe.map (\details -> ( details.name, details.phoneNumber ))
                )
                rowData.motherIdentifier
                |> Maybe.withDefault ( "", Nothing )
    in
    tr []
        [ td [ class "name" ] [ text name ]
        , td [ class "mother-name" ] [ text motherName ]
        , td [ class "phone-number" ] [ text <| Maybe.withDefault "-" phoneNumber ]
        ]


filterStatsByAge : NominalDate -> ({ months : Int, days : Int } -> Bool) -> DashboardStats -> DashboardStats
filterStatsByAge currentDate func stats =
    let
        childrenBeneficiaries =
            applyAgeFilter currentDate func stats.childrenBeneficiaries

        completedPrograms =
            applyAgeFilter currentDate func stats.completedPrograms

        missedSessions =
            applyAgeFilter currentDate func stats.missedSessions
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiaries
        , completedPrograms = completedPrograms
        , missedSessions = missedSessions
    }


applyAgeFilter : NominalDate -> ({ months : Int, days : Int } -> Bool) -> List { a | birthDate : NominalDate } -> List { a | birthDate : NominalDate }
applyAgeFilter currentDate func list =
    List.filter (\item -> isDiffTruthy item.birthDate currentDate func) list


getFamilyPlanningSignsCounter : DashboardStats -> FamilyPlanningSignsCounter
getFamilyPlanningSignsCounter stats =
    List.foldl
        (\familyPlanning accum ->
            let
                currentCount sign =
                    Dict.get sign accum
                        |> Maybe.withDefault 0

                incrementCount sign accum_ =
                    Dict.insert
                        sign
                        (currentCount sign + 1)
                        accum_
            in
            if List.isEmpty familyPlanning.signs then
                accum

            else if List.member NoFamilyPlanning familyPlanning.signs then
                -- In case we have a `NoFamilyPlanning` we don't need to iterate over signs.
                incrementCount NoFamilyPlanning accum

            else
                -- Iterate over existing signs.
                List.foldl
                    (\sign innerAccum -> incrementCount sign innerAccum)
                    accum
                    familyPlanning.signs
        )
        Dict.empty
        stats.familyPlanning


withinThreePreviousMonths : Int -> Int -> Bool
withinThreePreviousMonths currentMonth monthNumber =
    if monthNumber == 13 then
        -- This indicates that we look at data of 13 months ago month.
        -- It's for sure not within last 3 month.
        False

    else
        case currentMonth of
            1 ->
                monthNumber > 9

            2 ->
                monthNumber > 10 || monthNumber == 1

            3 ->
                monthNumber == 12 || monthNumber == 1 || monthNumber == 2

            _ ->
                monthNumber < currentMonth && monthNumber >= (currentMonth - 3)


resolvePreviousMonth : Int -> Int
resolvePreviousMonth thisMonth =
    if thisMonth == 1 then
        12

    else
        thisMonth - 1


monthSelector : Language -> NominalDate -> Model -> Html Msg
monthSelector language dateLastDayOfSelectedMonth model =
    viewMonthSelector language dateLastDayOfSelectedMonth model.monthGap maxMonthGap ChangeMonthGap


viewNCDPage :
    Language
    -> NominalDate
    -> NCDSubPage
    -> AssembledData
    -> Model
    -> List (Html Msg)
viewNCDPage language currentDate activePage assembled model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        pageContent =
            case activePage of
                PageHypertension ->
                    viewHypertensionPage language dateLastDayOfSelectedMonth assembled.ncdData

                PageHIV ->
                    viewHIVPage language dateLastDayOfSelectedMonth assembled.ncdData assembled.pmtctData

                PageDiabetes ->
                    viewDiabetesPage language dateLastDayOfSelectedMonth assembled.ncdData assembled.prenatalData
    in
    [ viewNCDMenu language activePage
    , monthSelector language dateLastDayOfSelectedMonth model
    ]
        ++ pageContent


viewHypertensionPage : Language -> NominalDate -> List NCDDataItem -> List (Html Msg)
viewHypertensionPage language dateLastDayOfSelectedMonth dataItems =
    let
        totalCases =
            countTotalNumberOfPatientsWithHypertension dateLastDayOfSelectedMonth dataItems

        newCases =
            countNewlyIdentifieHypertensionCasesForSelectedMonth dateLastDayOfSelectedMonth dataItems
    in
    [ div [ class "ui grid" ]
        [ div [ class "two column row" ]
            [ chwCard language (Translate.Dashboard Translate.HypertensionCases) (String.fromInt totalCases)
            , chwCard language (Translate.Dashboard Translate.HypertensionNewCases) (String.fromInt newCases)
            ]
        ]
    ]


viewHIVPage : Language -> NominalDate -> List NCDDataItem -> List PMTCTDataItem -> List (Html Msg)
viewHIVPage language dateLastDayOfSelectedMonth dataItems pmtctData =
    let
        patientsWithHIV =
            generatePatientsWithHIV dateLastDayOfSelectedMonth dataItems

        totalCases =
            List.length patientsWithHIV

        -- Patients with HIV that participate at PMTCT group.
        managedByPMTCT =
            List.filter
                (\id ->
                    List.any
                        (\pmtctParticipant ->
                            (pmtctParticipant.identifier == id)
                                && -- PMTCT participation started at current month or before.
                                   withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth pmtctParticipant.startDate
                                && -- PMTCT participation ends at current month or after.
                                   withinOrAfterSelectedMonth dateLastDayOfSelectedMonth pmtctParticipant.endDate
                        )
                        pmtctData
                )
                patientsWithHIV
                |> List.length
    in
    [ div [ class "ui grid" ]
        [ div [ class "two column row" ]
            [ chwCard language (Translate.Dashboard Translate.TotalCases) (String.fromInt totalCases)
            , chwCard language (Translate.Dashboard Translate.ManagedByPMTCT) (String.fromInt managedByPMTCT)
            ]
        ]
    ]


viewDiabetesPage : Language -> NominalDate -> List NCDDataItem -> List PrenatalDataItem -> List (Html Msg)
viewDiabetesPage language dateLastDayOfSelectedMonth dataItems prenatalDataItems =
    let
        totalCases =
            countTotalNumberOfPatientsWithDiabetes dateLastDayOfSelectedMonth dataItems

        newCases =
            countNewlyIdentifiedDiabetesCasesForSelectedMonth dateLastDayOfSelectedMonth dataItems

        gestationalCases =
            let
                dataForNurses =
                    List.filterMap
                        (\pregnancy ->
                            let
                                nurseEncounters =
                                    List.filter isNurseEncounter pregnancy.encounters
                            in
                            if List.isEmpty nurseEncounters then
                                Nothing

                            else
                                Just { pregnancy | encounters = nurseEncounters }
                        )
                        prenatalDataItems
            in
            countTotalNumberOfPatientsWithGestationalDiabetes dateLastDayOfSelectedMonth dataForNurses
    in
    [ div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.TotalDiabeticCases) (String.fromInt totalCases)
            , chwCard language (Translate.Dashboard Translate.DiabetesNewCases) (String.fromInt newCases)
            , chwCard language Translate.GestationalDiabetes (String.fromInt gestationalCases)
            ]
        ]
    ]


viewChildWellnessPage :
    Language
    -> NominalDate
    -> Site
    -> ChildWellnessSubPage
    -> AssembledData
    -> Model
    -> List (Html Msg)
viewChildWellnessPage language currentDate site activePage assembled model =
    let
        dateLastDayOfSelectedMonth =
            resolveSelectedDateForMonthSelector currentDate model.monthGap

        pageContent =
            case activePage of
                PageChildWellnessOverview ->
                    viewChildWellnessOverviewPage language site dateLastDayOfSelectedMonth assembled.spvData assembled.childScoreboardData

                PageChildWellnessNutrition ->
                    viewChildWellnessNutritionPage language dateLastDayOfSelectedMonth assembled
    in
    [ viewChildWellnessMenu language activePage
    , monthSelector language dateLastDayOfSelectedMonth model
    ]
        ++ pageContent


viewChildWellnessOverviewPage : Language -> Site -> NominalDate -> List SPVDataItem -> List ChildScoreboardDataItem -> List (Html Msg)
viewChildWellnessOverviewPage language site dateLastDayOfSelectedMonth spvDataItems childScoreboardDataItem =
    let
        numberOfChildrenSeen =
            getEncountersForSelectedMonth dateLastDayOfSelectedMonth spvDataItems
                |> List.filter isNurseEncounter
                |> List.length

        -- For each participant, we resolve last SPV encounter.
        ecdDataItems =
            List.filterMap
                (\item ->
                    List.filter
                        (\encounter ->
                            isNurseEncounter encounter
                                && withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth encounter.startDate
                        )
                        item.encounters
                        |> List.sortWith (sortByDateDesc .startDate)
                        |> List.head
                )
                spvDataItems

        isNurseEncounter =
            .encounterType >> (==) PediatricCare

        ecdOnTrack =
            List.filter
                (\encounter -> EverySet.member NoECDMilstoneWarning encounter.warnings)
                ecdDataItems
                |> List.length

        ecdBehind =
            List.length ecdDataItems - ecdOnTrack

        spvDataDict =
            List.map (\item -> ( item.identifier, item )) spvDataItems
                |> Dict.fromList

        childScoreboardDict =
            List.map (\item -> ( item.identifier, item )) childScoreboardDataItem
                |> Dict.fromList

        allIdentifiers =
            Dict.keys spvDataDict
                ++ Dict.keys childScoreboardDict
                |> EverySet.fromList

        immunizationDataItems =
            EverySet.foldl
                (\identifier accum ->
                    let
                        spvItem =
                            Dict.get identifier spvDataDict

                        childScoreboardItem =
                            Dict.get identifier childScoreboardDict
                    in
                    case ( spvItem, childScoreboardItem ) of
                        ( Just spv, Just childScoreboard ) ->
                            { birthDate = Just spv.birthDate
                            , gender = spv.gender
                            , vaccinationProgress = generateVaccinationProgressDict site spv.gender spv.encounters childScoreboard.encounters
                            }
                                :: accum

                        ( Just spv, Nothing ) ->
                            { birthDate = Just spv.birthDate
                            , gender = spv.gender
                            , vaccinationProgress = generateVaccinationProgressDict site spv.gender spv.encounters []
                            }
                                :: accum

                        ( Nothing, Just childScoreboard ) ->
                            { birthDate = Just childScoreboard.birthDate
                            , gender = childScoreboard.gender
                            , vaccinationProgress = generateVaccinationProgressDict site childScoreboard.gender [] childScoreboard.encounters
                            }
                                :: accum

                        ( Nothing, Nothing ) ->
                            -- We never get here.
                            accum
                )
                []
                allIdentifiers

        isImmunizationOnTrack item =
            let
                -- Filter out vaccinations that were performed
                -- after the reference date.
                vaccinationProgressOnReferrenceDate =
                    Dict.map
                        (\_ dosesDict ->
                            Dict.filter
                                (\_ administeredDate ->
                                    Date.compare administeredDate dateLastDayOfSelectedMonth == LT
                                )
                                dosesDict
                        )
                        item.vaccinationProgress

                futureVaccinations =
                    generateFutureVaccinationsData dateLastDayOfSelectedMonth site item.birthDate item.gender False vaccinationProgressOnReferrenceDate

                closestDateForVaccination =
                    List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
                        |> List.sortWith Date.compare
                        |> List.head
            in
            Maybe.map
                (\closestDate ->
                    -- Closest date when vaccine is required is after last
                    -- day of selected month / current date, if current month
                    -- is selected.
                    Date.compare closestDate dateLastDayOfSelectedMonth == GT
                )
                closestDateForVaccination
                |> Maybe.withDefault
                    -- This indicates that there're no future vaccinations to be
                    -- done, and therefore, patient is on track.
                    True

        immunizationOnTrack =
            List.filter isImmunizationOnTrack immunizationDataItems
                |> List.length

        immunizationBehind =
            List.length immunizationDataItems - immunizationOnTrack

        ecdChartData =
            [ ( Translate.OnTrack, ecdOnTrack )
            , ( Translate.Behind, ecdBehind )
            ]

        immunizationChartData =
            [ ( Translate.OnTrack, immunizationOnTrack )
            , ( Translate.Behind, immunizationBehind )
            ]

        toColorFunc item =
            case item of
                Translate.OnTrack ->
                    Color.rgb (27 / 255) (207 / 255) (193 / 255)

                _ ->
                    Color.rgb (240 / 255) (111 / 255) (107 / 255)

        colors =
            [ ( Translate.OnTrack, Color.rgb (27 / 255) (207 / 255) (193 / 255) )
            , ( Translate.Behind, Color.rgb (240 / 255) (111 / 255) (107 / 255) )
            ]
                |> Dict.fromList
    in
    [ div [ class "ui grid" ]
        [ div [ class "three column row center" ]
            [ chwCard language (Translate.Dashboard Translate.NumberOfChildrenSeen) (String.fromInt numberOfChildrenSeen) ]
        ]
    , div [ class "ui blue segment donut-chart left" ]
        [ viewDonutChart language
            (Translate.Dashboard Translate.ECDOnTrackLabel)
            identity
            toColorFunc
            colors
            ecdChartData
        ]
    , div [ class "ui blue segment donut-chart left" ]
        [ viewDonutChart language
            (Translate.Dashboard Translate.ImmunizationOnTrackLabel)
            identity
            toColorFunc
            colors
            immunizationChartData
        ]
    ]


viewChildWellnessNutritionPage : Language -> NominalDate -> AssembledData -> List (Html Msg)
viewChildWellnessNutritionPage language dateLastDayOfSelectedMonth assembled =
    let
        dataItems =
            mergeDicts spvDict nutritionIndividualDict
                |> mergeDicts nutritionGroupDict
                |> Dict.toList
                |> List.map (\( identifier, encounters ) -> NutritionDataItem identifier encounters)

        mergeDicts d1 d2 =
            Dict.merge
                (\key value -> Dict.insert key value)
                -- In case we got both values for months, we give preference to
                -- the one with more recent date.
                (\key value1 value2 ->
                    value1
                        ++ value2
                        |> -- Sort DESC by date, so it will be easier to resolve l
                           -- ast occurance of encounter values.
                           List.sortWith (sortByDateDesc .startDate)
                        |> Dict.insert key
                )
                (\key value -> Dict.insert key value)
                d1
                d2
                Dict.empty

        spvDict =
            List.filterMap
                (\item ->
                    let
                        encounters =
                            generateEncounters (.encounterType >> (==) PediatricCare) .warnings item.encounters
                    in
                    if List.isEmpty encounters then
                        Nothing

                    else
                        Just <| NutritionDataItem item.identifier encounters
                )
                assembled.spvData
                |> itemsToDict

        nutritionIndividualDict =
            List.filterMap
                (\item ->
                    let
                        encounters =
                            generateEncounters (.encounterType >> (==) NutritionEncounterNurse) (always EverySet.empty) item.encounters
                    in
                    if List.isEmpty encounters then
                        Nothing

                    else
                        Just <| NutritionDataItem item.identifier encounters
                )
                assembled.nutritionIndividualData
                |> itemsToDict

        nutritionGroupDict =
            List.filterMap
                (\item ->
                    let
                        encounters =
                            generateEncounters (always True) (always EverySet.empty) item.encounters
                    in
                    if List.isEmpty encounters then
                        Nothing

                    else
                        Just <| NutritionDataItem item.identifier encounters
                )
                assembled.nutritionGroupData
                |> itemsToDict

        generateEncounters isNurseEncounterFunc resolveWarningsFunc =
            List.filterMap
                (\encounter ->
                    if isNurseEncounterFunc encounter then
                        Just <|
                            NutritionEncounterDataItem encounter.startDate
                                (resolveWarningsFunc encounter)
                                encounter.zscoreStunting
                                encounter.zscoreUnderweight
                                encounter.zscoreWasting
                                encounter.muac
                                encounter.nutritionSigns

                    else
                        Nothing
                )

        itemsToDict =
            List.map
                (\item ->
                    ( item.identifier, item.encounters )
                )
                >> Dict.fromList

        encountersForSelectedMonth =
            getEncountersForSelectedMonth dateLastDayOfSelectedMonth dataItems

        -- Percent of good nutrition encounters from total encounters
        -- performed during selected month.
        percentOfGoodNutrition =
            let
                goodNutritionEncounters =
                    List.filter isGoodNutritionEncounter encountersForSelectedMonth
                        |> List.length
            in
            round (100 * toFloat goodNutritionEncounters / toFloat totalEncountersCompleted)

        -- Total Nutrition encounters performed during selected month.
        totalEncountersCompleted =
            List.length encountersForSelectedMonth

        -- Number of children who had moderate or acute wasting diagnosed
        -- at encounter during selected month or previously, and did not have
        -- an encounter afterwards that indicated that condition was resolved.
        totalBeneficiariesWasting =
            countCurrentlyDiagnosedByValue .zscoreWasting (\zscore -> zscore < 2)

        -- Number of children who are had firs diagnosis of wasting during
        -- selected month.
        incidentsOfWasting =
            List.filter
                (\item ->
                    let
                        wastingDates =
                            List.filterMap
                                (\encounter ->
                                    Maybe.andThen
                                        (\zscore ->
                                            if zscore < -2 then
                                                Just encounter.startDate

                                            else
                                                Nothing
                                        )
                                        encounter.zscoreWasting
                                )
                                item.encounters
                    in
                    (not <| List.isEmpty wastingDates) && List.all (withinSelectedMonth dateLastDayOfSelectedMonth) wastingDates
                )
                itemsWithinOrBeforeSelectedMonth
                |> List.length

        -- Number of children who had moderate or acute stunting diagnosed at
        -- encounter during selected month or previously, and did not have an
        -- encounter afterwards that indicated that condition was resolved.
        numberOfStunting =
            countCurrentlyDiagnosedByValue .zscoreStunting (\zscore -> zscore < 2)

        -- Number of Children who had either micro or macrocephaly diagnosed at
        -- encounter during selected month or previously, and did not have an
        -- encounter afterwards that indicated that condition was resolved.
        numberOfCephaly =
            countCurrentlyDiagnosedByValue
                (\encounter ->
                    if
                        EverySet.isEmpty encounter.warnings
                            || EverySet.member NoHeadCircumferenceWarning encounter.warnings
                    then
                        Nothing

                    else
                        Just encounter.warnings
                )
                (\warnings ->
                    List.any
                        (\warning ->
                            EverySet.member warning warnings
                        )
                        [ WarningHeadCircumferenceMicrocephaly, WarningHeadCircumferenceMacrocephaly ]
                )

        -- Number children who had malnutrition diagnosed at encounter during
        -- selected month or previously, and did not have an encounter
        -- afterwards that indicated that condition was resolved.
        numberOfDiagnosedMalnorished =
            countCurrentlyDiagnosedByValue Just (isGoodNutritionEncounter >> not)

        itemsWithinOrBeforeSelectedMonth =
            List.map
                (\item ->
                    let
                        encounters =
                            List.filter (.startDate >> withinOrBeforeSelectedMonth dateLastDayOfSelectedMonth) item.encounters
                    in
                    { item | encounters = encounters }
                )
                dataItems

        isGoodNutritionEncounter encounter =
            (case EverySet.toList encounter.nutritionSigns of
                [] ->
                    True

                [ NormalChildNutrition ] ->
                    True

                _ ->
                    False
            )
                && (Maybe.map (\muac -> muac > 12.5) encounter.muac
                        |> Maybe.withDefault True
                   )
                && (Maybe.Extra.values [ encounter.zscoreStunting, encounter.zscoreUnderweight, encounter.zscoreWasting ]
                        |> List.all (\zscore -> zscore >= -2)
                   )

        countCurrentlyDiagnosedByValue valueMappingFunc valueConditionFunc =
            List.filter
                (\item ->
                    let
                        valuesWithDate =
                            List.filterMap
                                (\encounter ->
                                    valueMappingFunc encounter
                                        |> Maybe.map (\value -> ( encounter.startDate, value ))
                                )
                                item.encounters

                        lastValueDate =
                            List.head valuesWithDate
                                |> Maybe.map Tuple.first

                        lastDiagnosisDate =
                            List.filter (Tuple.second >> valueConditionFunc) valuesWithDate
                                |> List.head
                                |> Maybe.map Tuple.first
                    in
                    isJust lastDiagnosisDate && lastValueDate == lastDiagnosisDate
                )
                itemsWithinOrBeforeSelectedMonth
                |> List.length
    in
    [ div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.GoodNutritionLabel) (String.fromInt percentOfGoodNutrition)
            , chwCard language (Translate.Dashboard Translate.TotalEncountersLabel) (String.fromInt totalEncountersCompleted)
            , chwCard language (Translate.Dashboard Translate.TotalBeneficiariesWasting) (String.fromInt totalBeneficiariesWasting)
            ]
        , div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.IncidentsOfWasting) (String.fromInt incidentsOfWasting)
            , chwCard language (Translate.Dashboard Translate.NumberOfStunting) (String.fromInt numberOfStunting)
            , chwCard language (Translate.Dashboard Translate.NumberOfCephaly) (String.fromInt numberOfCephaly)
            ]
        , div [ class "three column row center" ]
            [ chwCard language (Translate.Dashboard Translate.NumberOfDiagnosedMalnourished) (String.fromInt numberOfDiagnosedMalnorished) ]
        ]
    ]
