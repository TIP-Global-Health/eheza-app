module Pages.Dashboard.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, DashboardStats, GoodNutrition, Nutrition, NutritionValue, ParticipantStats, Periods, TotalBeneficiaries)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Unit(..), isBetween, numberToMonth)
import Debug exposing (toString)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, isDiffTruthy)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import List.Extra
import Maybe exposing (Maybe)
import Pages.Dashboard.GraphUtils exposing (..)
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Pages.Utils exposing (calculatePercentage, monthList)
import Path
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, r)
import Time exposing (Month(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes as Explicit exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..))
import Utils.Html exposing (viewModal)


{-| Shows a dashboard page.
-}
view : Language -> DashboardPage -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language page currentDate healthCenterId model db =
    let
        stats =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.withDefault Backend.Dashboard.Model.emptyModel
                -- Filter by period.
                |> filterStatsByPeriod currentDate model

        ( pageView, goBackPage ) =
            case page of
                MainPage ->
                    if Dict.isEmpty stats.totalBeneficiaries then
                        -- We can use the general "viewLoading" but we need to add a messgae here as well.
                        ( div [ class "ui segment blue center aligned" ]
                            [ div []
                                [ translateText language <| Translate.Dashboard Translate.SyncNotice ]
                            ]
                        , PinCodePage
                        )

                    else
                        ( viewMainPage language currentDate stats model, PinCodePage )

                StatsPage ->
                    if List.isEmpty stats.missedSessions && List.isEmpty stats.completedProgram then
                        ( div [ class "ui segment" ] [ translateText language <| Translate.Dashboard Translate.NoDataGeneral ], PinCodePage )

                    else
                        ( viewStatsPage language currentDate stats model healthCenterId db, UserPage <| DashboardPage MainPage )

                CaseManagementPage ->
                    if List.isEmpty stats.caseManagement then
                        ( div [ class "ui segment" ] [ translateText language <| Translate.Dashboard Translate.NoDataGeneral ], PinCodePage )

                    else
                        ( viewCaseManagementPage language currentDate stats model, UserPage <| DashboardPage model.latestPage )

        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.DashboardLabel ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage goBackPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    div
        [ class "wrap" ]
        [ header
        , pageView
        ]


viewMainPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewMainPage language currentDate stats model =
    div [ class "dashboard main" ]
        [ viewPeriodFilter language model filterPeriods
        , div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                [ viewGoodNutrition language stats.goodNutrition
                ]
            , div [ class "eight wide column" ]
                [ viewTotalEncounters language stats.totalEncounters
                ]
            , div [ class "sixteen wide column" ]
                [ viewMonthlyChart language (Translate.Dashboard Translate.TotalBeneficiaries) FilterBeneficiariesChart stats.totalBeneficiaries model.currentBeneficiariesChartsFilter
                ]
            , div [ class "sixteen wide column" ]
                [ viewMonthlyChart language (Translate.Dashboard Translate.IncidenceOf) FilterBeneficiariesIncidenceChart stats.totalBeneficiariesIncidence model.currentBeneficiariesIncidenceChartsFilter
                ]
            , div [ class "sixteen wide column" ]
                [ viewDashboardPagesLinks language
                ]
            ]
        ]


viewStatsPage : Language -> NominalDate -> DashboardStats -> Model -> HealthCenterId -> ModelIndexedDb -> Html Msg
viewStatsPage language currentDate stats model healthCenterId db =
    div [ class "dashboard stats" ]
        [ viewPeriodFilter language model filterPeriodsForStatsPage
        , viewAllStatsCards language stats currentDate model healthCenterId db
        , viewBeneficiariesTable language currentDate stats model
        , viewFamilyPlanning language stats
        , viewStatsTableModal language model
        ]


viewCaseManagementPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewCaseManagementPage language currentDate stats model =
    let
        tableData =
            List.foldl
                (\person accum ->
                    case model.currentCaseManagementFilter of
                        Stunting ->
                            { name = person.name, nutrition = person.nutrition.stunting } :: accum

                        Underweight ->
                            { name = person.name, nutrition = person.nutrition.underweight } :: accum

                        Wasting ->
                            { name = person.name, nutrition = person.nutrition.wasting } :: accum

                        MUAC ->
                            { name = person.name, nutrition = person.nutrition.muac } :: accum
                )
                []
                stats.caseManagement
                |> List.sortBy .name
    in
    div [ class "dashboard case" ]
        [ viewPeriodFilter language model filterPeriods
        , div [ class "ui segment blue" ]
            [ div [ class "case-management" ]
                [ div [ class "header" ]
                    [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.CaseManagementLabel ]
                    , div [ class "filters" ]
                        (List.map (viewFilters FilterCaseManagement model.currentCaseManagementFilter) filterCharts)
                    ]
                , div [ class "content" ]
                    [ viewCaseManagementTable language model tableData
                    ]
                ]
            ]
        ]


viewCaseManagementTable : Language -> Model -> List { name : String, nutrition : Dict Int NutritionValue } -> Html Msg
viewCaseManagementTable language model tableData =
    table [ class "ui very basic collapsing celled table" ]
        [ thead []
            [ tr []
                (th [ class "name" ] [ translateText language <| Translate.Name ]
                    :: List.map (\month -> th [] [ span [] [ translateText language <| Translate.ResolveMonth month True ] ]) monthList
                )
            ]
        , tbody []
            (List.map viewCaseManagementTableRow tableData)
        ]


viewCaseManagementTableRow : { name : String, nutrition : Dict Int NutritionValue } -> Html Msg
viewCaseManagementTableRow rowData =
    let
        rowList =
            rowData.nutrition
                |> Dict.toList
                |> List.sortBy Tuple.first
    in
    tr []
        (td [ class "name" ] [ text rowData.name ]
            :: List.map viewMonthCell rowList
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


viewPeriodFilter : Language -> Model -> List FilterPeriod -> Html Msg
viewPeriodFilter language model filterPeriodsPerPage =
    let
        renderButton period =
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
    div [ class "ui segment filters" ]
        (List.map renderButton filterPeriodsPerPage)


viewGoodNutrition : Language -> GoodNutrition -> Html Msg
viewGoodNutrition language nutrition =
    let
        percentageThisYear =
            calculatePercentage nutrition.all.thisYear nutrition.good.thisYear
                |> round

        percentageLastYear =
            calculatePercentage nutrition.all.lastYear nutrition.good.lastYear
                |> round

        percentageDiff =
            percentageThisYear - percentageLastYear

        statsCard =
            { title = translate language <| Translate.Dashboard Translate.GoodNutritionLabel
            , cardClasses = "good-nutrition"
            , cardAction = Nothing
            , value = percentageThisYear
            , valueSeverity = Neutral
            , valueIsPercentage = True
            , previousPercentage = percentageDiff
            , previousPercentageLabel = OneYear
            , newCases = Nothing
            }
    in
    viewCard language statsCard


viewTotalEncounters : Language -> Periods -> Html Msg
viewTotalEncounters language encounters =
    let
        diff =
            encounters.thisYear - encounters.lastYear

        percentageDiff =
            calculatePercentage encounters.thisYear diff
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


viewAllStatsCards : Language -> DashboardStats -> NominalDate -> Model -> HealthCenterId -> ModelIndexedDb -> Html Msg
viewAllStatsCards language stats currentDate model healthCenterId db =
    let
        modelWithLastMonth =
            if model.period == ThisMonth then
                { model | period = LastMonth }

            else
                { model | period = ThreeMonths }

        monthBeforeStats =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.withDefault Backend.Dashboard.Model.emptyModel
                -- Filter by period.
                |> filterStatsByPeriod currentDate modelWithLastMonth
    in
    if List.isEmpty stats.malnourished then
        div [ class "ui segment" ] [ translateText language <| Translate.Dashboard Translate.NoDataForPeriod ]

    else
        div [ class "ui equal width grid" ]
            [ viewMalnourishedCards language stats monthBeforeStats
            , viewMiscCards language stats monthBeforeStats
            ]


viewMalnourishedCards : Language -> DashboardStats -> DashboardStats -> Html Msg
viewMalnourishedCards language stats monthBeforeStats =
    let
        total =
            stats.malnourished
                |> List.length

        totalBefore =
            monthBeforeStats.malnourished
                |> List.length

        totalDiff =
            total - totalBefore

        totalPercentage =
            calculatePercentage total totalDiff
                |> round

        totalCard =
            { title = translate language <| Translate.Dashboard Translate.TotalMalnourished
            , cardClasses = "stats-card total-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
            , value = total
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = totalPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just totalDiff
            }

        severe =
            stats.malnourished
                |> List.filter (\row -> row.zscore <= -2)
                |> List.length

        severeBefore =
            monthBeforeStats.malnourished
                |> List.filter (\row -> row.zscore <= -2)
                |> List.length

        severeDiff =
            severe - severeBefore

        severePercentage =
            calculatePercentage severe severeDiff
                |> round

        severeCard =
            { title = translate language <| Translate.Dashboard Translate.SeverelyMalnourished
            , cardClasses = "stats-card severely-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
            , value = severe
            , valueSeverity = Severe
            , valueIsPercentage = False
            , previousPercentage = severePercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just severeDiff
            }

        moderate =
            stats.malnourished
                |> List.filter (\row -> row.zscore > -2)
                |> List.length

        moderateBefore =
            monthBeforeStats.malnourished
                |> List.filter (\row -> row.zscore > -2)
                |> List.length

        moderateDiff =
            moderate - moderateBefore

        moderatePercentage =
            calculatePercentage moderate moderateDiff
                |> round

        moderateCard =
            { title = translate language <| Translate.Dashboard Translate.ModeratelyMalnourished
            , cardClasses = "stats-card moderately-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
            , value = moderate
            , valueSeverity = Moderate
            , valueIsPercentage = False
            , previousPercentage = moderatePercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just moderateDiff
            }
    in
    div [ class "row" ]
        [ div [ class "column" ] [ viewCard language totalCard ]
        , div [ class "column" ] [ viewCard language severeCard ]
        , div [ class "column" ] [ viewCard language moderateCard ]
        ]


viewMiscCards : Language -> DashboardStats -> DashboardStats -> Html Msg
viewMiscCards language stats monthBeforeStats =
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
                    { name = childrenBeneficiaries.name
                    , gender = childrenBeneficiaries.gender
                    , birthDate = childrenBeneficiaries.birthDate
                    , motherName = childrenBeneficiaries.motherName
                    , phoneNumber = childrenBeneficiaries.phoneNumber
                    , dates = []
                    }
                        :: accum
                )
                []
                stats.childrenBeneficiaries

        totalNewBeneficiariesDiff =
            totalNewBeneficiaries - totalNewBeneficiariesBefore

        totalNewBeneficiariesPercentage =
            calculatePercentage totalNewBeneficiaries totalNewBeneficiariesDiff
                |> round

        totalNewBeneficiariesTitle =
            translate language <| Translate.Dashboard Translate.NewBeneficiaries

        totalNewBeneficiariesCard =
            { title = totalNewBeneficiariesTitle
            , cardClasses = "stats-card new-beneficiaries"
            , cardAction = Just (ModalToggle True totalNewBeneficiariesTable totalNewBeneficiariesTitle)
            , value = totalNewBeneficiaries
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = totalNewBeneficiariesPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Nothing
            }

        completedProgram =
            stats.completedProgramCount

        completedProgramBefore =
            monthBeforeStats.completedProgramCount

        completedProgramDiff =
            completedProgram - completedProgramBefore

        completedProgramPercentage =
            calculatePercentage completedProgram completedProgramDiff
                |> round

        completedProgramTitle =
            translate language <| Translate.Dashboard Translate.CompletedProgramLabel

        completedProgramCard =
            { title = completedProgramTitle
            , cardClasses = "stats-card completed-program"
            , cardAction = Just (ModalToggle True stats.completedProgram completedProgramTitle)
            , value = completedProgram
            , valueSeverity = Good
            , valueIsPercentage = False
            , previousPercentage = completedProgramPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Nothing
            }

        missedSessions =
            stats.missedSessionsCount

        missedSessionsBefore =
            monthBeforeStats.missedSessionsCount

        missedSessionsDiff =
            missedSessions - missedSessionsBefore

        missedSessionsPercentage =
            calculatePercentage missedSessions missedSessionsDiff
                |> round

        missedSessionsTitle =
            translate language <| Translate.Dashboard Translate.MissedSessionsLabel

        missedSessionsCard =
            { title = missedSessionsTitle
            , cardClasses = "stats-card missed-sessions"
            , cardAction = Just (ModalToggle True stats.missedSessions missedSessionsTitle)
            , value = missedSessions
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
            (class <| "ui segment blue dashboard-cards " ++ statsCard.cardClasses ++ " " ++ cardLinkClass) :: cardAction

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
                    , ( "dashboard-filters", True )
                    ]
                , onClick <| SetFilterGender gender
                ]
                [ genderTranslated
                ]
    in
    div [ class "filters" ]
        (List.map renderButton filterGenders)


viewBeneficiariesTable : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewBeneficiariesTable language currentDate stats model =
    let
        statsFilteredByGender =
            stats
                |> filterStatsByGender currentDate model

        filterStatsByAgeDo func =
            filterStatsByAge
                currentDate
                func
                statsFilteredByGender

        stats0_5 =
            filterStatsByAgeDo (\{ months } -> months >= 0 && months < 6)

        stats6_8 =
            filterStatsByAgeDo (\{ months } -> months >= 6 && months <= 8)

        stats9_11 =
            filterStatsByAgeDo (\{ months } -> months >= 9 && months <= 11)

        stats12_23 =
            filterStatsByAgeDo (\{ months } -> months >= 12 && months <= 23)

        getNewBeneficiariesCount stats_ =
            stats_.childrenBeneficiaries
                |> List.length
                |> String.fromInt

        getCompletedProgramBeneficiariesCount stats_ =
            stats_.completedProgram
                |> List.length
                |> String.fromInt

        getMissedSessionBeneficiariesCount stats_ =
            stats_.missedSessions
                |> List.length
                |> String.fromInt

        getTotalMalnourishedCount stats_ =
            stats_.malnourished
                |> List.length
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
                        , th [] [ text "12-23" ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel "new" ]
                        , td [] [ text <| getNewBeneficiariesCount stats0_5 ]
                        , td [] [ text <| getNewBeneficiariesCount stats6_8 ]
                        , td [] [ text <| getNewBeneficiariesCount stats9_11 ]
                        , td [] [ text <| getNewBeneficiariesCount stats12_23 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel "completed" ]
                        , td [] [ text <| getCompletedProgramBeneficiariesCount stats0_5 ]
                        , td [] [ text <| getCompletedProgramBeneficiariesCount stats6_8 ]
                        , td [] [ text <| getCompletedProgramBeneficiariesCount stats9_11 ]
                        , td [] [ text <| getCompletedProgramBeneficiariesCount stats12_23 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel "missed" ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount stats0_5 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount stats6_8 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount stats9_11 ]
                        , td [] [ text <| getMissedSessionBeneficiariesCount stats12_23 ]
                        ]
                    , tr []
                        [ td [ class "label" ] [ translateText language <| Translate.Dashboard <| Translate.BeneficiariesTableColumnLabel "malnourished" ]
                        , td [] [ text <| getTotalMalnourishedCount stats0_5 ]
                        , td [] [ text <| getTotalMalnourishedCount stats6_8 ]
                        , td [] [ text <| getTotalMalnourishedCount stats9_11 ]
                        , td [] [ text <| getTotalMalnourishedCount stats12_23 ]
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
            , DashboardPage StatsPage
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
            , DashboardPage CaseManagementPage
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
        [ class "ui blue segment family-planning" ]
        [ div [ class "header" ]
            [ h3 [ class "title" ] [ translateText language <| Translate.Dashboard Translate.FamilyPlanningLabel ]
            ]
        , div [ class "ui center aligned grid" ]
            [ div [ class "middle aligned row" ]
                [ viewDonutChart language stats ]
            ]
        ]


viewDonutChart : Language -> DashboardStats -> Html Msg
viewDonutChart language stats =
    let
        dict =
            getFamilyPlanningSignsCounter stats
    in
    if Dict.isEmpty dict then
        div [] [ text "No family plannings for the selected period." ]

    else
        let
            totalCount =
                dict
                    |> Dict.values
                    |> List.foldl (\val accum -> val + accum) 0

            totalNoFamilyPlanning =
                Dict.get NoFamilyPlanning dict
                    |> Maybe.withDefault 0

            useFamilyPlanning =
                totalCount - totalNoFamilyPlanning

            totalPercent =
                useFamilyPlanning * 100 // totalCount
        in
        div [ class "content" ]
            [ viewChart dict
            , div [ class "in-chart" ]
                [ div [ class "stats" ]
                    [ span [ class "percentage neutral" ] [ text <| String.fromInt totalPercent ++ "%" ]
                    , text " "
                    , span [ class "use-label" ] [ translateText language <| Translate.Dashboard Translate.UseFamilyPlanning ]
                    , div [ class "count" ]
                        [ translateText language <|
                            Translate.Dashboard <|
                                Translate.FamilyPlanningOutOfWomen
                                    { total = totalCount
                                    , useFamilyPlanning = useFamilyPlanning
                                    }
                        ]
                    ]
                ]
            , viewFamilyPlanningChartLegend language dict
            ]


viewMonthlyChart : Language -> TranslationId -> FilterType -> Dict Int TotalBeneficiaries -> FilterCharts -> Html Msg
viewMonthlyChart language title filterType data currentFilter =
    let
        chartList =
            data
                |> Dict.toList
                |> List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2))
                |> Dict.fromList

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
                )
                Dict.empty
                chartList
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
            List.map (\( key, value ) -> choose value 0) chartData

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
            [ h3 [ class "title left floated column" ] [ text <| translate language title ++ " " ++ toString currentFilter ]
            , div [ class "filters" ]
                (List.map (viewFilters filterType currentFilter) filterCharts)
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


viewFilters : FilterType -> FilterCharts -> FilterCharts -> Html Msg
viewFilters filterType currentChartFilter filter =
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
            [ ( "dashboard-filters", True )
            , ( "active", filter == currentChartFilter )
            ]
        , onClick <| filterAction
        ]
        [ text <| toString filter ]


viewFamilyPlanningChartLegend : Language -> FamilyPlanningSignsCounter -> Html Msg
viewFamilyPlanningChartLegend language dict =
    let
        listSorted =
            dict
                |> Dict.toList
                |> List.sortBy (\( name, val ) -> Debug.toString name)
    in
    div [ class "legend" ]
        (List.map
            (\( sign, _ ) ->
                div [ class "legend-item" ]
                    [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                        [ Svg.circle [ cx "50", cy "50", r "40", fill <| Fill <| familyPlanningSignToColor sign ] []
                        ]
                    , span [] [ translateText language <| Translate.FamilyPlanningSignLabel sign ]
                    ]
            )
            listSorted
        )


viewChart : FamilyPlanningSignsCounter -> Svg msg
viewChart dict =
    let
        arcs =
            dict
                |> Dict.values
                |> List.map toFloat

        signsList =
            dict
                |> Dict.keys

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
        [ annular signsList pieData ]


viewStatsTableModal : Language -> Model -> Html Msg
viewStatsTableModal language model =
    let
        participantsStatsDialog =
            if model.modalState then
                Just <|
                    div [ class "ui tiny active modal segment blue" ]
                        [ div
                            [ class "header" ]
                            [ div [ class "title left floated column" ] [ text model.modalTitle ]
                            , span
                                [ class "overlay-close right floated column"
                                , onClick <| ModalToggle False [] ""
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
                                    (List.map viewModalTableRow model.modalTable)
                                ]
                            ]
                        ]

            else
                Nothing
    in
    viewModal participantsStatsDialog


viewModalTableRow : ParticipantStats -> Html Msg
viewModalTableRow rowData =
    tr []
        [ td [ class "name" ] [ text rowData.name ]
        , td [ class "mother-name" ] [ text rowData.motherName ]
        , td [ class "phone-number" ] [ text <| Maybe.withDefault "-" rowData.phoneNumber ]
        ]


filterStatsByAge : NominalDate -> ({ months : Int, days : Int } -> Bool) -> DashboardStats -> DashboardStats
filterStatsByAge currentDate func stats =
    let
        childrenBeneficiaries =
            stats.childrenBeneficiaries
                |> List.filter (\row -> isDiffTruthy row.birthDate currentDate func)

        completedProgram =
            stats.completedProgram
                |> List.filter (\row -> isDiffTruthy row.birthDate currentDate func)

        missedSessions =
            stats.missedSessions
                |> List.filter (\row -> isDiffTruthy row.birthDate currentDate func)

        malnourished =
            stats.malnourished
                |> List.filter (\row -> isDiffTruthy row.birthDate currentDate func)
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiaries
        , completedProgram = completedProgram
        , missedSessions = missedSessions
        , malnourished = malnourished
    }


{-| Filter stats to match the selected period.
-}
filterStatsByPeriod : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByPeriod currentDate model stats =
    let
        ( startDate, endDate ) =
            case model.period of
                OneYear ->
                    ( Date.add Years -1 currentDate, currentDate )

                ThisMonth ->
                    ( Date.add Months -1 currentDate, currentDate )

                LastMonth ->
                    ( Date.add Months -2 currentDate, Date.add Months -1 currentDate )

                ThreeMonths ->
                    ( Date.add Months -3 currentDate, Date.add Months -2 currentDate )

        filterPartial =
            isBetween startDate endDate

        childrenBeneficiariesUpdated =
            stats.childrenBeneficiaries
                |> List.filter (\child -> filterPartial child.memberSince)

        familyPlanningUpdated =
            stats.familyPlanning
                |> List.filter (\familyPlanning -> filterPartial familyPlanning.created)

        malnourishedUpdated =
            stats.malnourished
                |> List.filter (\malnourished -> filterPartial malnourished.created)

        completedProgramCount =
            List.foldl
                (\completedProgram accum ->
                    (List.filter filterPartial completedProgram.dates
                        |> List.length
                    )
                        + accum
                )
                0
                stats.completedProgram

        missedSessionsCount =
            List.foldl
                (\missedSessions accum ->
                    (List.filter filterPartial missedSessions.dates
                        |> List.length
                    )
                        + accum
                )
                0
                stats.missedSessions
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiariesUpdated
        , familyPlanning = familyPlanningUpdated
        , malnourished = malnourishedUpdated
        , completedProgramCount = completedProgramCount
        , missedSessionsCount = missedSessionsCount
    }


{-| Filter stats to match the selected gender.
-}
filterStatsByGender : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByGender currentDate model stats =
    let
        -- Filter by gender
        filterDo data =
            data
                |> List.filter
                    (\personStats ->
                        case ( personStats.gender, model.beneficiariesGender ) of
                            ( Backend.Person.Model.Male, Pages.Dashboard.Model.Boys ) ->
                                True

                            ( Backend.Person.Model.Female, Pages.Dashboard.Model.Girls ) ->
                                True

                            _ ->
                                False
                    )
    in
    { stats
        | childrenBeneficiaries = filterDo stats.childrenBeneficiaries
        , completedProgram = filterDo stats.completedProgram
        , missedSessions = filterDo stats.missedSessions
        , malnourished = filterDo stats.malnourished
    }


getFamilyPlanningSignsCounter : DashboardStats -> FamilyPlanningSignsCounter
getFamilyPlanningSignsCounter stats =
    if List.isEmpty stats.familyPlanning then
        Dict.empty

    else
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


annular : List FamilyPlanningSign -> List Arc -> Svg msg
annular signsList arcs =
    let
        getColor index =
            List.Extra.getAt index signsList
                |> Maybe.withDefault NoFamilyPlanning
                |> (\sign -> Dict.get sign colors)
                |> Maybe.withDefault Color.black

        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius - 60 })
                [ fill <| Fill <| getColor index ]
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]
