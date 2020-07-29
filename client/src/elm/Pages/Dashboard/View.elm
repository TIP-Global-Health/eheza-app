module Pages.Dashboard.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model
    exposing
        ( CaseManagement
        , CaseNutrition
        , CaseNutritionTotal
        , DashboardStats
        , GoodNutrition
        , Nutrition
        , NutritionValue
        , ParticipantStats
        , Periods
        , TotalBeneficiaries
        , emptyNutrition
        , emptyTotalBeneficiaries
        )
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Month, Unit(..), isBetween, numberToMonth)
import Debug exposing (toString)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, allMonths, formatYYYYMMDD, isDiffTruthy, yearYYNumber)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import List.Extra
import Maybe exposing (Maybe)
import Pages.Dashboard.GraphUtils exposing (..)
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Pages.Utils exposing (calculatePercentage)
import Path
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, r)
import Translate exposing (Language, TranslationId, translate, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes as Explicit exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..))
import Utils.Html exposing (spinner, viewModal)


{-| Shows a dashboard page.
-}
view : Language -> DashboardPage -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language page currentDate healthCenterId model db =
    let
        ( content, goBackPage ) =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.map
                    (\stats ->
                        case page of
                            MainPage ->
                                ( viewMainPage language currentDate stats model, PinCodePage )

                            StatsPage ->
                                ( viewStatsPage language currentDate stats model healthCenterId db, UserPage <| DashboardPage MainPage )

                            CaseManagementPage ->
                                ( viewCaseManagementPage language currentDate stats model, UserPage <| DashboardPage model.latestPage )
                    )
                |> Maybe.withDefault ( spinner, PinCodePage )

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
        , content
        ]


viewMainPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewMainPage language currentDate stats model =
    let
        currentPeriodStats =
            filterStatsWithinPeriod currentDate model stats

        totalBeneficiariesMonthlyDuringPastYear =
            generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats

        emptyTotalBeneficiariesDict =
            List.repeat 12 emptyTotalBeneficiaries
                |> List.indexedMap (\index empty -> ( index + 1, empty ))
                |> Dict.fromList

        totalsGraphData =
            stats.caseManagement
                |> List.map (.nutrition >> generateCaseNutritionTotals)
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear

        newCasesGraphData =
            stats.caseManagement
                |> List.map (.nutrition >> generateCaseNutritionNewCases currentDate)
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear
    in
    div [ class "dashboard main" ]
        [ viewPeriodFilter language model filterPeriodsForMainPage
        , div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                [ viewGoodNutrition language currentPeriodStats.maybeGoodNutrition
                ]
            , div [ class "eight wide column" ]
                [ viewTotalEncounters language currentPeriodStats.totalEncounters
                ]
            , div [ class "sixteen wide column" ]
                [ viewMonthlyChart language currentDate MonthlyChartTotals FilterBeneficiariesChart totalsGraphData model.currentBeneficiariesChartsFilter
                ]
            , div [ class "sixteen wide column" ]
                [ viewMonthlyChart language currentDate MonthlyChartIncidence FilterBeneficiariesIncidenceChart newCasesGraphData model.currentBeneficiariesIncidenceChartsFilter
                ]
            , div [ class "sixteen wide column" ]
                [ viewDashboardPagesLinks language
                ]
            ]
        ]


applyTotalBeneficiariesDenomination : Dict Int Int -> Dict Int TotalBeneficiaries -> Dict Int TotalBeneficiaries
applyTotalBeneficiariesDenomination beneficiariesPerMonthsDict totalBeneficiariesDict =
    let
        applyDenomination number denominator =
            ceiling (100 * toFloat number / toFloat denominator)
    in
    totalBeneficiariesDict
        |> Dict.map
            (\month totalBeneficiaries ->
                Dict.get month beneficiariesPerMonthsDict
                    |> Maybe.map
                        (\total ->
                            { stunting =
                                Nutrition (applyDenomination totalBeneficiaries.stunting.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.stunting.moderateNutrition total)
                            , underweight =
                                Nutrition (applyDenomination totalBeneficiaries.underweight.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.underweight.moderateNutrition total)
                            , wasting =
                                Nutrition (applyDenomination totalBeneficiaries.wasting.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.wasting.moderateNutrition total)
                            , muac =
                                Nutrition (applyDenomination totalBeneficiaries.muac.severeNutrition total)
                                    (applyDenomination totalBeneficiaries.muac.moderateNutrition total)
                            }
                        )
                    |> Maybe.withDefault totalBeneficiaries
            )


generateTotalBeneficiariesMonthlyDuringPastYear : NominalDate -> DashboardStats -> Dict Int Int
generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats =
    let
        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        ( thisYear, lastYear ) =
            List.repeat 12 0
                |> List.indexedMap (\index _ -> index + 1)
                |> List.Extra.splitAt currentMonth

        orderedList =
            (lastYear ++ thisYear)
                |> List.reverse
                |> List.indexedMap
                    (\index month ->
                        let
                            maxJoinDate =
                                Date.add Months (-1 * index) currentDate
                                    |> Date.ceiling Date.Month
                                    |> Date.add Days -1

                            minGraduationDate =
                                Date.add Months (-1 * index) currentDate
                                    |> Date.floor Date.Month

                            totalBeneficiaries =
                                stats.childrenBeneficiaries
                                    |> List.filter
                                        (\child ->
                                            (Date.compare child.memberSince maxJoinDate == LT)
                                                && (Date.compare minGraduationDate child.graduationDate == LT)
                                        )
                                    |> List.length
                        in
                        ( month, totalBeneficiaries )
                    )
                |> Dict.fromList
    in
    orderedList


accumCaseNutritionTotals : CaseNutritionTotal -> Dict Int TotalBeneficiaries -> Dict Int TotalBeneficiaries
accumCaseNutritionTotals totals dict =
    Dict.toList dict
        |> List.map
            (\( key, accum ) ->
                let
                    stunting =
                        Dict.get key totals.stunting
                            |> Maybe.map
                                (\totalsStunting ->
                                    Nutrition (totalsStunting.severeNutrition + accum.stunting.severeNutrition) (totalsStunting.moderateNutrition + accum.stunting.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.stunting

                    underweight =
                        Dict.get key totals.underweight
                            |> Maybe.map
                                (\totalsUnderweight ->
                                    Nutrition (totalsUnderweight.severeNutrition + accum.underweight.severeNutrition) (totalsUnderweight.moderateNutrition + accum.underweight.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.underweight

                    wasting =
                        Dict.get key totals.wasting
                            |> Maybe.map
                                (\totalsWasting ->
                                    Nutrition (totalsWasting.severeNutrition + accum.wasting.severeNutrition) (totalsWasting.moderateNutrition + accum.wasting.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.wasting

                    muac =
                        Dict.get key totals.muac
                            |> Maybe.map
                                (\totalsMuac ->
                                    Nutrition (totalsMuac.severeNutrition + accum.muac.severeNutrition) (totalsMuac.moderateNutrition + accum.muac.moderateNutrition)
                                )
                            |> Maybe.withDefault accum.muac
                in
                ( key, TotalBeneficiaries stunting underweight wasting muac )
            )
        |> Dict.fromList


generateCaseNutritionTotals : CaseNutrition -> CaseNutritionTotal
generateCaseNutritionTotals caseNutrition =
    let
        generateTotals nutrition =
            Dict.toList nutrition
                |> List.filterMap
                    (\( month, nutritionValue ) ->
                        if month == 13 then
                            Nothing

                        else
                            case nutritionValue.class of
                                Backend.Dashboard.Model.Moderate ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 0 1 )

                                Backend.Dashboard.Model.Severe ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 1 0 )

                                _ ->
                                    Just ( month, Backend.Dashboard.Model.Nutrition 0 0 )
                    )
                |> Dict.fromList
    in
    { stunting = generateTotals caseNutrition.stunting
    , underweight = generateTotals caseNutrition.underweight
    , wasting = generateTotals caseNutrition.wasting
    , muac = generateTotals caseNutrition.muac
    }


generateCaseNutritionNewCases : NominalDate -> CaseNutrition -> CaseNutritionTotal
generateCaseNutritionNewCases currentDate caseNutrition =
    let
        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        generateTotals nutrition =
            let
                sorted =
                    Dict.toList nutrition
                        |> List.sortBy Tuple.first

                oneBeforeFirst =
                    List.reverse sorted
                        |> List.head

                ( thisYear, lastYear ) =
                    List.take 12 sorted
                        |> List.Extra.splitAt currentMonth

                yearData =
                    lastYear ++ thisYear

                yearDataShiftedLeft =
                    oneBeforeFirst
                        |> Maybe.map (\beforeFirst -> beforeFirst :: List.take 11 yearData)
                        |> Maybe.withDefault yearData
            in
            List.map2
                (\( month, nutritionValue ) ( _, previousNutritionValue ) ->
                    case nutritionValue.class of
                        Backend.Dashboard.Model.Moderate ->
                            if previousNutritionValue.class == Backend.Dashboard.Model.Moderate then
                                ( month, Backend.Dashboard.Model.Nutrition 0 1 )

                            else
                                ( month, Backend.Dashboard.Model.Nutrition 0 0 )

                        Backend.Dashboard.Model.Severe ->
                            if previousNutritionValue.class == Backend.Dashboard.Model.Severe then
                                ( month, Backend.Dashboard.Model.Nutrition 1 0 )

                            else
                                ( month, Backend.Dashboard.Model.Nutrition 0 0 )

                        _ ->
                            ( month, Backend.Dashboard.Model.Nutrition 0 0 )
                )
                yearData
                yearDataShiftedLeft
                |> Dict.fromList
    in
    { stunting = generateTotals caseNutrition.stunting
    , underweight = generateTotals caseNutrition.underweight
    , wasting = generateTotals caseNutrition.wasting
    , muac = generateTotals caseNutrition.muac
    }


viewStatsPage : Language -> NominalDate -> DashboardStats -> Model -> HealthCenterId -> ModelIndexedDb -> Html Msg
viewStatsPage language currentDate stats model healthCenterId db =
    let
        currentPeriodStats =
            filterStatsWithinPeriod currentDate model stats
    in
    div [ class "dashboard stats" ]
        [ viewPeriodFilter language model filterPeriodsForStatsPage
        , viewAllStatsCards language stats currentPeriodStats currentDate model healthCenterId db
        , viewBeneficiariesTable language currentDate stats currentPeriodStats model
        , viewFamilyPlanning language currentPeriodStats
        , viewStatsTableModal language model
        ]


viewCaseManagementPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewCaseManagementPage language currentDate stats model =
    let
        currentPeriodStats =
            filterStatsWithinPeriod currentDate model stats

        currentMonth =
            Date.month currentDate
                |> Date.monthToNumber

        withinThreePreviousMonths monthNumber =
            case currentMonth of
                1 ->
                    monthNumber > 9

                2 ->
                    monthNumber > 10 || monthNumber == 1

                3 ->
                    monthNumber == 12 || monthNumber == 1 || monthNumber == 2

                _ ->
                    monthNumber < currentMonth && monthNumber >= (currentMonth - 3)

        filterForCaseManagementTableFunc caseNutrition =
            caseNutrition
                |> Dict.toList
                |> List.filter (Tuple.first >> withinThreePreviousMonths)
                |> List.sortBy Tuple.first
                |> List.reverse

        -- We want data for 3 previous months, where's there's at least
        -- one entry is non-normal or missed.
        tableData =
            List.foldl
                (\person accum ->
                    case model.currentCaseManagementFilter of
                        Stunting ->
                            { name = person.name, nutrition = filterForCaseManagementTableFunc person.nutrition.stunting } :: accum

                        Underweight ->
                            { name = person.name, nutrition = filterForCaseManagementTableFunc person.nutrition.underweight } :: accum

                        Wasting ->
                            { name = person.name, nutrition = filterForCaseManagementTableFunc person.nutrition.wasting } :: accum

                        MUAC ->
                            { name = person.name, nutrition = filterForCaseManagementTableFunc person.nutrition.muac } :: accum
                )
                []
                currentPeriodStats.caseManagement
                |> List.filter
                    (.nutrition
                        >> List.all (Tuple.second >> .class >> (==) Backend.Dashboard.Model.Good)
                        >> not
                    )
                -- List table by person's name but turn it to lowercase for the comparision so it's truly sorted, this
                -- will not effect the display.
                |> List.sortWith (\p1 p2 -> compare (String.toLower p1.name) (String.toLower p2.name))
    in
    div [ class "dashboard case" ]
        [ viewPeriodFilter language model filterPeriodsForCaseManagementPage
        , div [ class "ui segment blue" ]
            [ div [ class "case-management" ]
                [ div [ class "header" ]
                    [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.CaseManagementLabel ]
                    , div [ class "filters" ]
                        (List.map (viewFilters FilterCaseManagement model.currentCaseManagementFilter) filterCharts)
                    ]
                , div [ class "content" ]
                    [ viewCaseManagementTable language currentDate model tableData ]
                ]
            ]
        ]


viewCaseManagementTable : Language -> NominalDate -> Model -> List { name : String, nutrition : List ( Int, NutritionValue ) } -> Html Msg
viewCaseManagementTable language currentDate model tableData =
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


viewGoodNutrition : Language -> Maybe GoodNutrition -> Html Msg
viewGoodNutrition language maybeNutrition =
    case maybeNutrition of
        Just nutrition ->
            let
                percentageThisYear =
                    (toFloat nutrition.good.thisYear / toFloat nutrition.all.thisYear)
                        * 100
                        |> round

                percentageLastYear =
                    calculatePercentage nutrition.good.thisYear nutrition.good.lastYear
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
                    , previousPercentage = percentageLastYear
                    , previousPercentageLabel = OneYear
                    , newCases = Nothing
                    }
            in
            viewCard language statsCard

        Nothing ->
            emptyNode


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


viewAllStatsCards : Language -> DashboardStats -> DashboardStats -> NominalDate -> Model -> HealthCenterId -> ModelIndexedDb -> Html Msg
viewAllStatsCards language stats currentPeriodStats currentDate model healthCenterId db =
    let
        modelWithLastMonth =
            if model.period == ThisMonth then
                { model | period = LastMonth }

            else
                { model | period = ThreeMonthsAgo }

        monthBeforeStats =
            filterStatsWithinPeriod currentDate modelWithLastMonth stats
    in
    div [ class "ui equal width grid" ]
        [ viewMalnourishedCards language currentPeriodStats monthBeforeStats
        , viewMiscCards language currentDate currentPeriodStats monthBeforeStats
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

        totalPercentage =
            calculatePercentage total totalBefore
                |> round

        malnourishedBeforeIdentifiers =
            monthBeforeStats.malnourished
                |> List.map .identifier

        malnourishedNewCases =
            stats.malnourished
                |> List.filter (\malnourished -> List.member malnourished.identifier malnourishedBeforeIdentifiers |> not)
                |> List.length

        totalCard =
            { title = translate language <| Translate.Dashboard Translate.TotalMalnourished
            , cardClasses = "stats-card total-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
            , value = total
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , previousPercentage = totalPercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just malnourishedNewCases
            }

        severe =
            stats.malnourished
                |> List.filter (\row -> row.zscore <= -2)

        severeBefore =
            monthBeforeStats.malnourished
                |> List.filter (\row -> row.zscore <= -2)

        severePercentage =
            calculatePercentage (List.length severe) (List.length severeBefore)
                |> round

        severeBeforeIdentifiers =
            severeBefore
                |> List.map .identifier

        severeNewCases =
            severe
                |> List.filter (\severe_ -> List.member severe_.identifier severeBeforeIdentifiers |> not)
                |> List.length

        severeCard =
            { title = translate language <| Translate.Dashboard Translate.SeverelyMalnourished
            , cardClasses = "stats-card severely-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
            , value = List.length severe
            , valueSeverity = Severe
            , valueIsPercentage = False
            , previousPercentage = severePercentage
            , previousPercentageLabel = ThisMonth
            , newCases = Just severeNewCases
            }

        moderate =
            stats.malnourished
                |> List.filter (\row -> row.zscore > -2)

        moderateBefore =
            monthBeforeStats.malnourished
                |> List.filter (\row -> row.zscore > -2)

        moderatePercentage =
            calculatePercentage (List.length moderate) (List.length moderateBefore)
                |> round

        moderateBeforeIdentifiers =
            moderateBefore
                |> List.map .identifier

        moderateNewCases =
            moderate
                |> List.filter (\moderate_ -> List.member moderate_.identifier moderateBeforeIdentifiers |> not)
                |> List.length

        moderateCard =
            { title = translate language <| Translate.Dashboard Translate.ModeratelyMalnourished
            , cardClasses = "stats-card moderately-malnourished"
            , cardAction = Just (SetActivePage <| UserPage <| DashboardPage <| CaseManagementPage)
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
                    { name = childrenBeneficiaries.name
                    , gender = childrenBeneficiaries.gender
                    , birthDate = childrenBeneficiaries.birthDate
                    , motherName = childrenBeneficiaries.motherName
                    , phoneNumber = childrenBeneficiaries.phoneNumber
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
            , cardAction = Just (ModalToggle True totalNewBeneficiariesTable totalNewBeneficiariesTitle)
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
            , cardAction = Just (ModalToggle True stats.completedPrograms completedProgramTitle)
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
            , cardAction = Just (ModalToggle True stats.missedSessions missedSessionsTitle)
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


viewBeneficiariesTable : Language -> NominalDate -> DashboardStats -> DashboardStats -> Model -> Html Msg
viewBeneficiariesTable language currentDate stats currentPeriodStats model =
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
                                |> Date.ceiling Date.Month
                                |> Date.add Days -1

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
            filterStatsByGender currentDate model currentPeriodStats

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

        getTotalMalnourishedCount stats_ =
            lengthAsString stats_.malnourished

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
                        , td [] [ text <| getTotalMalnourishedCount currentPeriodStats0_5 ]
                        , td [] [ text <| getTotalMalnourishedCount currentPeriodStats6_8 ]
                        , td [] [ text <| getTotalMalnourishedCount currentPeriodStats9_11 ]
                        , td [] [ text <| getTotalMalnourishedCount currentPeriodStats12_25 ]
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
                    |> List.sortBy (\( name, val ) -> Debug.toString name)
        in
        div [ class "content" ]
            [ viewChart signs
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
            , viewFamilyPlanningChartLegend language signs
            ]


viewMonthlyChart : Language -> NominalDate -> MonthlyChartType -> FilterType -> Dict Int TotalBeneficiaries -> FilterCharts -> Html Msg
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
                    div [ class "title left floated column" ] [ text <| translate language (Translate.Dashboard Translate.TotalBeneficiaries) ++ " " ++ toString currentFilter ++ " (%)" ]

                MonthlyChartIncidence ->
                    div [ class "title left floated column" ]
                        [ div [] [ text <| translate language (Translate.Dashboard Translate.IncidenceOf) ++ " " ++ toString currentFilter ++ " (%)" ]
                        , div [ class "helper" ] [ text "(New cases per month)" ]
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
            [ caption
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


viewFamilyPlanningChartLegend : Language -> List ( FamilyPlanningSign, Int ) -> Html Msg
viewFamilyPlanningChartLegend language signs =
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
            signs
        )


viewChart : List ( FamilyPlanningSign, Int ) -> Svg msg
viewChart signs =
    let
        arcs =
            signs
                |> List.map (Tuple.second >> toFloat)

        signsList =
            signs
                |> List.map Tuple.first

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
            applyAgeFilter currentDate func stats.childrenBeneficiaries

        completedPrograms =
            applyAgeFilter currentDate func stats.completedPrograms

        missedSessions =
            applyAgeFilter currentDate func stats.missedSessions

        malnourished =
            applyAgeFilter currentDate func stats.malnourished
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiaries
        , completedPrograms = completedPrograms
        , missedSessions = missedSessions
        , malnourished = malnourished
    }


applyAgeFilter : NominalDate -> ({ months : Int, days : Int } -> Bool) -> List { a | birthDate : NominalDate } -> List { a | birthDate : NominalDate }
applyAgeFilter currentDate func list =
    List.filter (\item -> isDiffTruthy item.birthDate currentDate func) list


filterStatsWithinPeriod : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsWithinPeriod currentDate model stats =
    filterStatsByPeriod isBetween currentDate model stats


filterStatsOutsidePeriod : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsOutsidePeriod currentDate model stats =
    let
        outside start end date =
            isBetween start end date |> not
    in
    filterStatsByPeriod outside currentDate model stats


{-| Filter stats to match the selected period.
-}
filterStatsByPeriod : (NominalDate -> NominalDate -> NominalDate -> Bool) -> NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByPeriod fiterFunc currentDate model stats =
    let
        ( startDate, endDate ) =
            case model.period of
                OneYear ->
                    ( Date.add Years -1 currentDate, Date.add Days 1 currentDate )

                ThisMonth ->
                    -- From beginning of the month to this day.
                    ( Date.floor Date.Month currentDate, Date.add Days 1 currentDate )

                LastMonth ->
                    -- From the beginning of last month to the end of last month.
                    ( Date.add Months -1 currentDate
                        |> Date.floor Date.Month
                    , Date.add Months -1 currentDate
                        |> Date.ceiling Date.Month
                        -- We have to remove a day because the "ceiling" function for some reason is going up to the
                        -- first day of the next month.
                        |> Date.add Days -1
                    )

                ThreeMonthsAgo ->
                    -- From the beginning of 3 months ago to the end of 3 months ago.
                    ( Date.add Months -2 currentDate
                        |> Date.floor Date.Month
                    , Date.add Months -2 currentDate
                        |> Date.ceiling Date.Month
                        -- We have to remove a day because the "ceiling" function for some reason is going up to the
                        -- first day of the next month.
                        |> Date.add Days -1
                    )

        filterPartial =
            fiterFunc startDate endDate

        childrenBeneficiariesUpdated =
            stats.childrenBeneficiaries
                |> List.filter (\child -> filterPartial child.memberSince)

        familyPlanningUpdated =
            stats.familyPlanning
                |> List.filter (\familyPlanning -> filterPartial familyPlanning.created)

        malnourishedUpdated =
            stats.malnourished
                |> List.filter (\malnourished -> filterPartial malnourished.created)

        completedPrograms =
            stats.completedPrograms
                |> List.filter (\completedProgram -> filterPartial completedProgram.expectedDate)

        missedSessions =
            stats.missedSessions
                |> List.filter (\missedSession -> filterPartial missedSession.expectedDate)
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiariesUpdated
        , familyPlanning = familyPlanningUpdated
        , malnourished = malnourishedUpdated
        , completedPrograms = completedPrograms
        , missedSessions = missedSessions
    }


{-| Filter stats to match the selected gender.
-}
filterStatsByGender : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByGender currentDate model stats =
    { stats
        | childrenBeneficiaries = applyGenderFilter model stats.childrenBeneficiaries
        , completedPrograms = applyGenderFilter model stats.completedPrograms
        , missedSessions = applyGenderFilter model stats.missedSessions
        , malnourished = applyGenderFilter model stats.malnourished
    }


applyGenderFilter : Model -> List { a | gender : Backend.Person.Model.Gender } -> List { a | gender : Backend.Person.Model.Gender }
applyGenderFilter model list =
    List.filter
        (\item ->
            case ( item.gender, model.beneficiariesGender ) of
                ( Backend.Person.Model.Male, Pages.Dashboard.Model.Boys ) ->
                    True

                ( Backend.Person.Model.Female, Pages.Dashboard.Model.Girls ) ->
                    True

                _ ->
                    False
        )
        list


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
