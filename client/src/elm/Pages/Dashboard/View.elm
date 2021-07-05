module Pages.Dashboard.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Dashboard.Model
    exposing
        ( AcuteIllnessDataItem
        , AcuteIllnessEncounterDataItem
        , AssembledData
        , CaseManagement
        , CaseManagementData
        , CaseNutrition
        , CaseNutritionTotal
        , ChildrenBeneficiariesData
        , DashboardStats
        , Nutrition
        , NutritionValue
        , ParticipantStats
        , Periods
        , PersonIdentifier
        , PrenatalDataItem
        , ProgramType(..)
        , TotalBeneficiaries
        , TotalEncountersData
        , emptyNutrition
        , emptyNutritionValue
        , emptyTotalBeneficiaries
        )
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..), IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Month, Unit(..), isBetween, monthNumber, numberToMonth, year)
import Debug exposing (toString)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, allMonths, formatYYYYMMDD, isDiffTruthy, yearYYNumber)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Dashboard.GraphUtils exposing (..)
import Pages.Dashboard.Model exposing (..)
import Pages.Dashboard.Utils exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (allEncounterTypes)
import Pages.Page exposing (AcuteIllnessDashboardPage(..), ChwDashboardPage(..), DashboardPage(..), NurseDashboardPage(..), Page(..), UserPage(..))
import Pages.Utils exposing (calculatePercentage)
import Path
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, r)
import Time exposing (millisToPosix)
import Translate exposing (Language, TranslationId, translate, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes as Explicit exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..))
import Utils.Html exposing (spinner, viewModal)


{-| Shows a dashboard page.
-}
view : Language -> DashboardPage -> NominalDate -> HealthCenterId -> Bool -> Nurse -> Model -> ModelIndexedDb -> Html Msg
view language page currentDate healthCenterId isChw nurse model db =
    let
        header =
            case page of
                MainPage ->
                    let
                        label =
                            if isChw then
                                Translate.ChwDashboardLabel

                            else
                                Translate.DashboardLabel
                    in
                    viewHeader language label PinCodePage

                ChwPage chwDashboardPage ->
                    case chwDashboardPage of
                        AcuteIllnessPage _ ->
                            viewHeader language (Translate.EncounterTypeFileterLabel AcuteIllnessEncounter) (UserPage <| DashboardPage MainPage)

                        NutritionPage ->
                            viewHeader language (Translate.EncounterTypeFileterLabel NutritionEncounter) (UserPage <| DashboardPage MainPage)

                        AntenatalPage ->
                            viewHeader language (Translate.EncounterTypeFileterLabel AntenatalEncounter) (UserPage <| DashboardPage MainPage)

                NursePage nurseDashboardPage ->
                    let
                        goBackPage =
                            case nurseDashboardPage of
                                StatsPage ->
                                    UserPage <| DashboardPage MainPage

                                CaseManagementPage ->
                                    UserPage <| DashboardPage model.latestPage
                    in
                    viewHeader language Translate.DashboardLabel goBackPage

        content =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.map
                    (\stats ->
                        let
                            assembled =
                                generateAssembledData healthCenterId stats db model

                            ( pageContent, pageClass ) =
                                case page of
                                    MainPage ->
                                        ( viewMainPage language currentDate healthCenterId isChw nurse assembled db model
                                        , "main"
                                        )

                                    NursePage nurseDashboardPage ->
                                        case nurseDashboardPage of
                                            StatsPage ->
                                                ( viewStatsPage language currentDate False nurse assembled.stats healthCenterId db model, "stats" )

                                            CaseManagementPage ->
                                                ( viewCaseManagementPage language currentDate assembled.stats db model, "case" )

                                    ChwPage chwDashboardPage ->
                                        case chwDashboardPage of
                                            AcuteIllnessPage acuteIllnessPage ->
                                                ( viewAcuteIllnessPage language currentDate acuteIllnessPage assembled db model, "acute-illness" )

                                            NutritionPage ->
                                                ( viewNutritionPage language currentDate True nurse assembled.stats db model, "nutrition" )

                                            AntenatalPage ->
                                                ( viewAntenatalPage language currentDate assembled db model, "prenatal" )
                        in
                        div [ class <| "dashboard " ++ pageClass ] <|
                            [ viewFiltersPane language page db model ]
                                ++ pageContent
                                ++ [ viewCustomModal language isChw nurse assembled.stats db model
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


viewHeader : Language -> TranslationId -> Page -> Html Msg
viewHeader language label goBackPage =
    div [ class "ui basic head segment" ]
        [ h1 [ class "ui header" ]
            [ translateText language label ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewMainPage : Language -> NominalDate -> HealthCenterId -> Bool -> Nurse -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewMainPage language currentDate healthCenterId isChw nurse assembled db model =
    if isChw then
        viewChwMainPage language currentDate healthCenterId assembled db model

    else
        viewNutritionPage language currentDate False nurse assembled.stats db model


viewChwMainPage : Language -> NominalDate -> HealthCenterId -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewChwMainPage language currentDate healthCenterId assembled db model =
    let
        selectedDate =
            getSelectedDate currentDate model

        -- ANC
        encountersForSelectedMonth =
            getAcuteIllnessEncountersForSelectedMonth selectedDate assembled.acuteIllnessData

        ( sentToHC, managedLocally ) =
            countAcuteIllnessCasesByHCReferrals encountersForSelectedMonth

        -- Prenatal
        currentlyPregnant =
            countCurrentlyPregnantForSelectedMonth currentDate selectedDate assembled.prenatalData

        totalNewborn =
            countNewbornForSelectedMonth selectedDate assembled.prenatalData

        -- Case Management
        ( totalNutritionFollowUps, totalAcuteIllnessFollowUps, totalPrenatalFollowUps ) =
            Maybe.map2 (getFollowUpsTotals language currentDate db)
                model.selectedVillageFilter
                assembled.caseManagementData
                |> Maybe.withDefault ( 0, 0, 0 )
    in
    [ viewChwMenu language
    , monthSelector language selectedDate model
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.AcuteIllnessDiagnosed) (String.fromInt <| sentToHC + managedLocally)
            , chwCard language (Translate.Dashboard Translate.MothersInANC) (String.fromInt currentlyPregnant)
            , chwCard language (Translate.Dashboard Translate.NewbornsInCare) (String.fromInt totalNewborn)
            ]
        ]
    , div [ class "case-management-label" ] [ text <| translate language <| Translate.CaseManagement ]
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.EncounterTypeFileterLabel AcuteIllnessEncounter) (String.fromInt totalAcuteIllnessFollowUps)
            , chwCard language (Translate.EncounterTypeFileterLabel NutritionEncounter) (String.fromInt totalNutritionFollowUps)
            , chwCard language (Translate.EncounterTypeFileterLabel AntenatalEncounter) (String.fromInt totalPrenatalFollowUps)
            ]
        ]
    ]


applyProgramTypeAndResidentsFilters :
    Dict VillageId (List PersonIdentifier)
    -> Dict ProgramType (List { a | identifier : PersonIdentifier })
    -> Model
    -> List { a | identifier : PersonIdentifier }
applyProgramTypeAndResidentsFilters villagesWithResidents dict model =
    case model.programTypeFilter of
        FilterAllPrograms ->
            let
                achi =
                    Dict.get ProgramAchi dict
                        |> Maybe.withDefault []

                fbf =
                    Dict.get ProgramFbf dict
                        |> Maybe.withDefault []

                pmtct =
                    Dict.get ProgramPmtct dict
                        |> Maybe.withDefault []

                sorwathe =
                    Dict.get ProgramSorwathe dict
                        |> Maybe.withDefault []

                individual =
                    Dict.get ProgramIndividual dict
                        |> Maybe.withDefault []
            in
            achi ++ fbf ++ pmtct ++ sorwathe ++ individual

        FilterProgramAchi ->
            Dict.get ProgramAchi dict
                |> Maybe.withDefault []

        FilterProgramFbf ->
            Dict.get ProgramFbf dict
                |> Maybe.withDefault []

        FilterProgramPmtct ->
            Dict.get ProgramPmtct dict
                |> Maybe.withDefault []

        FilterProgramSorwathe ->
            Dict.get ProgramSorwathe dict
                |> Maybe.withDefault []

        FilterProgramCommunity ->
            let
                villageResidents =
                    model.selectedVillageFilter
                        |> Maybe.andThen (\village -> Dict.get village villagesWithResidents)
                        |> Maybe.withDefault []

                villageFilterFunc caseManagement =
                    if isJust model.selectedVillageFilter then
                        List.member caseManagement.identifier villageResidents

                    else
                        -- Do not filter by village, if village is not selected.
                        True

                achi =
                    Dict.get ProgramAchi dict
                        |> Maybe.withDefault []
                        |> List.filter villageFilterFunc

                fbf =
                    Dict.get ProgramFbf dict
                        |> Maybe.withDefault []
                        |> List.filter villageFilterFunc

                pmtct =
                    Dict.get ProgramPmtct dict
                        |> Maybe.withDefault []
                        |> List.filter villageFilterFunc

                sorwathe =
                    Dict.get ProgramSorwathe dict
                        |> Maybe.withDefault []
                        |> List.filter villageFilterFunc

                individual =
                    Dict.get ProgramIndividual dict
                        |> Maybe.withDefault []
                        |> List.filter villageFilterFunc
            in
            achi ++ fbf ++ pmtct ++ sorwathe ++ individual


totalEncountersApplyBreakdownFilters : TotalEncountersData -> Model -> Periods
totalEncountersApplyBreakdownFilters data model =
    let
        emptyPeriods =
            Periods 0 0
    in
    case model.programTypeFilter of
        FilterAllPrograms ->
            let
                achi =
                    Dict.get ProgramAchi data.global
                        |> Maybe.withDefault emptyPeriods

                fbf =
                    Dict.get ProgramFbf data.global
                        |> Maybe.withDefault emptyPeriods

                pmtct =
                    Dict.get ProgramPmtct data.global
                        |> Maybe.withDefault emptyPeriods

                sorwathe =
                    Dict.get ProgramSorwathe data.global
                        |> Maybe.withDefault emptyPeriods

                individual =
                    Dict.get ProgramIndividual data.global
                        |> Maybe.withDefault emptyPeriods

                sumPeriods p1 p2 =
                    Periods (p1.lastYear + p2.lastYear) (p1.thisYear + p2.thisYear)
            in
            sumPeriods achi fbf
                |> sumPeriods pmtct
                |> sumPeriods sorwathe
                |> sumPeriods individual

        FilterProgramAchi ->
            Dict.get ProgramAchi data.global
                |> Maybe.withDefault emptyPeriods

        FilterProgramFbf ->
            Dict.get ProgramFbf data.global
                |> Maybe.withDefault emptyPeriods

        FilterProgramPmtct ->
            Dict.get ProgramPmtct data.global
                |> Maybe.withDefault emptyPeriods

        FilterProgramSorwathe ->
            Dict.get ProgramSorwathe data.global
                |> Maybe.withDefault emptyPeriods

        FilterProgramCommunity ->
            let
                dict =
                    case model.selectedVillageFilter of
                        Just village ->
                            Dict.get village data.villages
                                |> Maybe.withDefault Dict.empty

                        -- When village is not selected, we show global data.
                        Nothing ->
                            data.global

                achi =
                    Dict.get ProgramAchi dict
                        |> Maybe.withDefault emptyPeriods

                fbf =
                    Dict.get ProgramFbf dict
                        |> Maybe.withDefault emptyPeriods

                pmtct =
                    Dict.get ProgramPmtct dict
                        |> Maybe.withDefault emptyPeriods

                sorwathe =
                    Dict.get ProgramSorwathe dict
                        |> Maybe.withDefault emptyPeriods

                individual =
                    Dict.get ProgramIndividual dict
                        |> Maybe.withDefault emptyPeriods

                sumPeriods p1 p2 =
                    Periods (p1.lastYear + p2.lastYear) (p1.thisYear + p2.thisYear)
            in
            sumPeriods achi fbf
                |> sumPeriods pmtct
                |> sumPeriods sorwathe
                |> sumPeriods individual


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


generateTotalBeneficiariesMonthlyDuringPastYear :
    NominalDate
    -> DashboardStats
    -> Model
    -> Dict Int Int
generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats model =
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
    , nutritionSigns = generateTotals caseNutrition.nutritionSigns
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
                            if previousNutritionValue.class /= Backend.Dashboard.Model.Moderate then
                                ( month, Backend.Dashboard.Model.Nutrition 0 1 )

                            else
                                ( month, Backend.Dashboard.Model.Nutrition 0 0 )

                        Backend.Dashboard.Model.Severe ->
                            if previousNutritionValue.class /= Backend.Dashboard.Model.Severe then
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
    , nutritionSigns = generateTotals caseNutrition.nutritionSigns
    }


viewStatsPage : Language -> NominalDate -> Bool -> Nurse -> DashboardStats -> HealthCenterId -> ModelIndexedDb -> Model -> List (Html Msg)
viewStatsPage language currentDate isChw nurse stats healthCenterId db model =
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
                filterStatsWithinPeriod currentDate model stats

            monthBeforeStats =
                filterStatsWithinPeriod currentDate modelWithLastMonth stats

            currentPeriodCaseManagement =
                applyProgramTypeAndResidentsFilters stats.villagesWithResidents currentPeriodStats.caseManagement.thisYear model

            malnourishedCurrentMonth =
                mapMalnorishedByMonth displayedMonth currentPeriodCaseManagement

            malnourishedPreviousMonth =
                mapMalnorishedByMonth (resolvePreviousMonth displayedMonth) currentPeriodCaseManagement
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
                                        if List.any ((==) Backend.Dashboard.Model.Severe) values then
                                            MalnorishedNutritionData caseNutrition.identifier
                                                caseNutrition.birthDate
                                                caseNutrition.gender
                                                Backend.Dashboard.Model.Severe
                                                |> Just

                                        else if List.any ((==) Backend.Dashboard.Model.Moderate) values then
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


viewCaseManagementPage : Language -> NominalDate -> DashboardStats -> ModelIndexedDb -> Model -> List (Html Msg)
viewCaseManagementPage language currentDate stats db model =
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
                                in
                                { name = caseNutrition.name, nutrition = nutrition } :: accum
                            )
                            []
                            (applyProgramTypeAndResidentsFilters stats.villagesWithResidents stats.caseManagement.thisYear model)
                            |> List.filter (.nutrition >> List.all (Tuple.second >> .class >> (==) Backend.Dashboard.Model.Good) >> not)

                    _ ->
                        List.foldl
                            (\caseNutrition accum ->
                                case model.currentCaseManagementFilter of
                                    Stunting ->
                                        { name = caseNutrition.name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.stunting } :: accum

                                    Underweight ->
                                        { name = caseNutrition.name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.underweight } :: accum

                                    Wasting ->
                                        { name = caseNutrition.name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.wasting } :: accum

                                    MUAC ->
                                        { name = caseNutrition.name, nutrition = filterForCaseManagementTableFunc caseNutrition.nutrition.muac } :: accum

                                    -- We'll never get here - need to list it to satisfy compiler.
                                    MissedSession ->
                                        accum
                            )
                            []
                            (applyProgramTypeAndResidentsFilters stats.villagesWithResidents stats.caseManagement.thisYear model)
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


viewFiltersPane : Language -> DashboardPage -> ModelIndexedDb -> Model -> Html Msg
viewFiltersPane language page db model =
    let
        filters =
            case page of
                MainPage ->
                    [ labelSelected, programTypeFilterButton ]

                NursePage nursePage ->
                    case nursePage of
                        StatsPage ->
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

                        CaseManagementPage ->
                            []

                ChwPage _ ->
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


viewChwMenu : Language -> Html Msg
viewChwMenu language =
    div [ class "ui segment chw-filters" ]
        [ viewChwMenuButton language AntenatalPage Nothing
        , viewChwMenuButton language NutritionPage Nothing
        , viewChwMenuButton language (AcuteIllnessPage OverviewPage) Nothing
        ]


viewAcuteIllnessMenu : Language -> AcuteIllnessDashboardPage -> Html Msg
viewAcuteIllnessMenu language activePage =
    let
        viewMenu targetPage =
            button
                [ classList
                    [ ( "active", activePage == targetPage )
                    , ( "primary ui button", True )
                    ]
                , DashboardPage (ChwPage <| AcuteIllnessPage targetPage)
                    |> UserPage
                    |> SetActivePage
                    |> onClick
                ]
                [ translateText language <| Translate.EncounterTypePageLabel <| AcuteIllnessPage targetPage ]
    in
    div [ class "ui segment chw-filters" ]
        [ viewChwMenuButton language (AcuteIllnessPage OverviewPage) (Just <| AcuteIllnessPage activePage)
        , viewChwMenuButton language (AcuteIllnessPage Covid19Page) (Just <| AcuteIllnessPage activePage)
        , viewChwMenuButton language (AcuteIllnessPage MalariaPage) (Just <| AcuteIllnessPage activePage)
        , viewChwMenuButton language (AcuteIllnessPage GastroPage) (Just <| AcuteIllnessPage activePage)
        ]


viewChwMenuButton : Language -> ChwDashboardPage -> Maybe ChwDashboardPage -> Html Msg
viewChwMenuButton language targetPage activePage =
    let
        label =
            if isNothing activePage && targetPage == AcuteIllnessPage OverviewPage then
                -- On Main page, and target is Acute Illness page.
                Translate.EncounterTypeFileterLabel AcuteIllnessEncounter

            else
                Translate.EncounterTypePageLabel targetPage
    in
    button
        [ classList
            [ ( "active", activePage == Just targetPage )
            , ( "primary ui button", True )
            ]
        , DashboardPage (ChwPage targetPage)
            |> UserPage
            |> SetActivePage
            |> onClick
        ]
        [ translateText language label ]


viewAcuteIllnessPage : Language -> NominalDate -> AcuteIllnessDashboardPage -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewAcuteIllnessPage language currentDate activePage assembled db model =
    let
        selectedDate =
            getSelectedDate currentDate model

        encountersForSelectedMonth =
            getAcuteIllnessEncountersForSelectedMonth selectedDate assembled.acuteIllnessData

        ( managedCovid, managedMalaria, managedGI ) =
            Maybe.map2 (getAcuteIllnessFollowUpsBreakdownByDiagnosis language currentDate db)
                model.selectedVillageFilter
                assembled.caseManagementData
                |> Maybe.withDefault ( 0, 0, 0 )

        pageContent =
            case activePage of
                OverviewPage ->
                    viewAcuteIllnessOverviewPage language encountersForSelectedMonth model

                Covid19Page ->
                    viewCovid19Page language encountersForSelectedMonth managedCovid model

                MalariaPage ->
                    viewMalariaPage language selectedDate assembled.acuteIllnessData encountersForSelectedMonth managedMalaria model

                GastroPage ->
                    viewGastroPage language selectedDate assembled.acuteIllnessData encountersForSelectedMonth managedGI model
    in
    [ viewAcuteIllnessMenu language activePage
    , monthSelector language selectedDate model
    ]
        ++ pageContent


viewAcuteIllnessOverviewPage : Language -> List AcuteIllnessEncounterDataItem -> Model -> List (Html Msg)
viewAcuteIllnessOverviewPage language encounters model =
    let
        totalAssesments =
            countAcuteIllnessAssesments encounters

        ( sentToHC, managedLocally ) =
            countAcuteIllnessCasesByHCReferrals encounters

        undeterminedCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisUndeterminedMoreEvaluationNeeded ] False encounters

        feverOfUnknownOriginCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisFeverOfUnknownOrigin ] False encounters

        covidCases =
            countAcuteIllnessCasesByPossibleDiagnosises [ DiagnosisCovid19 ] True encounters

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

        feverByCauses =
            List.filter (Tuple.second >> (/=) 0)
                [ ( FeverCauseCovid19, covidCases )
                , ( FeverCauseMalaria, malariaCases )
                , ( FeverCauseRespiratory, respiratoryCases )
                , ( FeverCauseGI, giCases )
                , ( FeverCauseUnknown, feverOfUnknownOriginCases )
                ]
    in
    [ div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.TotalAssessment) (String.fromInt totalAssesments)
            , chwCard language (Translate.Dashboard Translate.CommunityLevelCases) (String.fromInt managedLocally)
            , chwCard language (Translate.Dashboard Translate.HealthCenterReferrals) (String.fromInt sentToHC)
            ]
        ]
    , div [ class "ui centered grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.DiagnosisUndetermined) (String.fromInt undeterminedCases)
            , chwCard language (Translate.Dashboard Translate.FeverOfUnknownOrigin) (String.fromInt feverOfUnknownOriginCases)
            ]
        ]
    , div [ class "ui blue segment donut-chart fever" ]
        [ viewFeverDistributionDonutChart language feverByCauses ]
    ]


viewFeverDistributionDonutChart : Language -> List ( FeverCause, Int ) -> Html Msg
viewFeverDistributionDonutChart language feverByCauses =
    if List.isEmpty feverByCauses then
        div [ class "no-data-message" ] [ translateText language <| Translate.Dashboard Translate.NoDataForPeriod ]

    else
        div [ class "ui center aligned grid" ]
            [ div [ class "middle aligned row" ]
                [ div [ class "content" ]
                    [ viewPieChart feverCausesColors feverByCauses
                    , div [ class "in-chart" ]
                        [ translateText language <| Translate.Dashboard Translate.FeversByCause ]
                    , viewPieChartLegend language (Translate.FeverCause >> Translate.Dashboard) feverCauseToColor feverByCauses
                    ]
                ]
            ]


viewCovid19Page : Language -> List AcuteIllnessEncounterDataItem -> Int -> Model -> List (Html Msg)
viewCovid19Page language encounters managedCovid model =
    let
        callsTo114 =
            countDiagnosedWithCovidCallsTo114 encounters

        sentToHC =
            countDiagnosedWithCovidSentToHC encounters

        managedAtHome =
            countDiagnosedWithCovidManagedAtHome encounters
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


viewMalariaPage : Language -> NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem -> Int -> Model -> List (Html Msg)
viewMalariaPage language selectedDate acuteIllnessData encountersForSelectedMonth managedMalaria model =
    let
        totalDaignosed =
            countDiagnosedWithMalaria encountersForSelectedMonth

        uncomplicatedMalariaManagedByChw =
            countUncomplicatedMalariaManagedByChw encountersForSelectedMonth

        uncomplicatedMalariaAndPregnantSentToHC =
            countUncomplicatedMalariaAndPregnantSentToHC encountersForSelectedMonth

        complicatedMalariaSentToHC =
            countComplicatedMalariaSentToHC encountersForSelectedMonth

        resolvedMalariaCases =
            countResolvedMalariaCasesForSelectedMonth selectedDate acuteIllnessData
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


viewGastroPage : Language -> NominalDate -> List AcuteIllnessDataItem -> List AcuteIllnessEncounterDataItem -> Int -> Model -> List (Html Msg)
viewGastroPage language selectedDate acuteIllnessData encountersForSelectedMonth managedGI model =
    let
        totalDaignosed =
            countDiagnosedWithGI encountersForSelectedMonth

        uncomplicatedGIManagedByChw =
            countUncomplicatedGIManagedByChw encountersForSelectedMonth

        complicatedGISentToHC =
            countComplicatedGISentToHC encountersForSelectedMonth

        resolvedGICases =
            countResolvedGICasesForSelectedMonth selectedDate acuteIllnessData
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


viewNutritionPage : Language -> NominalDate -> Bool -> Nurse -> DashboardStats -> ModelIndexedDb -> Model -> List (Html Msg)
viewNutritionPage language currentDate isChw nurse stats db model =
    let
        currentPeriodStats =
            filterStatsWithinPeriod currentDate model stats

        totalBeneficiariesMonthlyDuringPastYear =
            generateTotalBeneficiariesMonthlyDuringPastYear currentDate stats model

        emptyTotalBeneficiariesDict =
            List.repeat 12 emptyTotalBeneficiaries
                |> List.indexedMap (\index empty -> ( index + 1, empty ))
                |> Dict.fromList

        caseManagementsThisYear =
            applyProgramTypeAndResidentsFilters stats.villagesWithResidents stats.caseManagement.thisYear model

        caseManagementsLastYear =
            applyProgramTypeAndResidentsFilters stats.villagesWithResidents stats.caseManagement.lastYear model

        caseNutritionTotalsThisYear =
            caseManagementsThisYear
                |> List.map (.nutrition >> generateCaseNutritionTotals)

        caseNutritionTotalsLastYear =
            caseManagementsLastYear
                |> List.map (.nutrition >> generateCaseNutritionTotals)

        totalsGraphData =
            caseNutritionTotalsThisYear
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear

        newCasesGraphData =
            caseManagementsThisYear
                |> List.map (.nutrition >> generateCaseNutritionNewCases currentDate)
                |> List.foldl accumCaseNutritionTotals emptyTotalBeneficiariesDict
                |> applyTotalBeneficiariesDenomination totalBeneficiariesMonthlyDuringPastYear

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
            [ viewGoodNutrition language caseNutritionTotalsThisYear caseNutritionTotalsLastYear
            ]
        , div [ class "eight wide column" ]
            [ totalEncountersApplyBreakdownFilters currentPeriodStats.totalEncounters model
                |> viewTotalEncounters language
            ]
        , div [ class "sixteen wide column" ]
            [ viewMonthlyChart language currentDate MonthlyChartTotals FilterBeneficiariesChart totalsGraphData model.currentBeneficiariesChartsFilter
            ]
        , div [ class "sixteen wide column" ]
            [ viewMonthlyChart language currentDate MonthlyChartIncidence FilterBeneficiariesIncidenceChart newCasesGraphData model.currentBeneficiariesIncidenceChartsFilter
            ]
        , links
        ]
    ]


viewAntenatalPage : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewAntenatalPage language currentDate assembled db model =
    let
        selectedDate =
            getSelectedDate currentDate model

        newlyIdentifiedPreganancies =
            countNewlyIdentifiedPregananciesForSelectedMonth selectedDate assembled.prenatalData

        currentlyPregnant =
            countCurrentlyPregnantForSelectedMonth currentDate selectedDate assembled.prenatalData

        pregnanciesDueWithin4Month =
            countPregnanciesDueWithin4MonthsForSelectedMonth selectedDate assembled.prenatalData

        currentlyPregnantWithDangerSigns =
            countCurrentlyPregnantWithDangerSignsForSelectedMonth currentDate selectedDate assembled.prenatalData

        deliveriesAtHome =
            countDeliveriesAtLocationForSelectedMonth selectedDate HomeDelivery assembled.prenatalData

        deliveriesAtFacility =
            countDeliveriesAtLocationForSelectedMonth selectedDate FacilityDelivery assembled.prenatalData
    in
    [ monthSelector language selectedDate model
    , div [ class "ui grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.NewPregnancy) (String.fromInt newlyIdentifiedPreganancies)
            , chwCard language (Translate.Dashboard Translate.CurrentPregnancies) (String.fromInt currentlyPregnant)
            , chwCard language (Translate.Dashboard Translate.Within4MonthsOfDueDate) (String.fromInt pregnanciesDueWithin4Month)
            ]
        ]
    , div [ class "ui centered grid" ]
        [ div [ class "three column row" ]
            [ chwCard language (Translate.Dashboard Translate.WithDangerSigns) (String.fromInt currentlyPregnantWithDangerSigns)
            , chwCard language (Translate.Dashboard Translate.HomeDeliveries) (String.fromInt deliveriesAtHome)
            , chwCard language (Translate.Dashboard Translate.HealthFacilityDeliveries) (String.fromInt deliveriesAtFacility)
            ]
        ]
    ]


chwCard : Language -> TranslationId -> String -> Html Msg
chwCard language titleTransId value =
    div [ class "column" ]
        [ viewChwCard language titleTransId value ]


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


viewChwCard : Language -> TranslationId -> String -> Html Msg
viewChwCard language titleTransId value =
    div [ class "ui segment dashboard-card chw" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ text <| translate language titleTransId ]
            , div [ class "value this-year" ] [ text value ]
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
            , DashboardPage (NursePage StatsPage)
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
            , DashboardPage (NursePage CaseManagementPage)
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
                    |> List.sortBy (\( name, val ) -> Debug.toString name)
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
                [ fill <| Fill <| getColor index ]
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
                        [ Svg.circle [ cx "50", cy "50", r "40", fill <| Fill <| colorFunc sign ] []
                        ]
                    , span [] [ text <| label ++ " (" ++ normalizedPercentage ++ "%)" ]
                    ]
            )
            signs
        )


viewCustomModal : Language -> Bool -> Nurse -> DashboardStats -> ModelIndexedDb -> Model -> Html Msg
viewCustomModal language isChw nurse stats db model =
    model.modalState
        |> Maybe.map
            (\state ->
                case state of
                    StatisticsModal title data ->
                        viewStatsTableModal language title data

                    FiltersModal ->
                        viewFiltersModal language isChw nurse stats db model
            )
        |> viewModal


viewStatsTableModal : Language -> String -> List ParticipantStats -> Html Msg
viewStatsTableModal language title data =
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
                    (List.map viewModalTableRow data)
                ]
            ]
        ]


viewFiltersModal : Language -> Bool -> Nurse -> DashboardStats -> ModelIndexedDb -> Model -> Html Msg
viewFiltersModal language isChw nurse stats db model =
    let
        programTypeFilterInputSection =
            if isChw then
                -- For CHW nurses, program type is always set to FilterProgramCommunity.
                []

            else
                let
                    allOptions =
                        [ FilterAllPrograms
                        , FilterProgramFbf
                        , FilterProgramPmtct
                        , FilterProgramSorwathe
                        , FilterProgramAchi
                        , FilterProgramCommunity
                        ]

                    programTypeFilterInput =
                        allOptions
                            |> List.map
                                (\programTypeFilter ->
                                    option
                                        [ value (filterProgramTypeToString programTypeFilter)
                                        , selected (model.programTypeFilter == programTypeFilter)
                                        ]
                                        [ text <| translate language <| Translate.Dashboard <| Translate.FilterProgramType programTypeFilter ]
                                )
                            |> select
                                [ onInput SetFilterProgramType
                                , class "select-input"
                                ]
                in
                [ div [ class "helper" ] [ text <| translate language <| Translate.Dashboard Translate.ProgramType ]
                , programTypeFilterInput
                ]

        villageInputSection =
            if model.programTypeFilter /= FilterProgramCommunity then
                []

            else
                db.villages
                    |> RemoteData.toMaybe
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
                                    Dict.keys stats.villagesWithResidents
                                        |> List.filterMap
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

        closeButtonDisabled =
            (model.programTypeFilter == FilterProgramCommunity) && isNothing model.selectedVillageFilter

        closeButtonAttributes =
            if closeButtonDisabled then
                [ class "ui primary fluid button disabled"
                ]

            else
                [ class "ui primary fluid button"
                , onClick <| SetModalState Nothing
                ]
    in
    div [ class "ui active modal" ]
        [ div [ class "header" ]
            [ text <| translate language <| Translate.Dashboard Translate.Filters ]
        , div [ class "content" ] <|
            programTypeFilterInputSection
                ++ villageInputSection
        , div [ class "actions" ]
            [ button
                closeButtonAttributes
                [ text <| translate language Translate.Close ]
            ]
        ]


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
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiaries
        , completedPrograms = completedPrograms
        , missedSessions = missedSessions
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
monthSelector language selectedDate model =
    let
        monthNumber =
            Date.monthNumber selectedDate

        month =
            Date.numberToMonth monthNumber

        year =
            Date.year selectedDate
    in
    div [ class "month-selector" ]
        [ span
            [ classList
                [ ( "icon-back", True )
                , ( "hidden", model.monthGap == maxMonthGap )
                ]
            , onClick <| ChangeMonthGap 1
            ]
            []
        , span [ class "label" ]
            [ text <| translate language (Translate.ResolveMonth False month) ++ " " ++ String.fromInt year ]
        , span
            [ classList
                [ ( "icon-back rotate-180", True )
                , ( "hidden", model.monthGap == 0 )
                ]
            , onClick <| ChangeMonthGap -1
            ]
            []
        ]
