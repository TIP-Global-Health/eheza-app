module Pages.Reports.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (SelectedEntity(..))
import Backend.Reports.Model exposing (..)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra exposing (unique)
import Maybe.Extra
import Pages.Reports.Model exposing (..)
import Set


eddToLmpDate : NominalDate -> NominalDate
eddToLmpDate eddDate =
    Date.add Days -280 eddDate


resolvePregnancyTrimester : NominalDate -> NominalDate -> PregnancyTrimester
resolvePregnancyTrimester date lmpDate =
    let
        diffInWeeks =
            diffDays lmpDate date // 7
    in
    if diffInWeeks < 13 then
        FirstTrimester

    else if diffInWeeks < 28 then
        SecondTrimester

    else
        ThirdTrimester


reportTypeToString : ReportType -> String
reportTypeToString reportType =
    case reportType of
        ReportAcuteIllness ->
            "acute-illness"

        ReportDemographics ->
            "demographics"

        ReportNutrition ->
            "nutrition"

        ReportPrenatal ->
            "prenatal"

        ReportPrenatalDiagnoses ->
            "prenatal-diagnoses"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "acute-illness" ->
            Just ReportAcuteIllness

        "demographics" ->
            Just ReportDemographics

        "nutrition" ->
            Just ReportNutrition

        "prenatal" ->
            Just ReportPrenatal

        "prenatal-diagnoses" ->
            Just ReportPrenatalDiagnoses

        _ ->
            Nothing


countTotalEncounters : PatientData -> Int
countTotalEncounters data =
    countTotalNutritionEncounters data
        + countIndividualDataEncounters data.acuteIllnessData
        + countIndividualDataEncounters (Maybe.map (List.map .encounters) data.prenatalData)
        + countIndividualDataEncounters data.homeVisitData
        + countIndividualDataEncounters data.childScorecardData
        + countIndividualDataEncounters data.ncdData
        + countIndividualDataEncounters data.hivData
        + countIndividualDataEncounters data.tuberculosisData


countTotalNutritionEncounters : PatientData -> Int
countTotalNutritionEncounters data =
    let
        countGroupDataEncounters =
            Maybe.map List.length
                >> Maybe.withDefault 0
    in
    countIndividualDataEncounters data.wellChildData
        + countIndividualDataEncounters data.individualNutritionData
        + countGroupDataEncounters data.groupNutritionPmtctData
        + countGroupDataEncounters data.groupNutritionFbfData
        + countGroupDataEncounters data.groupNutritionSorwatheData
        + countGroupDataEncounters data.groupNutritionChwData
        + countGroupDataEncounters data.groupNutritionAchiData


countIndividualDataEncounters : Maybe (List (List a)) -> Int
countIndividualDataEncounters =
    Maybe.map (List.map List.length >> List.sum)
        >> Maybe.withDefault 0


sumNutritionMetrics : List NutritionMetrics -> NutritionMetrics
sumNutritionMetrics =
    List.foldl
        (\metrics accum ->
            { accum
                | stuntingNormal = accum.stuntingNormal ++ metrics.stuntingNormal
                , stuntingModerate = accum.stuntingModerate ++ metrics.stuntingModerate
                , stuntingSevere = accum.stuntingSevere ++ metrics.stuntingSevere
                , wastingNormal = accum.wastingNormal ++ metrics.wastingNormal
                , wastingModerate = accum.wastingModerate ++ metrics.wastingModerate
                , wastingSevere = accum.wastingSevere ++ metrics.wastingSevere
                , underweightNormal = accum.underweightNormal ++ metrics.underweightNormal
                , underweightModerate = accum.underweightModerate ++ metrics.underweightModerate
                , underweightSevere = accum.underweightSevere ++ metrics.underweightSevere
            }
        )
        emptyNutritionMetrics


nutritionEncounterDataToNutritionMetrics : PersonId -> NutritionEncounterData -> NutritionMetrics
nutritionEncounterDataToNutritionMetrics personId =
    .nutritionData
        >> Maybe.map
            (\data ->
                let
                    categorizeZScore =
                        Maybe.map
                            (\score ->
                                if score <= -3 then
                                    ( [], [], [ personId ] )

                                else if score <= -2 then
                                    ( [], [ personId ], [] )

                                else
                                    ( [ personId ], [], [] )
                            )
                            >> Maybe.withDefault ( [], [], [] )

                    ( stuntingNormal, stuntingModerate, stuntingSevere ) =
                        categorizeZScore data.stunting

                    ( wastingNormal, wastingModerate, wastingSevere ) =
                        categorizeZScore data.wasting

                    ( underweightNormal, underweightModerate, underweightSevere ) =
                        categorizeZScore data.underweight
                in
                { stuntingNormal = stuntingNormal
                , stuntingModerate = stuntingModerate
                , stuntingSevere = stuntingSevere
                , wastingNormal = wastingNormal
                , wastingModerate = wastingModerate
                , wastingSevere = wastingSevere
                , underweightNormal = underweightNormal
                , underweightModerate = underweightModerate
                , underweightSevere = underweightSevere
                }
            )
        >> Maybe.withDefault emptyNutritionMetrics


generatePrevalenceNutritionMetricsResults : NutritionMetrics -> NutritionMetricsResults
generatePrevalenceNutritionMetricsResults metrics =
    let
        calculatePercentage nominator total =
            if List.isEmpty total then
                0

            else
                (toFloat (List.length nominator) / toFloat (List.length total)) * 100

        stuntingTotal =
            metrics.stuntingModerate
                ++ metrics.stuntingSevere
                ++ metrics.stuntingNormal
                |> unique

        wastingTotal =
            metrics.wastingModerate
                ++ metrics.wastingSevere
                ++ metrics.wastingNormal
                |> unique

        underweightTotal =
            metrics.underweightModerate
                ++ metrics.underweightSevere
                ++ metrics.underweightNormal
                |> unique
    in
    { stuntingModerate = calculatePercentage metrics.stuntingModerate stuntingTotal
    , stuntingSevere = calculatePercentage metrics.stuntingSevere stuntingTotal
    , wastingModerate = calculatePercentage metrics.wastingModerate wastingTotal
    , wastingSevere = calculatePercentage metrics.wastingSevere wastingTotal
    , underweightModerate = calculatePercentage metrics.underweightModerate underweightTotal
    , underweightSevere = calculatePercentage metrics.underweightSevere underweightTotal
    }


generateIncidenceNutritionMetricsResults : NutritionMetrics -> NutritionMetrics -> NutritionMetricsResults
generateIncidenceNutritionMetricsResults currentPeriodMetric previousPeriodMetric =
    let
        calculatePercentage nominator total =
            if Set.isEmpty total then
                0

            else
                (toFloat (Set.size nominator) / toFloat (Set.size total)) * 100

        -- STUNTING
        previousPeriodStuntingModerateSevere =
            previousPeriodMetric.stuntingModerate
                ++ previousPeriodMetric.stuntingSevere
                |> unique

        previousPeriodStuntingTotal =
            previousPeriodStuntingModerateSevere
                ++ previousPeriodMetric.stuntingNormal
                |> Set.fromList

        stuntingModerateTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.stuntingModerate) previousPeriodStuntingTotal

        stuntingModerateNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.stuntingModerate) (Set.fromList previousPeriodStuntingModerateSevere)

        stuntingSevereTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.stuntingSevere) previousPeriodStuntingTotal

        stuntingSevereNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.stuntingSevere) (Set.fromList previousPeriodMetric.stuntingSevere)

        -- WASTING
        previousPeriodWastingModerateSevere =
            previousPeriodMetric.wastingModerate
                ++ previousPeriodMetric.wastingSevere
                |> unique

        previousPeriodWastingTotal =
            previousPeriodWastingModerateSevere
                ++ previousPeriodMetric.wastingNormal
                |> Set.fromList

        wastingModerateTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.wastingModerate) previousPeriodWastingTotal

        wastingModerateNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.wastingModerate) (Set.fromList previousPeriodWastingModerateSevere)

        wastingSevereTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.wastingSevere) previousPeriodWastingTotal

        wastingSevereNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.wastingSevere) (Set.fromList previousPeriodMetric.wastingSevere)

        -- UNDERWEIGHT
        previousPeriodUnderweightModerateSevere =
            previousPeriodMetric.underweightModerate
                ++ previousPeriodMetric.underweightSevere
                |> unique

        previousPeriodUnderweightTotal =
            previousPeriodUnderweightModerateSevere
                ++ previousPeriodMetric.underweightNormal
                |> Set.fromList

        underweightModerateTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.underweightModerate) previousPeriodUnderweightTotal

        underweightModerateNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.underweightModerate) (Set.fromList previousPeriodUnderweightModerateSevere)

        underweightSevereTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.underweightSevere) previousPeriodUnderweightTotal

        underweightSevereNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.underweightSevere) (Set.fromList previousPeriodMetric.underweightSevere)
    in
    { stuntingModerate =
        calculatePercentage
            (Set.intersect stuntingModerateTestedInPreviousPeriod stuntingModerateNotIdentifiedInPreviousPeriod)
            previousPeriodStuntingTotal
    , stuntingSevere =
        calculatePercentage
            (Set.intersect stuntingSevereTestedInPreviousPeriod stuntingSevereNotIdentifiedInPreviousPeriod)
            previousPeriodStuntingTotal
    , wastingModerate =
        calculatePercentage
            (Set.intersect wastingModerateTestedInPreviousPeriod wastingModerateNotIdentifiedInPreviousPeriod)
            previousPeriodWastingTotal
    , wastingSevere =
        calculatePercentage
            (Set.intersect wastingSevereTestedInPreviousPeriod wastingSevereNotIdentifiedInPreviousPeriod)
            previousPeriodWastingTotal
    , underweightModerate =
        calculatePercentage
            (Set.intersect underweightModerateTestedInPreviousPeriod underweightModerateNotIdentifiedInPreviousPeriod)
            previousPeriodUnderweightTotal
    , underweightSevere =
        calculatePercentage
            (Set.intersect underweightSevereTestedInPreviousPeriod underweightSevereNotIdentifiedInPreviousPeriod)
            previousPeriodUnderweightTotal
    }


resolveDataSetForMonth : NominalDate -> Int -> Dict ( Int, Int ) NutritionMetrics -> NutritionMetrics
resolveDataSetForMonth date monthIndex encountersByMonth =
    let
        selectedDate =
            Date.add Months (-1 * monthIndex) date

        year =
            Date.year selectedDate

        monthNumber =
            Date.monthNumber selectedDate
    in
    Dict.get ( year, monthNumber ) encountersByMonth
        |> Maybe.withDefault emptyNutritionMetrics


{-| Previous data set for given month is defined as aggregation of values taken
from 3 months that come prior to that monnths. For example, for November, it's
October, September and August.
-}
resolvePreviousDataSetForMonth : NominalDate -> Int -> Dict ( Int, Int ) NutritionMetrics -> NutritionMetrics
resolvePreviousDataSetForMonth date monthIndex encountersByMonth =
    List.range 1 3
        |> List.map
            (\gap ->
                resolveDataSetForMonth date (monthIndex + gap) encountersByMonth
            )
        |> sumNutritionMetrics


resolveDataSetForQuarter : NominalDate -> Int -> Dict ( Int, Int ) NutritionMetrics -> NutritionMetrics
resolveDataSetForQuarter date quarterIndex encountersByMonth =
    let
        selectedDate =
            Date.add Months (-3 * (quarterIndex - 1)) date

        ( year, quarter ) =
            resolvePreviousQuarterDateInfo selectedDate
    in
    quarterToMonths quarter
        |> List.map
            (\month ->
                Dict.get ( year, month ) encountersByMonth
            )
        |> Maybe.Extra.values
        |> sumNutritionMetrics


resolvePreviousQuarterDateInfo : NominalDate -> ( Int, Int )
resolvePreviousQuarterDateInfo date =
    let
        year =
            Date.year date

        quarter =
            Date.quarter date
    in
    if quarter == 1 then
        ( year - 1, 4 )

    else
        ( year, quarter - 1 )


quarterToMonths : Int -> List Int
quarterToMonths quarter =
    List.range 1 3
        |> List.map (\gap -> 3 * (quarter - 1) + gap)


resolveDataSetForYear : NominalDate -> Int -> Dict ( Int, Int ) NutritionMetrics -> NutritionMetrics
resolveDataSetForYear date yearIndex encountersByMonth =
    let
        selectedYear =
            Date.year date - yearIndex
    in
    Dict.filter
        (\( year, _ ) _ ->
            year == selectedYear
        )
        encountersByMonth
        |> Dict.values
        |> sumNutritionMetrics


isWideScope : SelectedEntity -> Bool
isWideScope selectedEntity =
    List.member selectedEntity [ EntityGlobal, EntityProvince, EntityDistrict, EntityHealthCenter ]
