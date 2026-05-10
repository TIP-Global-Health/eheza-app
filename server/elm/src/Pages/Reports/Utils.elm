module Pages.Reports.Utils exposing (allVaccineTypes, countTotalEncounters, countTotalNutritionEncounters, eddToLmpDate, familyNutritionEncounterToMetrics, generateIncidenceNutritionMetricsResults, generatePrevalenceNutritionMetricsResults, isWideScope, nutritionEncounterDataToNutritionMetrics, prenatalContactTypeToEncountersAtWeek, reportTypeFromString, reportTypeToString, resolveDataSetForMonth, resolveDataSetForQuarter, resolveDataSetForYear, resolvePregnancyTrimester, resolvePreviousDataSetForMonth, sumNutritionMetrics, visibleReportTypes)

import App.Types exposing (Site(..), SiteFeature(..))
import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (PersonId, SelectedEntity(..))
import Backend.Reports.Model exposing (FamilyNutritionEncounterData, NutritionEncounterData, PatientData)
import Backend.Scoreboard.Model exposing (VaccineType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra exposing (unique)
import Maybe.Extra
import Pages.Reports.Model exposing (NutritionMetrics, NutritionMetricsResults, PregnancyTrimester(..), PrenatalContactType(..), ReportType(..), emptyNutritionMetrics)
import Set


prenatalContactTypeToEncountersAtWeek : PrenatalContactType -> ( Int, Int )
prenatalContactTypeToEncountersAtWeek prenatalContactType =
    case prenatalContactType of
        PrenatalContact1 ->
            ( 1, 12 )

        PrenatalContact2 ->
            ( 2, 20 )

        PrenatalContact3 ->
            ( 3, 26 )

        PrenatalContact4 ->
            ( 4, 30 )

        PrenatalContact5 ->
            ( 5, 34 )

        PrenatalContact6 ->
            ( 6, 36 )

        PrenatalContact7 ->
            ( 7, 38 )

        PrenatalContact8 ->
            ( 8, 40 )


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

        ReportFBFDistribution ->
            "fbf-distribution"

        ReportNutrition ->
            "nutrition"

        ReportPeripartum ->
            "peripartum"

        ReportPostnatalCare ->
            "postnatal-care"

        ReportPrenatal ->
            "prenatal"

        ReportPrenatalContacts ->
            "prenatal-contacts"

        ReportPrenatalDiagnoses ->
            "prenatal-diagnoses"


reportTypeFromString : String -> Maybe ReportType
reportTypeFromString reportType =
    case reportType of
        "acute-illness" ->
            Just ReportAcuteIllness

        "demographics" ->
            Just ReportDemographics

        "fbf-distribution" ->
            Just ReportFBFDistribution

        "nutrition" ->
            Just ReportNutrition

        "peripartum" ->
            Just ReportPeripartum

        "postnatal-care" ->
            Just ReportPostnatalCare

        "prenatal" ->
            Just ReportPrenatal

        "prenatal-contacts" ->
            Just ReportPrenatalContacts

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
        -- familyNutritionData (date-only, mother-side) is always 0 for
        -- children (the nutrition report filters to under-6), but this
        -- function is also called via countTotalEncounters for the
        -- Demographics impacted filter, which iterates mothers.
        + countIndividualDataEncounters data.familyNutritionData
        + countIndividualDataEncounters data.familyNutritionMuacData
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
                , acuteMalnutritionNormal = accum.acuteMalnutritionNormal ++ metrics.acuteMalnutritionNormal
                , acuteMalnutritionMam = accum.acuteMalnutritionMam ++ metrics.acuteMalnutritionMam
                , acuteMalnutritionSam = accum.acuteMalnutritionSam ++ metrics.acuteMalnutritionSam
            }
        )
        emptyNutritionMetrics


categorizeAcuteMalnutrition : PersonId -> Maybe Float -> Bool -> ( List PersonId, List PersonId, List PersonId )
categorizeAcuteMalnutrition personId mMuacCm hasEdema =
    case mMuacCm of
        Nothing ->
            ( [], [], [] )

        Just muacCm ->
            if muacCm < 11.5 || hasEdema then
                ( [], [], [ personId ] )

            else if muacCm < 12.5 then
                ( [], [ personId ], [] )

            else
                ( [ personId ], [], [] )


nutritionEncounterDataToNutritionMetrics : PersonId -> NutritionEncounterData -> NutritionMetrics
nutritionEncounterDataToNutritionMetrics personId encounter =
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
            encounter.nutritionData
                |> Maybe.map (.stunting >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( wastingNormal, wastingModerate, wastingSevere ) =
            encounter.nutritionData
                |> Maybe.map (.wasting >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( underweightNormal, underweightModerate, underweightSevere ) =
            encounter.nutritionData
                |> Maybe.map (.underweight >> categorizeZScore)
                |> Maybe.withDefault ( [], [], [] )

        ( acuteMalnutritionNormal, acuteMalnutritionMam, acuteMalnutritionSam ) =
            categorizeAcuteMalnutrition personId encounter.muacCm encounter.hasEdema
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
    , acuteMalnutritionNormal = acuteMalnutritionNormal
    , acuteMalnutritionMam = acuteMalnutritionMam
    , acuteMalnutritionSam = acuteMalnutritionSam
    }


familyNutritionEncounterToMetrics : PersonId -> FamilyNutritionEncounterData -> NutritionMetrics
familyNutritionEncounterToMetrics personId encounter =
    let
        ( acuteMalnutritionNormal, acuteMalnutritionMam, acuteMalnutritionSam ) =
            categorizeAcuteMalnutrition personId encounter.muacCm False
    in
    { emptyNutritionMetrics
        | acuteMalnutritionNormal = acuteMalnutritionNormal
        , acuteMalnutritionMam = acuteMalnutritionMam
        , acuteMalnutritionSam = acuteMalnutritionSam
    }


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

        acuteMalnutritionTotal =
            metrics.acuteMalnutritionMam
                ++ metrics.acuteMalnutritionSam
                ++ metrics.acuteMalnutritionNormal
                |> unique
    in
    { stuntingModerate = calculatePercentage metrics.stuntingModerate stuntingTotal
    , stuntingSevere = calculatePercentage metrics.stuntingSevere stuntingTotal
    , wastingModerate = calculatePercentage metrics.wastingModerate wastingTotal
    , wastingSevere = calculatePercentage metrics.wastingSevere wastingTotal
    , underweightModerate = calculatePercentage metrics.underweightModerate underweightTotal
    , underweightSevere = calculatePercentage metrics.underweightSevere underweightTotal
    , acuteMalnutritionMam = calculatePercentage metrics.acuteMalnutritionMam acuteMalnutritionTotal
    , acuteMalnutritionSam = calculatePercentage metrics.acuteMalnutritionSam acuteMalnutritionTotal
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

        -- ACUTE MALNUTRITION
        previousPeriodAcuteMalnutritionMamSam =
            previousPeriodMetric.acuteMalnutritionMam
                ++ previousPeriodMetric.acuteMalnutritionSam
                |> unique

        previousPeriodAcuteMalnutritionTotal =
            previousPeriodAcuteMalnutritionMamSam
                ++ previousPeriodMetric.acuteMalnutritionNormal
                |> Set.fromList

        acuteMalnutritionMamTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.acuteMalnutritionMam) previousPeriodAcuteMalnutritionTotal

        acuteMalnutritionMamNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.acuteMalnutritionMam) (Set.fromList previousPeriodAcuteMalnutritionMamSam)

        acuteMalnutritionSamTestedInPreviousPeriod =
            Set.intersect (Set.fromList currentPeriodMetric.acuteMalnutritionSam) previousPeriodAcuteMalnutritionTotal

        acuteMalnutritionSamNotIdentifiedInPreviousPeriod =
            Set.diff (Set.fromList currentPeriodMetric.acuteMalnutritionSam) (Set.fromList previousPeriodMetric.acuteMalnutritionSam)
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
    , acuteMalnutritionMam =
        calculatePercentage
            (Set.intersect acuteMalnutritionMamTestedInPreviousPeriod acuteMalnutritionMamNotIdentifiedInPreviousPeriod)
            previousPeriodAcuteMalnutritionTotal
    , acuteMalnutritionSam =
        calculatePercentage
            (Set.intersect acuteMalnutritionSamTestedInPreviousPeriod acuteMalnutritionSamNotIdentifiedInPreviousPeriod)
            previousPeriodAcuteMalnutritionTotal
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


{-| Vaccines scheduled for a given site. Mirrors the client-side
`Measurement.Utils.allVaccineTypes`: only Burundi schedules
`VaccineDTPStandalone`; other sites schedule `VaccineHPV` instead.
-}
allVaccineTypes : Site -> List VaccineType
allVaccineTypes site =
    let
        common =
            [ VaccineBCG
            , VaccineOPV
            , VaccineDTP
            , VaccinePCV13
            , VaccineRotarix
            , VaccineIPV
            , VaccineMR
            ]
    in
    case site of
        SiteBurundi ->
            common ++ [ VaccineDTPStandalone ]

        _ ->
            common ++ [ VaccineHPV ]


{-| Filter the Statistical Queries report-type list down to the entries that
have a meaningful data domain on this deployment. Reports whose sole data
source is feature-gated drop out when that feature is disabled.

`ReportDemographics` and `ReportFBFDistribution` always show — they are
multi-source and apply row-level filters internally.

`ReportNutrition` shows when at least one of its four contributing sources
is enabled.

-}
visibleReportTypes : EverySet SiteFeature -> List ReportType
visibleReportTypes features =
    let
        member f =
            EverySet.member f features
    in
    List.filter
        (\reportType ->
            case reportType of
                ReportAcuteIllness ->
                    member FeatureAcuteIllness

                ReportDemographics ->
                    True

                ReportFBFDistribution ->
                    True

                ReportNutrition ->
                    member FeatureWellChild
                        || member FeatureNutritionIndividual
                        || member FeatureNutritionGroup
                        || member FeatureFamilyNutrition

                ReportPeripartum ->
                    member FeatureAntenatal

                ReportPostnatalCare ->
                    member FeatureWellChild

                ReportPrenatal ->
                    member FeatureAntenatal

                ReportPrenatalContacts ->
                    member FeatureAntenatal

                ReportPrenatalDiagnoses ->
                    member FeatureAntenatal
        )
        [ ReportAcuteIllness
        , ReportDemographics
        , ReportFBFDistribution
        , ReportNutrition
        , ReportPeripartum
        , ReportPostnatalCare
        , ReportPrenatal
        , ReportPrenatalContacts
        , ReportPrenatalDiagnoses
        ]
