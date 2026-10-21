module Pages.Reports.Utils exposing (allVaccineTypes, calculateNutritionReportData, countTotalEncounters, csvRow, eddToLmpDate, filterRecordsByDateRange, generateIncidenceNutritionMetricsResults, generatePrevalenceNutritionMetricsResults, isPostpartumEncounter, isPregnancyCompleted, isWideScope, prenatalContactTypeToEncountersAtWeek, reportTypeFromString, reportTypeToString, resolveDataSetForMonth, resolveDataSetForQuarter, resolveDataSetForYear, resolvePregnancyTrimester, resolvePreviousDataSetForMonth, resolveQuarterForIndex)

import App.Types exposing (Site(..))
import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (PersonId, SelectedEntity(..))
import Backend.Reports.Model exposing (FamilyNutritionEncounterData, NutritionEncounterData, PatientData, PrenatalEncounterData, PrenatalEncounterType(..), PrenatalParticipantData)
import Backend.Scoreboard.Model exposing (VaccineType(..))
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra exposing (unique)
import Maybe.Extra
import Pages.Reports.Model exposing (NutritionMetrics, NutritionMetricsResults, NutritionReportData, PregnancyTrimester(..), PrenatalContactType(..), ReportType(..), emptyNutritionMetrics)
import Pages.Utils exposing (launchDate)
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


isPostpartumEncounter : PrenatalEncounterData -> Bool
isPostpartumEncounter encounter =
    List.member encounter.encounterType [ NursePostpartumEncounter, ChwPostpartumEncounter ]


{-| A pregnancy is completed once it is concluded, or more than 30 days
after its EDD. For example, EDD 1 March is still active on 31 March.
-}
isPregnancyCompleted : NominalDate -> PrenatalParticipantData -> Bool
isPregnancyCompleted limitDate pregnancy =
    Maybe.Extra.isJust pregnancy.dateConcluded
        || Maybe.Extra.unwrap False (\eddDate -> Date.compare (Date.add Days 30 eddDate) limitDate == LT) pregnancy.eddDate


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
        -- familyNutritionData is mother-side. The nutrition report skips
        -- anyone six by its first year, so never sees mothers; the
        -- Demographics impacted filter, through countTotalEncounters, does.
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
            let
                -- The metric lists hold one entry per encounter, so a child
                -- measured several times during the period appears several
                -- times. Count children on both sides: deduplicating only the
                -- total inflated the percentage, and could push it past 100%.
                totalChildren =
                    unique total
            in
            if List.isEmpty totalChildren then
                0

            else
                (toFloat (List.length (unique nominator)) / toFloat (List.length totalChildren)) * 100

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
        ( year, quarter ) =
            resolveQuarterForIndex date quarterIndex
    in
    quarterToMonths quarter
        |> List.map
            (\month ->
                Dict.get ( year, month ) encountersByMonth
            )
        |> Maybe.Extra.values
        |> sumNutritionMetrics


{-| The (year, quarter) of a quarter column. Index 1 is the last complete
quarter: on 2027-01-15 it is (2026, 4), and index 2 is (2026, 3).
-}
resolveQuarterForIndex : NominalDate -> Int -> ( Int, Int )
resolveQuarterForIndex date quarterIndex =
    Date.add Months (-3 * (quarterIndex - 1)) date
        |> resolvePreviousQuarterDateInfo


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


{-| One CSV line. A cell holding a comma, quote or line break is quoted,
so `a, b` becomes `"a, b"` and stays in one column.
-}
csvRow : List String -> String
csvRow =
    let
        csvCell cell =
            if String.any (\char -> char == ',' || char == '"' || char == '\n' || char == '\u{000D}') cell then
                "\"" ++ String.replace "\"" "\"\"" cell ++ "\""

            else
                cell
    in
    List.map csvCell >> String.join ","


calculateNutritionReportData : NominalDate -> List PatientData -> NutritionReportData
calculateNutritionReportData currentDate data =
    let
        -- A visit counts only if the child was under six on that day.
        -- A child born 2019-03 keeps a 2024 visit, but not a 2025-04 one.
        records =
            -- Six by 1 January of the starting year: no visit in the report counts.
            List.filter
                (\record ->
                    Date.diff Years record.birthDate (Date.fromOrdinalDate startingYear 1) < 6
                )
                data
                |> List.map
                    (\record ->
                        let
                            underSix encounter =
                                Date.diff Years record.birthDate encounter.startDate < 6

                            keepGroupVisits =
                                Maybe.map (List.filter underSix)

                            keepIndividualVisits =
                                Maybe.map (List.map (List.filter underSix))
                        in
                        { record
                            | familyNutritionData = keepIndividualVisits record.familyNutritionData
                            , familyNutritionMuacData = keepIndividualVisits record.familyNutritionMuacData
                            , wellChildData = keepIndividualVisits record.wellChildData
                            , individualNutritionData = keepIndividualVisits record.individualNutritionData
                            , groupNutritionPmtctData = keepGroupVisits record.groupNutritionPmtctData
                            , groupNutritionFbfData = keepGroupVisits record.groupNutritionFbfData
                            , groupNutritionSorwatheData = keepGroupVisits record.groupNutritionSorwatheData
                            , groupNutritionChwData = keepGroupVisits record.groupNutritionChwData
                            , groupNutritionAchiData = keepGroupVisits record.groupNutritionAchiData
                        }
                    )

        impacted =
            List.filterMap
                (\record ->
                    if countTotalNutritionEncounters record > 1 then
                        Just record.id

                    else
                        Nothing
                )
                records

        currentYear =
            Date.year currentDate

        startingYear =
            currentYear - 3

        filterByYear encounter =
            Date.year encounter.startDate >= startingYear

        allEncounters =
            List.concatMap
                (\record ->
                    [ Maybe.map
                        (List.concat
                            >> List.filter filterByYear
                            >> List.map
                                (\item ->
                                    -- WellChildEncounterData mirrors NutritionEncounterData's
                                    -- shape (date, nutritionData, muacCm, hasEdema) plus
                                    -- immunisationData; well-child has no FBF distribution,
                                    -- so that one is absent.
                                    ( record.id
                                    , { startDate = item.startDate
                                      , nutritionData = item.nutritionData
                                      , muacCm = item.muacCm
                                      , hasEdema = item.hasEdema
                                      , fbfAmount = Nothing
                                      }
                                    )
                                )
                        )
                        record.wellChildData
                    , Maybe.map
                        (List.concat
                            >> List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.individualNutritionData
                    , Maybe.map
                        (List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.groupNutritionPmtctData
                    , Maybe.map
                        (List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.groupNutritionFbfData
                    , Maybe.map
                        (List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.groupNutritionSorwatheData
                    , Maybe.map
                        (List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.groupNutritionChwData
                    , Maybe.map
                        (List.filter filterByYear
                            >> List.map (Tuple.pair record.id)
                        )
                        record.groupNutritionAchiData
                    ]
                        |> Maybe.Extra.values
                        |> List.concat
                )
                records

        -- A child's family-nutrition MUAC visits, keyed by the child's id.
        -- The mother-side familyNutritionData is counted by the
        -- Demographics row, not here.
        familyNutritionEncounters =
            List.concatMap
                (\record ->
                    record.familyNutritionMuacData
                        |> Maybe.map
                            (List.concat
                                >> List.filter filterByYear
                                >> List.map (Tuple.pair record.id)
                            )
                        |> Maybe.withDefault []
                )
                records

        insertMetricsForMonth ( year, month ) encounterMetrics accum =
            let
                updatedMetrics =
                    Dict.get ( year, month ) accum
                        |> Maybe.map
                            (\metricsSoFar ->
                                sumNutritionMetrics [ metricsSoFar, encounterMetrics ]
                            )
                        |> Maybe.withDefault encounterMetrics
            in
            Dict.insert ( year, month ) updatedMetrics accum

        encountersByMonthFromNutrition =
            List.foldl
                (\( personId, encounter ) accum ->
                    let
                        year =
                            Date.year encounter.startDate

                        month =
                            Date.monthNumber encounter.startDate

                        encounterMetrics =
                            nutritionEncounterDataToNutritionMetrics personId encounter
                    in
                    insertMetricsForMonth ( year, month ) encounterMetrics accum
                )
                Dict.empty
                allEncounters

        encountersByMonth =
            List.foldl
                (\( personId, encounter ) accum ->
                    let
                        year =
                            Date.year encounter.startDate

                        month =
                            Date.monthNumber encounter.startDate

                        encounterMetrics =
                            familyNutritionEncounterToMetrics personId encounter
                    in
                    insertMetricsForMonth ( year, month ) encounterMetrics accum
                )
                encountersByMonthFromNutrition
                familyNutritionEncounters
    in
    { impacted = impacted
    , encountersByMonth = encountersByMonth
    }


{-| The records the date-range reports use: patients and pregnancies created
by TO, with only visits between FROM and TO. A pregnancy concluded after TO
reads as ongoing.
-}
filterRecordsByDateRange : NominalDate -> NominalDate -> NominalDate -> List PatientData -> List PatientData
filterRecordsByDateRange currentDate startDate limitDate records =
    if
        (Date.compare startDate launchDate == EQ)
            && (Date.compare limitDate currentDate == EQ)
    then
        records

    else
        List.filterMap
            (\record ->
                if
                    -- Patient was created not after the TO date.
                    not <| Date.compare record.created limitDate == GT
                then
                    let
                        filterPrenatalData =
                            Maybe.map
                                (List.filterMap
                                    (\participantData ->
                                        -- Pregnancy was created not after the TO date.
                                        if Date.compare participantData.created limitDate == GT then
                                            Nothing

                                        else
                                            let
                                                filteredEncounters =
                                                    List.filter (inRangeBy .startDate) participantData.encounters
                                            in
                                            if List.isEmpty filteredEncounters then
                                                Nothing

                                            else
                                                let
                                                    dateConcluded =
                                                        -- If pregnancy was concluded, but conclusion date is
                                                        -- after TO date, we mark pregnancy as not concluded.
                                                        Maybe.andThen
                                                            (\date ->
                                                                if Date.compare limitDate date == LT then
                                                                    Nothing

                                                                else
                                                                    Just date
                                                            )
                                                            participantData.dateConcluded
                                                in
                                                Just { participantData | dateConcluded = dateConcluded, encounters = filteredEncounters }
                                    )
                                )

                        inRangeBy resolveDateFunc encounterData =
                            let
                                encounterDate =
                                    resolveDateFunc encounterData
                            in
                            (not <| Date.compare encounterDate startDate == LT)
                                && (not <| Date.compare encounterDate limitDate == GT)

                        filterIndividualBy resolveDateFunc =
                            Maybe.map (List.map (List.filter (inRangeBy resolveDateFunc)))

                        filterGroupBy resolveDateFunc =
                            Maybe.map (List.filter (inRangeBy resolveDateFunc))
                    in
                    Just
                        { record
                            | acuteIllnessData = filterIndividualBy .startDate record.acuteIllnessData
                            , prenatalData = filterPrenatalData record.prenatalData
                            , homeVisitData = filterIndividualBy identity record.homeVisitData
                            , wellChildData = filterIndividualBy .startDate record.wellChildData
                            , childScorecardData = filterIndividualBy identity record.childScorecardData
                            , ncdData = filterIndividualBy identity record.ncdData
                            , hivData = filterIndividualBy identity record.hivData
                            , tuberculosisData = filterIndividualBy identity record.tuberculosisData
                            , individualNutritionData = filterIndividualBy .startDate record.individualNutritionData
                            , groupNutritionPmtctData = filterGroupBy .startDate record.groupNutritionPmtctData
                            , groupNutritionFbfData = filterGroupBy .startDate record.groupNutritionFbfData
                            , groupNutritionSorwatheData = filterGroupBy .startDate record.groupNutritionSorwatheData
                            , groupNutritionChwData = filterGroupBy .startDate record.groupNutritionChwData
                            , groupNutritionAchiData = filterGroupBy .startDate record.groupNutritionAchiData
                            , groupNutritionFbfMotherData = filterGroupBy .startDate record.groupNutritionFbfMotherData
                            , familyNutritionData = filterIndividualBy .startDate record.familyNutritionData
                            , familyNutritionMuacData = filterIndividualBy .startDate record.familyNutritionMuacData
                        }

                else
                    Nothing
            )
            records
