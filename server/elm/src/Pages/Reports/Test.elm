module Pages.Reports.Test exposing (all)

import AssocList as Dict
import Backend.Components.Model exposing (PersonId)
import Backend.Reports.Model exposing (Gender(..), NutritionEncounterData, PatientData)
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (NutritionReportData)
import Pages.Reports.Utils exposing (calculateNutritionReportData)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


{-| The report is calculated on 2026-10-20, so it shows visits from
1 January 2023 on.
-}
reportDate : NominalDate
reportDate =
    Date.fromCalendarDate 2026 Oct 20


{-| A person with no visits of any kind.
-}
emptyPatient : PersonId -> NominalDate -> PatientData
emptyPatient id birthDate =
    { id = id
    , created = birthDate
    , birthDate = birthDate
    , gender = Male
    , acuteIllnessData = Nothing
    , prenatalData = Nothing
    , familyNutritionData = Nothing
    , familyNutritionMuacData = Nothing
    , homeVisitData = Nothing
    , wellChildData = Nothing
    , childScorecardData = Nothing
    , ncdData = Nothing
    , hivData = Nothing
    , tuberculosisData = Nothing
    , individualNutritionData = Nothing
    , groupNutritionPmtctData = Nothing
    , groupNutritionFbfData = Nothing
    , groupNutritionSorwatheData = Nothing
    , groupNutritionChwData = Nothing
    , groupNutritionAchiData = Nothing
    , groupNutritionFbfMotherData = Nothing
    }


{-| A CHW group visit whose stunting z-score is moderate, so the visit
shows up in the stuntingModerate bucket of its month.
-}
stuntedVisit : NominalDate -> NutritionEncounterData
stuntedVisit date =
    { startDate = date
    , nutritionData = Just { stunting = Just -2.5, underweight = Nothing, wasting = Nothing }
    , muacCm = Nothing
    , hasEdema = False
    , fbfAmount = Nothing
    }


childWithGroupVisits : PersonId -> NominalDate -> List NominalDate -> PatientData
childWithGroupVisits id birthDate dates =
    let
        patient =
            emptyPatient id birthDate
    in
    { patient | groupNutritionChwData = Just (List.map stuntedVisit dates) }


{-| Every (year, month, person) the report counted, from the stunting
and acute malnutrition buckets.
-}
countedVisits : NutritionReportData -> List ( ( Int, Int ), PersonId )
countedVisits report =
    Dict.toList report.encountersByMonth
        |> List.concatMap
            (\( yearMonth, metrics ) ->
                List.map (Tuple.pair yearMonth) (metrics.stuntingModerate ++ metrics.acuteMalnutritionMam)
            )
        |> List.sortBy (\( ( year, month ), id ) -> ( year, month, id ))


calculate : List PatientData -> NutritionReportData
calculate =
    calculateNutritionReportData reportDate


calculateNutritionReportDataTest : Test
calculateNutritionReportDataTest =
    describe "calculateNutritionReportData counts a visit only if the child was under six on that day"
        [ test "a child who is six today keeps the visits made before turning six" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Mar 1) [ Date.fromCalendarDate 2024 May 2, Date.fromCalendarDate 2025 Apr 2 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2024, 5 ), 1 ) ]
        , test "a visit the day before the sixth birthday counts, one on the birthday does not" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Jun 15) [ Date.fromCalendarDate 2025 Jun 14, Date.fromCalendarDate 2025 Jun 15 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2025, 6 ), 1 ) ]
        , test "a child born on 29 February is six on 1 March" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2020 Feb 29) [ Date.fromCalendarDate 2026 Feb 28, Date.fromCalendarDate 2026 Mar 1 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2026, 2 ), 1 ) ]
        , test "a child under six today keeps every visit and counts as impacted" <|
            \_ ->
                let
                    report =
                        calculate [ childWithGroupVisits 1 (Date.fromCalendarDate 2022 Jan 10) [ Date.fromCalendarDate 2025 Nov 6, Date.fromCalendarDate 2026 Mar 3 ] ]
                in
                Expect.equal ( [ ( ( 2025, 11 ), 1 ), ( ( 2026, 3 ), 1 ) ], [ 1 ] )
                    ( countedVisits report, report.impacted )
        , test "impacted counts only the visits made before six" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Mar 1) [ Date.fromCalendarDate 2025 Feb 15, Date.fromCalendarDate 2025 Mar 15 ] ]
                    |> calculate
                    |> .impacted
                    |> Expect.equal []
        , test "a family-nutrition MUAC visit counts only before six" <|
            \_ ->
                let
                    patient =
                        emptyPatient 1 (Date.fromCalendarDate 2019 Mar 1)

                    muacVisit date =
                        { startDate = date, muacCm = Just 12, ahezaAmount = Nothing }
                in
                [ { patient | familyNutritionMuacData = Just [ [ muacVisit (Date.fromCalendarDate 2024 Jul 1), muacVisit (Date.fromCalendarDate 2025 Jul 1) ] ] } ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2024, 7 ), 1 ) ]
        , test "someone six by 1 January of the starting year adds nothing" <|
            \_ ->
                -- Child 1 made two visits before six, both before 2023: only
                -- the starting-year skip keeps the child out of impacted.
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2016 Jun 1) [ Date.fromCalendarDate 2021 Mar 5, Date.fromCalendarDate 2022 Mar 5 ]
                , childWithGroupVisits 2 (Date.fromCalendarDate 1990 Jan 1) [ Date.fromCalendarDate 2025 Nov 3 ]
                ]
                    |> calculate
                    |> Expect.equal { impacted = [], encountersByMonth = Dict.empty }
        ]


all : Test
all =
    describe "Pages.Reports"
        [ calculateNutritionReportDataTest ]
