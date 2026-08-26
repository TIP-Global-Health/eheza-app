module Pages.Dashboard.Test exposing (all)

import AssocList as Dict
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw)
import Backend.Dashboard.Encoder exposing (encodeDashboardStatsRaw)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, DashboardStatsRaw, NutritionStatus(..), NutritionValue, PersonIdentifier, PrenatalDataItem, PrenatalEncounterDataItem, SPVDataItem, SPVEncounterDataItem)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), WellChildEncounterType(..))
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode
import Json.Encode
import Pages.Dashboard.Model exposing (ECDStatus(..))
import Pages.Dashboard.Utils exposing (caseManagementMergeDuplicates, countChildrenSeenForSelectedMonth, countCurrentlyPregnantForSelectedMonth, dataItemMergeDuplicates, resolveECDStatus)
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


emptyCaseNutrition : CaseNutrition
emptyCaseNutrition =
    { stunting = Dict.empty
    , underweight = Dict.empty
    , wasting = Dict.empty
    , muac = Dict.empty
    , nutritionSigns = Dict.empty
    }


nutritionValue : NutritionValue
nutritionValue =
    { class = Moderate, value = "1.0" }


{-| A case-management record for person `identifier`, holding a single stunting
measurement for the given `month`.
-}
caseWithStuntingMonth : Int -> Int -> CaseManagement
caseWithStuntingMonth identifier month =
    { identifier = identifier
    , birthDate = Date.fromCalendarDate 2018 Time.Jan 1
    , gender = Female
    , nutrition = { emptyCaseNutrition | stunting = Dict.singleton month nutritionValue }
    }


caseManagementMergeDuplicatesTest : Test
caseManagementMergeDuplicatesTest =
    describe "caseManagementMergeDuplicates"
        [ test "merging two records for the SAME person keeps BOTH persons' nutrition months" <|
            \_ ->
                -- The same child enrolled across programs appears as two records
                -- sharing one identifier, each carrying different months of
                -- nutrition data. The merge must combine them, not discard one.
                let
                    merged =
                        caseManagementMergeDuplicates
                            [ caseWithStuntingMonth 1 1
                            , caseWithStuntingMonth 1 2
                            ]
                in
                case merged of
                    [ person ] ->
                        Dict.keys person.nutrition.stunting
                            |> List.sort
                            |> Expect.equal [ 1, 2 ]

                    _ ->
                        Expect.fail "expected the two same-identifier records to merge into exactly one"
        , test "records with DIFFERENT identifiers are not merged" <|
            \_ ->
                caseManagementMergeDuplicates
                    [ caseWithStuntingMonth 1 1
                    , caseWithStuntingMonth 2 3
                    ]
                    |> List.length
                    |> Expect.equal 2
        ]


generatedOn : NominalDate
generatedOn =
    Date.fromCalendarDate 2026 Time.Jul 15


statsWithGenerationDate : DashboardStatsRaw
statsWithGenerationDate =
    { caseManagement = { thisYear = Dict.empty, lastYear = Dict.empty }
    , childrenBeneficiaries = Dict.empty
    , completedPrograms = []
    , familyPlanning = []
    , missedSessions = []
    , totalEncounters = { global = Dict.empty, villages = Dict.empty }
    , acuteIllnessData = []
    , prenatalData = []
    , ncdData = []
    , pmtctData = []
    , spvData = []
    , childScoreboardData = []
    , nutritionIndividualData = []
    , nutritionGroupData = []
    , groupEducationData = Dict.empty
    , villagesWithResidents = Dict.empty
    , patientsDetails = Dict.empty
    , timestamp = "15-07-2026, 09:49"
    , statsGeneratedDate = Just generatedOn
    , cacheHash = "hash"
    }


statsStorageRoundTripTest : Test
statsStorageRoundTripTest =
    describe "encodeDashboardStatsRaw"
        [ test "keeps the generation date, which the monthly figures are read against" <|
            \_ ->
                -- Downloaded statistics are re-encoded through this function
                -- before being stored, and decoded again on the next start. A
                -- field the encoder drops is gone from then on.
                statsWithGenerationDate
                    |> encodeDashboardStatsRaw
                    |> Json.Encode.object
                    |> Json.Decode.decodeValue decodeDashboardStatsRaw
                    |> Result.map .statsGeneratedDate
                    |> Expect.equal (Ok (Just generatedOn))
        ]


{-| A nurse (`PediatricCare`) encounter on `startDate`, carrying `warnings`.
-}
spvEncounterOn : NominalDate -> List EncounterWarning -> SPVEncounterDataItem
spvEncounterOn startDate warnings =
    { startDate = startDate
    , encounterType = PediatricCare
    , warnings = EverySet.fromList warnings
    , zscoreStunting = Nothing
    , zscoreUnderweight = Nothing
    , zscoreWasting = Nothing
    , muac = Nothing
    , nutritionSigns = EverySet.empty
    , bcgImminizationDates = EverySet.empty
    , opvImminizationDates = EverySet.empty
    , dtpImminizationDates = EverySet.empty
    , dtpStandaloneImminizationDates = EverySet.empty
    , pcv13ImminizationDates = EverySet.empty
    , rotarixImminizationDates = EverySet.empty
    , ipvImminizationDates = EverySet.empty
    , mrImminizationDates = EverySet.empty
    , hpvImminizationDates = EverySet.empty
    }


spvEncounter : Int -> List EncounterWarning -> SPVEncounterDataItem
spvEncounter dayOfJuly =
    spvEncounterOn (Date.fromCalendarDate 2026 Time.Jul dayOfJuly)


{-| One well-child individual participant for the child with `identifier`. The
same child can hold more than one, which is why the identifier is a parameter.
-}
day : Int -> NominalDate
day n =
    Date.fromCalendarDate 2026 Time.Jul n


childSeenAt : PersonIdentifier -> List SPVEncounterDataItem -> SPVDataItem
childSeenAt identifier encounters =
    { identifier = identifier
    , created = Date.fromCalendarDate 2026 Time.Jan 1
    , birthDate = Date.fromCalendarDate 2025 Time.Jan 1
    , gender = Female
    , encounters = encounters
    }


resolveECDStatusTest : Test
resolveECDStatusTest =
    let
        endOfJuly =
            Date.fromCalendarDate 2026 Time.Jul 31

        resolve =
            childSeenAt 1 >> resolveECDStatus endOfJuly
    in
    describe "resolveECDStatus"
        [ test "a child whose latest verdict is on track is on track" <|
            \_ ->
                resolve [ spvEncounter 10 [ NoECDMilstoneWarning ] ]
                    |> Expect.equal (Just ECDOnTrack)
        , test "a child whose latest verdict is behind is behind" <|
            \_ ->
                resolve [ spvEncounter 10 [ WarningECDMilestoneBehind ] ]
                    |> Expect.equal (Just ECDBehind)
        , test "a child seen but never assessed is not assessed, not behind" <|
            \_ ->
                -- The ECD activity is only offered while milestones are still
                -- outstanding, so a child who has completed them all - or was
                -- seen for something else entirely - carries no ECD verdict.
                -- Counting that as behind is what this report used to do.
                resolve [ spvEncounter 10 [ NoEncounterWarnings ] ]
                    |> Expect.equal (Just ECDNotAssessed)
        , test "a later encounter with no verdict does not erase the verdict before it" <|
            \_ ->
                resolve
                    [ spvEncounter 10 [ NoECDMilstoneWarning ]
                    , spvEncounter 20 [ NoHeadCircumferenceWarning ]
                    ]
                    |> Expect.equal (Just ECDOnTrack)
        , test "the most recent verdict wins" <|
            \_ ->
                resolve
                    [ spvEncounter 10 [ NoECDMilstoneWarning ]
                    , spvEncounter 20 [ WarningECDMilestoneReferToSpecialist ]
                    ]
                    |> Expect.equal (Just ECDBehind)
        , test "encounters after the selected month are not read" <|
            \_ ->
                resolve
                    [ spvEncounter 10 [ NoECDMilstoneWarning ]
                    , spvEncounterOn (Date.fromCalendarDate 2026 Time.Aug 5) [ WarningECDMilestoneBehind ]
                    ]
                    |> Expect.equal (Just ECDOnTrack)
        , test "a child with no nurse encounter is not part of the report at all" <|
            \_ ->
                let
                    nurseEncounter =
                        spvEncounter 10 [ NoECDMilstoneWarning ]
                in
                resolve [ { nurseEncounter | encounterType = NewbornExam } ]
                    |> Expect.equal Nothing
        ]


countChildrenSeenForSelectedMonthTest : Test
countChildrenSeenForSelectedMonthTest =
    let
        endOfJuly =
            Date.fromCalendarDate 2026 Time.Jul 31

        count =
            countChildrenSeenForSelectedMonth endOfJuly
    in
    describe "countChildrenSeenForSelectedMonth"
        [ test "a child seen twice in the month counts once" <|
            \_ ->
                count
                    [ childSeenAt 1
                        [ spvEncounter 3 [ NoECDMilstoneWarning ]
                        , spvEncounter 24 [ NoECDMilstoneWarning ]
                        ]
                    ]
                    |> Expect.equal 1
        , test "two children seen count twice" <|
            \_ ->
                count
                    [ childSeenAt 1 [ spvEncounter 3 [ NoECDMilstoneWarning ] ]
                    , childSeenAt 2 [ spvEncounter 24 [ NoECDMilstoneWarning ] ]
                    ]
                    |> Expect.equal 2
        , test "one child holding two participants counts once" <|
            \_ ->
                -- An item is an individual participant, not a child, and the
                -- same child can hold more than one.
                count
                    [ childSeenAt 1 [ spvEncounter 3 [ NoECDMilstoneWarning ] ]
                    , childSeenAt 1 [ spvEncounter 24 [ NoECDMilstoneWarning ] ]
                    ]
                    |> Expect.equal 1
        , test "a child seen only in another month is not counted" <|
            \_ ->
                count
                    [ childSeenAt 1
                        [ spvEncounterOn (Date.fromCalendarDate 2026 Time.Jun 20) [ NoECDMilstoneWarning ] ]
                    ]
                    |> Expect.equal 0
        , test "a child seen only by a CHW is not counted" <|
            \_ ->
                let
                    nurseEncounter =
                        spvEncounter 10 [ NoECDMilstoneWarning ]
                in
                count [ childSeenAt 1 [ { nurseEncounter | encounterType = NewbornExam } ] ]
                    |> Expect.equal 0
        ]


{-| A pregnancy registered on `created`, still open, expected to conclude well
after the month being reported on, with a single nurse encounter.
-}
openPregnancyRegisteredOn : NominalDate -> PrenatalDataItem
openPregnancyRegisteredOn created =
    { identifier = 1
    , created = created
    , expectedDateConcluded = Just (Date.fromCalendarDate 2026 Time.Dec 1)
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , encounters = [ prenatalEncounterOn (Date.fromCalendarDate 2026 Time.Apr 1) ]
    }


prenatalEncounterOn : NominalDate -> PrenatalEncounterDataItem
prenatalEncounterOn startDate =
    { startDate = startDate
    , encounterType = NurseEncounter
    , dangerSigns = EverySet.empty
    , diagnoses = EverySet.empty
    , muac = Nothing
    , sendToHCSigns = EverySet.empty
    }


countCurrentlyPregnantForSelectedMonthTest : Test
countCurrentlyPregnantForSelectedMonthTest =
    let
        endOfJuly =
            Date.fromCalendarDate 2026 Time.Jul 31

        count =
            countCurrentlyPregnantForSelectedMonth endOfJuly False
    in
    describe "countCurrentlyPregnantForSelectedMonth"
        [ test "a pregnancy registered months earlier is still counted" <|
            \_ ->
                -- A woman is counted for every month her pregnancy is open,
                -- not only for the month she was registered in. Reading the
                -- registration date the other way round leaves the card
                -- showing the few women registered on the reference day.
                count [ openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10) ]
                    |> Expect.equal 1
        , test "a pregnancy registered within the month is counted" <|
            \_ ->
                count [ openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Jul 20) ]
                    |> Expect.equal 1
        , test "a pregnancy registered after the month is not counted" <|
            \_ ->
                count [ openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Aug 3) ]
                    |> Expect.equal 0
        , test "an encounter after the month does not place the woman at the facility" <|
            \_ ->
                -- A woman counts for a facility once it has seen her. Reading
                -- her whole encounter list would let a later visit change a
                -- month that has already been reported.
                let
                    pregnancy =
                        openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10)
                in
                count
                    [ { pregnancy | encounters = [ prenatalEncounterOn (Date.fromCalendarDate 2026 Time.Sep 15) ] } ]
                    |> Expect.equal 0
        , test "the CHW count reads the same encounters" <|
            \_ ->
                let
                    pregnancy =
                        openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10)

                    chwEncounter =
                        prenatalEncounterOn (Date.fromCalendarDate 2026 Time.Sep 15)
                in
                countCurrentlyPregnantForSelectedMonth endOfJuly
                    True
                    [ { pregnancy | encounters = [ { chwEncounter | encounterType = ChwFirstEncounter } ] } ]
                    |> Expect.equal 0
        , test "a woman seen only by a CHW is not counted at the health centre" <|
            \_ ->
                let
                    pregnancy =
                        openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10)

                    chwEncounter =
                        prenatalEncounterOn (Date.fromCalendarDate 2026 Time.Apr 1)
                in
                count [ { pregnancy | encounters = [ { chwEncounter | encounterType = ChwFirstEncounter } ] } ]
                    |> Expect.equal 0
        ]


dataItemMergeDuplicatesTest : Test
dataItemMergeDuplicatesTest =
    describe "dataItemMergeDuplicates"
        [ test "a child registered into two participants becomes one item holding both sets of encounters" <|
            \_ ->
                -- The statistics feed builds an item per individual
                -- participant, so keeping one of the two would drop the
                -- encounters recorded against the other.
                dataItemMergeDuplicates
                    [ childSeenAt 1 [ spvEncounter 3 [ NoECDMilstoneWarning ] ]
                    , childSeenAt 1 [ spvEncounter 24 [ WarningECDMilestoneBehind ] ]
                    ]
                    |> List.map (.encounters >> List.length)
                    |> Expect.equal [ 2 ]
        , test "a child-scoreboard item is merged the same way" <|
            \_ ->
                -- The helper is shared with the child-scoreboard feed, whose
                -- items carry a different encounter type.
                dataItemMergeDuplicates
                    [ { identifier = 1, created = day 1, birthDate = day 1, gender = Female, encounters = [ "a" ] }
                    , { identifier = 1, created = day 1, birthDate = day 1, gender = Female, encounters = [ "b" ] }
                    ]
                    |> List.map .encounters
                    |> Expect.equal [ [ "a", "b" ] ]
        , test "distinct children are left alone" <|
            \_ ->
                dataItemMergeDuplicates
                    [ childSeenAt 1 [ spvEncounter 3 [ NoECDMilstoneWarning ] ]
                    , childSeenAt 2 [ spvEncounter 24 [ NoECDMilstoneWarning ] ]
                    ]
                    |> List.map .identifier
                    |> List.sort
                    |> Expect.equal [ 1, 2 ]
        , test "a verdict recorded against the earlier participant still decides the status" <|
            \_ ->
                -- Without the merge the surviving item can be the one that
                -- never carried an ECD verdict, and the child reads as
                -- never assessed.
                dataItemMergeDuplicates
                    [ childSeenAt 1 [ spvEncounter 3 [ NoECDMilstoneWarning ] ]
                    , childSeenAt 1 [ spvEncounter 24 [ NoEncounterWarnings ] ]
                    ]
                    |> List.filterMap (resolveECDStatus (Date.fromCalendarDate 2026 Time.Jul 31))
                    |> Expect.equal [ ECDOnTrack ]
        ]


all : Test
all =
    describe "Pages.Dashboard.Utils"
        [ caseManagementMergeDuplicatesTest
        , countChildrenSeenForSelectedMonthTest
        , countCurrentlyPregnantForSelectedMonthTest
        , resolveECDStatusTest
        , dataItemMergeDuplicatesTest
        , statsStorageRoundTripTest
        ]
