module Pages.Dashboard.Test exposing (all)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..))
import Backend.Dashboard.Decoder exposing (decodeDashboardStatsRaw, decodePrenatalDataItem)
import Backend.Dashboard.Encoder exposing (encodeDashboardStatsRaw)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, DashboardStatsRaw, NutritionStatus(..), NutritionValue, PersonIdentifier, PrenatalDataItem, PrenatalEncounterDataItem, SPVDataItem, SPVEncounterDataItem)
import Backend.IndividualEncounterParticipant.Model exposing (DeliveryLocation(..), IndividualEncounterParticipantOutcome(..), PregnancyOutcome(..))
import Backend.Measurement.Model exposing (DangerSign(..), FollowUpOption(..), Gender(..), PrenatalAssesment(..), ReferToFacilitySign(..), SendToHCSign(..))
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), emptyPrenatalEncounter)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), WellChildEncounterType(..))
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode
import Json.Encode
import Pages.Dashboard.Model exposing (ECDStatus(..))
import Pages.Dashboard.Utils exposing (caseManagementMergeDuplicates, countChildrenSeenForSelectedMonth, countCurrentlyPregnantForSelectedMonth, countCurrentlyPregnantWithDangerSignsForSelectedMonth, countHospitalReferralsForSelectedMonth, dataItemMergeDuplicates, getFollowUpsTotals, previousThreeMonthsNewestFirst, resolveDayAfterSelectedDate, resolveECDStatus)
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)
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
    , referToFacilitySigns = EverySet.empty
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


{-| The Case Management cards count the follow ups that were open on the
selected date: a past month's last day, or today for the current month.
-}
followUpsTotalsTest : Test
followUpsTotalsTest =
    let
        date =
            Date.fromCalendarDate 2026

        villageId =
            toEntityUuid "village"

        personId =
            toEntityUuid "person"

        illnessId =
            toEntityUuid "illness"

        pregnancyId =
            toEntityUuid "pregnancy"

        augustVisit =
            toEntityUuid "august-visit"

        septemberVisit =
            toEntityUuid "september-visit"

        acuteIllnessEncounters =
            Dict.fromList
                [ ( augustVisit, emptyAcuteIllnessEncounter illnessId (date Time.Aug 20) 1 AcuteIllnessEncounterCHW Nothing )
                , ( septemberVisit, emptyAcuteIllnessEncounter illnessId (date Time.Sep 10) 2 AcuteIllnessEncounterCHW Nothing )
                ]

        prenatalEncounters =
            Dict.fromList
                [ ( augustVisit, emptyPrenatalEncounter pregnancyId (date Time.Aug 20) ChwFirstEncounter Nothing )
                , ( septemberVisit, emptyPrenatalEncounter pregnancyId (date Time.Sep 10) ChwSecondEncounter Nothing )
                ]

        db =
            { emptyModelIndexedDb
                | people = Dict.singleton personId (RemoteData.Success testPerson)
                , peopleInVillage =
                    Dict.singleton villageId
                        (RemoteData.Success (Dict.singleton personId testPerson))
                , acuteIllnessEncounters = Dict.map (\_ encounter -> RemoteData.Success encounter) acuteIllnessEncounters
                , acuteIllnessEncountersByParticipant = Dict.singleton illnessId (RemoteData.Success acuteIllnessEncounters)
                , prenatalEncounters = Dict.map (\_ encounter -> RemoteData.Success encounter) prenatalEncounters
                , prenatalEncountersByParticipant = Dict.singleton pregnancyId (RemoteData.Success prenatalEncounters)
            }

        measurement id dateMeasured encounterId value =
            ( toEntityUuid id
            , { dateMeasured = dateMeasured
              , nurse = Nothing
              , healthCenter = Nothing
              , participantId = personId
              , deleted = False
              , encounterId = encounterId
              , value = value
              }
            )

        nutritionFollowUp id dateMeasured option =
            measurement id
                dateMeasured
                Nothing
                { options = EverySet.singleton option
                , resolutionDate = Nothing
                , assesment = EverySet.empty
                }

        acuteIllnessFollowUp id dateMeasured encounterId =
            measurement id
                dateMeasured
                (Just encounterId)
                { options = EverySet.singleton OneWeek
                , resolutionDate = Nothing
                , diagnosis = Just DiagnosisMalariaUncomplicated
                }

        prenatalFollowUp id dateMeasured encounterId =
            measurement id
                dateMeasured
                (Just encounterId)
                { options = EverySet.singleton OneWeek
                , resolutionDate = Nothing
                , assesment = AssesmentNormalPregnancy
                }

        noFollowUps =
            { nutritionGroup = Dict.empty
            , nutritionIndividual = Dict.empty
            , acuteIllness = Dict.empty
            , prenatal = Dict.empty
            , wellChild = Dict.empty
            , tuberculosis = Dict.empty
            , hiv = Dict.empty
            , traceContacts = Dict.empty
            , prenatalLabs = Dict.empty
            , ncdLabs = Dict.empty
            , nextVisit = Dict.empty
            }

        totals selectedDate followUps =
            getFollowUpsTotals selectedDate
                (resolveDayAfterSelectedDate selectedDate)
                db
                villageId
                followUps

        homeVisits selectedDate nutritionFollowUps =
            let
                ( nutrition, _, _ ) =
                    totals selectedDate { noFollowUps | nutritionIndividual = Dict.fromList nutritionFollowUps }
            in
            nutrition

        lastDayOfAugust =
            date Time.Aug 31
    in
    describe "getFollowUpsTotals"
        [ test "Home Visit counts a follow up written during the selected past month" <|
            \_ ->
                homeVisits lastDayOfAugust [ nutritionFollowUp "august" (date Time.Aug 20) OneMonth ]
                    |> Expect.equal 1
        , test "Home Visit does not count a follow up written after the selected past month" <|
            \_ ->
                homeVisits lastDayOfAugust [ nutritionFollowUp "september" (date Time.Sep 10) OneDay ]
                    |> Expect.equal 0
        , test "Home Visit counts the child when a later follow up falls due before the past month's own" <|
            \_ ->
                homeVisits lastDayOfAugust
                    [ nutritionFollowUp "august" (date Time.Aug 20) OneMonth
                    , nutritionFollowUp "september" (date Time.Sep 10) OneDay
                    ]
                    |> Expect.equal 1
        , test "Home Visit counts a follow up written today on the first day of the month" <|
            \_ ->
                homeVisits (date Time.Oct 1) [ nutritionFollowUp "today" (date Time.Oct 1) OneDay ]
                    |> Expect.equal 1
        , test "Acute Illness counts an illness followed up in the past month, although a later visit followed it up again" <|
            \_ ->
                totals lastDayOfAugust
                    { noFollowUps
                        | acuteIllness =
                            Dict.fromList
                                [ acuteIllnessFollowUp "august" (date Time.Aug 20) augustVisit
                                , acuteIllnessFollowUp "september" (date Time.Sep 10) septemberVisit
                                ]
                    }
                    |> Expect.equal ( 0, 1, 0 )
        , test "Antenatal Care counts a pregnancy followed up in the past month, although a later visit followed it up again" <|
            \_ ->
                totals lastDayOfAugust
                    { noFollowUps
                        | prenatal =
                            Dict.fromList
                                [ prenatalFollowUp "august" (date Time.Aug 20) augustVisit
                                , prenatalFollowUp "september" (date Time.Sep 10) septemberVisit
                                ]
                    }
                    |> Expect.equal ( 0, 0, 1 )
        ]


resolveDayAfterSelectedDateTest : Test
resolveDayAfterSelectedDateTest =
    describe "resolveDayAfterSelectedDate"
        [ test "a past month's last day gives the first day of the next month" <|
            \_ ->
                resolveDayAfterSelectedDate (Date.fromCalendarDate 2026 Time.Aug 31)
                    |> Expect.equal (Date.fromCalendarDate 2026 Time.Sep 1)
        , test "the first day of the current month gives the day after it" <|
            \_ ->
                resolveDayAfterSelectedDate (Date.fromCalendarDate 2026 Time.Oct 1)
                    |> Expect.equal (Date.fromCalendarDate 2026 Time.Oct 2)
        ]


countCurrentlyPregnantWithDangerSignsForSelectedMonthTest : Test
countCurrentlyPregnantWithDangerSignsForSelectedMonthTest =
    let
        firstOfOctober =
            Date.fromCalendarDate 2026 Time.Oct 1

        pregnancy =
            openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10)

        encounterWith dangerSign startDate =
            let
                encounter =
                    prenatalEncounterOn startDate
            in
            { encounter | dangerSigns = EverySet.singleton dangerSign }
    in
    describe "countCurrentlyPregnantWithDangerSignsForSelectedMonth"
        [ test "an encounter today without danger signs clears last month's, on the first day of the month" <|
            \_ ->
                countCurrentlyPregnantWithDangerSignsForSelectedMonth firstOfOctober
                    False
                    [ { pregnancy
                        | encounters =
                            [ encounterWith VaginalBleeding (Date.fromCalendarDate 2026 Time.Sep 15)
                            , encounterWith NoDangerSign firstOfOctober
                            ]
                      }
                    ]
                    |> Expect.equal 0
        ]


previousThreeMonthsNewestFirstTest : Test
previousThreeMonthsNewestFirstTest =
    let
        -- Every month key the statistics can hold.
        resultFor currentMonth =
            List.range 1 13
                |> List.map (\month -> ( month, () ))
                |> Dict.fromList
                |> previousThreeMonthsNewestFirst currentMonth
                |> List.map Tuple.first
    in
    describe "previousThreeMonthsNewestFirst"
        [ test "January lists December, November, October" <|
            \_ ->
                resultFor 1
                    |> Expect.equal [ 12, 11, 10 ]
        , test "February lists January before the previous year's months" <|
            \_ ->
                resultFor 2
                    |> Expect.equal [ 1, 12, 11 ]
        , test "March lists February, January, December" <|
            \_ ->
                resultFor 3
                    |> Expect.equal [ 2, 1, 12 ]
        , test "July lists June, May, April" <|
            \_ ->
                resultFor 7
                    |> Expect.equal [ 6, 5, 4 ]
        , test "December lists November, October, September" <|
            \_ ->
                resultFor 12
                    |> Expect.equal [ 11, 10, 9 ]
        , test "a month with no entry is left out" <|
            \_ ->
                Dict.fromList [ ( 1, () ), ( 11, () ) ]
                    |> previousThreeMonthsNewestFirst 2
                    |> List.map Tuple.first
                    |> Expect.equal [ 1, 11 ]
        ]


{-| A prenatal visit of `encounterType` on `startDate`, whose Next Steps
referral recorded `referrals`.
-}
visitWithReferrals : PrenatalEncounterType -> NominalDate -> List ReferToFacilitySign -> PrenatalEncounterDataItem
visitWithReferrals encounterType startDate referrals =
    let
        encounter =
            prenatalEncounterOn startDate
    in
    { encounter
        | encounterType = encounterType
        , referToFacilitySigns = EverySet.fromList referrals
    }


{-| A nurse antenatal visit on `startDate` where the woman was referred to the
hospital.
-}
nurseHospitalReferralOn : NominalDate -> PrenatalEncounterDataItem
nurseHospitalReferralOn startDate =
    visitWithReferrals NurseEncounter startDate [ ReferToHospital, ReferralFormHospital ]


{-| An open pregnancy of person `identifier`, holding `encounters`.
-}
pregnancyWithVisits : PersonIdentifier -> List PrenatalEncounterDataItem -> PrenatalDataItem
pregnancyWithVisits identifier encounters =
    let
        pregnancy =
            openPregnancyRegisteredOn (Date.fromCalendarDate 2026 Time.Mar 10)
    in
    { pregnancy | identifier = identifier, encounters = encounters }


{-| A CHW visit of `encounterType` on `startDate` where the woman was sent to
the health centre, with the referral form handed and an escort.
-}
chwHealthCentreReferralOn : PrenatalEncounterType -> NominalDate -> PrenatalEncounterDataItem
chwHealthCentreReferralOn encounterType startDate =
    let
        encounter =
            prenatalEncounterOn startDate
    in
    { encounter
        | encounterType = encounterType
        , sendToHCSigns = EverySet.fromList [ HandReferrerForm, PrenatalAccompanyToHC, ReferToHealthCenter ]
    }


countHospitalReferralsForSelectedMonthTest : Test
countHospitalReferralsForSelectedMonthTest =
    let
        endOfJuly =
            day 31

        count =
            countHospitalReferralsForSelectedMonth endOfJuly

        -- One pregnancy whose only visit is a nurse visit on `startDate`
        -- recording `referrals`.
        nurseVisitOnly startDate referrals =
            [ pregnancyWithVisits 1 [ visitWithReferrals NurseEncounter startDate referrals ] ]
    in
    describe "countHospitalReferralsForSelectedMonth"
        [ describe "A pregnancy referred to the hospital at a nurse visit (antenatal or postpartum) in the selected month counts in Hospital Referrals for that month"
            [ test "a pregnancy referred to the hospital at a nurse antenatal visit in the selected month counts" <|
                \_ ->
                    -- The nurse records the hospital referral in Next Steps;
                    -- nothing is recorded among the health-centre signs.
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (day 12) ] ]
                        |> Expect.equal 1
            , test "a pregnancy referred to the hospital at a nurse postpartum visit in the selected month counts" <|
                \_ ->
                    -- The pregnancy concluded with a live birth earlier in the
                    -- month; the postpartum visit after it still counts.
                    let
                        pregnancy =
                            pregnancyWithVisits 1
                                [ prenatalEncounterOn (Date.fromCalendarDate 2026 Time.Jun 2)
                                , visitWithReferrals NursePostpartumEncounter (day 20) [ ReferToHospital ]
                                ]
                    in
                    count
                        [ { pregnancy
                            | dateConcluded = Just (day 5)
                            , outcome = Just (Pregnancy OutcomeLiveAtTerm)
                            , deliveryLocation = Just FacilityDelivery
                          }
                        ]
                        |> Expect.equal 1
            , test "the hospital referral counts without the referral form being handed" <|
                \_ ->
                    -- Whether the form was handed is a separate answer; the
                    -- referral itself is what is counted.
                    count (nurseVisitOnly (day 12) [ ReferToHospital ])
                        |> Expect.equal 1
            , test "a hospital referral recorded beside a referral to another service counts" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [ ReferToHospital, ReferToMentalHealthSpecialist, ReferralFormMentalHealthSpecialist ])
                        |> Expect.equal 1
            , test "a referral on the first day of the selected month counts" <|
                \_ ->
                    count (nurseVisitOnly (day 1) [ ReferToHospital ])
                        |> Expect.equal 1
            , test "a referral on the last day of the selected month counts" <|
                \_ ->
                    count (nurseVisitOnly (day 31) [ ReferToHospital ])
                        |> Expect.equal 1
            , test "a referral at a later nurse visit counts, after an earlier visit without one" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1
                            [ visitWithReferrals NurseEncounter (Date.fromCalendarDate 2026 Time.May 4) [ NoReferToFacilitySigns ]
                            , nurseHospitalReferralOn (day 12)
                            ]
                        ]
                        |> Expect.equal 1
            , test "a referral at the first nurse visit counts, before a later visit without one in the same month" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1
                            [ nurseHospitalReferralOn (day 3)
                            , visitWithReferrals NurseEncounter (day 24) [ NoReferToFacilitySigns ]
                            ]
                        ]
                        |> Expect.equal 1
            , test "two pregnancies each referred in the selected month count as two" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (day 8) ]
                        , pregnancyWithVisits 2 [ visitWithReferrals NursePostpartumEncounter (day 15) [ ReferToHospital ] ]
                        ]
                        |> Expect.equal 2
            , test "only the referred pregnancy counts, beside one that was not referred" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (day 8) ]
                        , pregnancyWithVisits 2 [ visitWithReferrals NurseEncounter (day 9) [ NoReferToFacilitySigns ] ]
                        ]
                        |> Expect.equal 1
            , test "no pregnancies count as 0" <|
                \_ ->
                    count []
                        |> Expect.equal 0
            ]
        , describe "A pregnancy referred to the hospital at two nurse visits in the same month counts once, the same way the card counts today"
            [ test "a pregnancy referred to the hospital at two nurse visits in the same month counts once" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (day 6), nurseHospitalReferralOn (day 27) ] ]
                        |> Expect.equal 1
            , test "a pregnancy referred at an antenatal and a postpartum nurse visit in the same month counts once" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1
                            [ nurseHospitalReferralOn (day 2)
                            , visitWithReferrals NursePostpartumEncounter (day 29) [ ReferToHospital ]
                            ]
                        ]
                        |> Expect.equal 1
            ]
        , describe "A nurse visit where the nurse answered that the woman was not referred to the hospital doesn't count"
            [ test "a nurse visit where the nurse answered that the woman was not referred to the hospital doesn't count" <|
                \_ ->
                    -- Answering "no" with a reason stores the 'none' value.
                    count (nurseVisitOnly (day 12) [ NoReferToFacilitySigns ])
                        |> Expect.equal 0
            , test "a nurse visit with no referral answer stored doesn't count" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [])
                        |> Expect.equal 0
            ]
        , describe "Referrals to other services (mental health specialist, ARV programme, NCD programme, ultrasound) don't count"
            [ test "a referral to a mental health specialist doesn't count" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [ ReferToMentalHealthSpecialist, ReferralFormMentalHealthSpecialist, AccompanyToMentalHealthSpecialist ])
                        |> Expect.equal 0
            , test "a referral to the ARV programme doesn't count" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [ ReferToARVProgram, ReferralFormARVProgram, AccompanyToARVProgram ])
                        |> Expect.equal 0
            , test "a referral to the NCD programme doesn't count" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [ ReferToNCDProgram, ReferralFormNCDProgram, AccompanyToNCDProgram ])
                        |> Expect.equal 0
            , test "a referral to ultrasound doesn't count" <|
                \_ ->
                    count (nurseVisitOnly (day 12) [ ReferToUltrasound, ReferralFormUltrasound ])
                        |> Expect.equal 0
            , test "referrals to every other service at once don't count" <|
                \_ ->
                    count
                        (nurseVisitOnly (day 12)
                            [ ReferToARVProgram
                            , ReferToMentalHealthSpecialist
                            , ReferToNCDProgram
                            , ReferToUltrasound
                            ]
                        )
                        |> Expect.equal 0
            , test "a referral to the health centre recorded at a nurse visit doesn't count" <|
                \_ ->
                    -- The card counts hospital referrals; a health-centre
                    -- sign on a nurse visit, with no hospital referral, is
                    -- not one.
                    let
                        encounter =
                            prenatalEncounterOn (day 12)
                    in
                    count [ pregnancyWithVisits 1 [ { encounter | sendToHCSigns = EverySet.singleton ReferToHealthCenter } ] ]
                        |> Expect.equal 0
            ]
        , describe "A CHW visit's referral to the health centre doesn't count"
            [ test "a CHW visit's referral to the health centre doesn't count" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ chwHealthCentreReferralOn ChwFirstEncounter (day 12) ] ]
                        |> Expect.equal 0
            , test "a referral to the health centre at any kind of CHW visit doesn't count" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1 [ chwHealthCentreReferralOn ChwFirstEncounter (day 3) ]
                        , pregnancyWithVisits 2 [ chwHealthCentreReferralOn ChwSecondEncounter (day 10) ]
                        , pregnancyWithVisits 3 [ chwHealthCentreReferralOn ChwThirdPlusEncounter (day 17) ]
                        , pregnancyWithVisits 4 [ chwHealthCentreReferralOn ChwPostpartumEncounter (day 24) ]
                        ]
                        |> Expect.equal 0
            , test "a pregnancy sent to the health centre by a CHW and referred to the hospital by the nurse counts once" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1
                            [ chwHealthCentreReferralOn ChwSecondEncounter (day 4)
                            , nurseHospitalReferralOn (day 5)
                            ]
                        ]
                        |> Expect.equal 1
            ]
        , describe "A hospital referral at a nurse visit in another month doesn't count in the selected month"
            [ test "a hospital referral at a nurse visit in another month doesn't count in the selected month" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (Date.fromCalendarDate 2026 Time.Jun 12) ] ]
                        |> Expect.equal 0
            , test "a referral on the last day of the previous month doesn't count" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (Date.fromCalendarDate 2026 Time.Jun 30) ] ]
                        |> Expect.equal 0
            , test "a referral on the first day of the next month doesn't count" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (Date.fromCalendarDate 2026 Time.Aug 1) ] ]
                        |> Expect.equal 0
            , test "a referral in the same month of the previous year doesn't count" <|
                \_ ->
                    count [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (Date.fromCalendarDate 2025 Time.Jul 12) ] ]
                        |> Expect.equal 0
            , test "a pregnancy referred last month and seen this month without a referral doesn't count" <|
                \_ ->
                    count
                        [ pregnancyWithVisits 1
                            [ nurseHospitalReferralOn (Date.fromCalendarDate 2026 Time.Jun 20)
                            , visitWithReferrals NurseEncounter (day 18) [ NoReferToFacilitySigns ]
                            ]
                        ]
                        |> Expect.equal 0
            , test "a pregnancy referred in the selected month and the next is counted in each" <|
                \_ ->
                    let
                        pregnancies =
                            [ pregnancyWithVisits 1
                                [ nurseHospitalReferralOn (day 20)
                                , nurseHospitalReferralOn (Date.fromCalendarDate 2026 Time.Aug 14)
                                ]
                            ]
                    in
                    [ count pregnancies
                    , countHospitalReferralsForSelectedMonth (Date.fromCalendarDate 2026 Time.Aug 31) pregnancies
                    ]
                        |> Expect.equal [ 1, 1 ]
            ]
        ]


{-| One pregnancy as the dashboard data carries it, with a nurse visit in
July 2026. `referrals` is the visit's `referrals` field, or `Nothing` for data
generated before that field existed.
-}
prenatalDataItemJson : Maybe Json.Encode.Value -> Json.Encode.Value
prenatalDataItemJson referrals =
    Json.Encode.object
        [ ( "id", Json.Encode.int 1 )
        , ( "created", Json.Encode.string "2026-03-10" )
        , ( "expected_date_concluded", Json.Encode.string "2026-12-01" )
        , ( "date_concluded", Json.Encode.null )
        , ( "outcome", Json.Encode.null )
        , ( "delivery_location", Json.Encode.null )
        , ( "encounters"
          , Json.Encode.list Json.Encode.object
                [ [ ( "start_date", Json.Encode.string "2026-07-12" )
                  , ( "encounter_type", Json.Encode.string "nurse" )
                  , ( "danger_signs", Json.Encode.list Json.Encode.string [ "none" ] )
                  , ( "diagnoses", Json.Encode.list Json.Encode.string [] )
                  , ( "send_to_hc", Json.Encode.list Json.Encode.string [] )
                  ]
                    ++ (Maybe.map (\value -> [ ( "referrals", value ) ]) referrals
                            |> Maybe.withDefault []
                       )
                ]
          )
        ]


hospitalReferralsInSavedDataTest : Test
hospitalReferralsInSavedDataTest =
    let
        -- Decodes one pregnancy and reads the card for July 2026.
        countDecoded json =
            Json.Decode.decodeValue decodePrenatalDataItem json
                |> Result.map (List.singleton >> countHospitalReferralsForSelectedMonth (day 31))

        strings =
            Json.Encode.list Json.Encode.string
    in
    describe "A device whose saved dashboard data has no hospital referrals still shows the dashboard, with 0 on this card, until new data arrives"
        [ test "a device whose saved dashboard data has no hospital referrals still shows the dashboard, with 0 on this card" <|
            \_ ->
                -- Data saved before the field existed has no `referrals` on
                -- its visits. It must still decode, or the dashboard is gone.
                countDecoded (prenatalDataItemJson Nothing)
                    |> Expect.equal (Ok 0)
        , test "saved dashboard data without referrals still decodes as a whole" <|
            \_ ->
                -- The whole stored record, as it is read on start, with a
                -- pregnancy whose visit has no `referrals` field.
                statsWithGenerationDate
                    |> encodeDashboardStatsRaw
                    |> List.map
                        (\( key, value ) ->
                            if key == "prenatal_data" then
                                ( key, Json.Encode.list identity [ prenatalDataItemJson Nothing ] )

                            else
                                ( key, value )
                        )
                    |> Json.Encode.object
                    |> Json.Decode.decodeValue decodeDashboardStatsRaw
                    |> Result.map (.prenatalData >> countHospitalReferralsForSelectedMonth (day 31))
                    |> Expect.equal (Ok 0)
        , test "a null referrals value decodes, with 0 on this card" <|
            \_ ->
                countDecoded (prenatalDataItemJson (Just Json.Encode.null))
                    |> Expect.equal (Ok 0)
        , test "an empty referrals list decodes, with 0 on this card" <|
            \_ ->
                countDecoded (prenatalDataItemJson (Just (strings [])))
                    |> Expect.equal (Ok 0)
        , test "new data with a hospital referral at a nurse visit shows on this card" <|
            \_ ->
                countDecoded (prenatalDataItemJson (Just (strings [ "hospital", "hospital-referral-form" ])))
                    |> Expect.equal (Ok 1)
        , test "a referral value this version does not know is dropped, and the hospital referral beside it still counts" <|
            \_ ->
                countDecoded (prenatalDataItemJson (Just (strings [ "a-future-facility", "hospital" ])))
                    |> Expect.equal (Ok 1)
        , test "new data with a hospital referral is still counted after it is saved and read back" <|
            \_ ->
                -- Downloaded data is re-encoded before it is stored, and
                -- decoded again on the next start.
                { statsWithGenerationDate | prenatalData = [ pregnancyWithVisits 1 [ nurseHospitalReferralOn (day 12) ] ] }
                    |> encodeDashboardStatsRaw
                    |> Json.Encode.object
                    |> Json.Decode.decodeValue decodeDashboardStatsRaw
                    |> Result.map (.prenatalData >> countHospitalReferralsForSelectedMonth (day 31))
                    |> Expect.equal (Ok 1)
        ]


all : Test
all =
    describe "Pages.Dashboard.Utils"
        [ caseManagementMergeDuplicatesTest
        , countChildrenSeenForSelectedMonthTest
        , countCurrentlyPregnantForSelectedMonthTest
        , resolveECDStatusTest
        , countCurrentlyPregnantWithDangerSignsForSelectedMonthTest
        , dataItemMergeDuplicatesTest
        , followUpsTotalsTest
        , countHospitalReferralsForSelectedMonthTest
        , hospitalReferralsInSavedDataTest
        , previousThreeMonthsNewestFirstTest
        , resolveDayAfterSelectedDateTest
        , statsStorageRoundTripTest
        ]
