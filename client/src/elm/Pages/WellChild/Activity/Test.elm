module Pages.WellChild.Activity.Test exposing (all)

import App.Model
import AssocList as Dict
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , HeadCircumferenceInCm(..)
        , HeightInCm(..)
        , MeasurementNote(..)
        , PregnancySummarySign(..)
        , PregnancySummaryValue
        , VaccinationValue
        , VaccineDose(..)
        , WeightInGrm(..)
        , WellChildMeasurements
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolvePreviousValuesSetForChild)
import Backend.WellChildEncounter.Model
import Date exposing (Date)
import EverySet
import Expect
import Measurement.Model exposing (RangedMeasurement(..), emptyHeightForm, emptyMuacForm, emptyVaccinationForm, emptyWeightForm)
import Pages.WellChild.Activity.Model exposing (Msg(..), WarningPopupType(..), emptyModel, emptyPregnancySummaryForm)
import Pages.WellChild.Activity.Types exposing (MedicationTask(..))
import Pages.WellChild.Activity.Update exposing (update)
import Pages.WellChild.Activity.Utils
    exposing
        ( ageInMonthsReached
        , expectMedicationByAge
        , pregnancySummaryFormWithDefault
        , pregnancySummaryMeasurementsOutOfRange
        , resolveFirstEncounterDateAfterMilestone
        , resolveNextDateForECDVisit
        , resolveNextDateForMedicationVisitUnderSixMonths
        , resolvePreviousMaybeValue
        )
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures exposing (testParticipant, testPerson, vitalsValueWith, wrapMeasurement)
import Time


all : Test
all =
    describe "Pages.WellChild.Activity"
        [ ageInMonthsReachedTests
        , resolveFirstEncounterDateAfterMilestoneTests
        , resolveNextDateForECDVisitTests
        , nextECDVisitOnMonthEndTests
        , nextECDVisitWeekMilestonesTests
        , resolveNextDateForMedicationVisitUnderSixMonthsTests
        , pregnancySummaryFormWithDefaultTests
        , pregnancySummaryMeasurementsOutOfRangeTests
        , nutritionAssessmentGateTests
        , dtpStandaloneSaveTests
        , resolvePreviousMaybeValueTests
        , previousHeadCircumferenceTests
        ]


emptyWellChildMeasurements : WellChildMeasurements
emptyWellChildMeasurements =
    { pregnancySummary = Nothing
    , symptomsReview = Nothing
    , vitals = Nothing
    , height = Nothing
    , muac = Nothing
    , nutrition = Nothing
    , photo = Nothing
    , weight = Nothing
    , contributingFactors = Nothing
    , healthEducation = Nothing
    , followUp = Nothing
    , sendToHC = Nothing
    , headCircumference = Nothing
    , ecd = Nothing
    , albendazole = Nothing
    , mebendezole = Nothing
    , vitaminA = Nothing
    , nextVisit = Nothing
    , bcgImmunisation = Nothing
    , dtpImmunisation = Nothing
    , dtpStandaloneImmunisation = Nothing
    , hpvImmunisation = Nothing
    , ipvImmunisation = Nothing
    , mrImmunisation = Nothing
    , opvImmunisation = Nothing
    , pcv13Immunisation = Nothing
    , rotarixImmunisation = Nothing
    , ncda = Nothing
    , feeding = Nothing
    , hygiene = Nothing
    , foodSecurity = Nothing
    , caring = Nothing
    }


{-| The head circumference form shows the child's last measured value. A
visit where it was not taken stores 0 cm with a note, and is skipped.
-}
previousHeadCircumferenceTests : Test
previousHeadCircumferenceTests =
    let
        currentDate =
            Date.fromCalendarDate 2026 Time.Jul 27

        childId =
            toEntityUuid "child"

        participantId =
            toEntityUuid "participant"

        dateMonthsAgo monthsAgo =
            Date.add Date.Months -monthsAgo currentDate

        encounterId monthsAgo =
            toEntityUuid ("encounter-" ++ String.fromInt monthsAgo)

        -- Each visit is (months ago, cm, notes).
        previousHeadCircumference visits =
            { emptyModelIndexedDb
                | individualParticipantsByPerson =
                    Dict.singleton childId
                        (Success <|
                            Dict.singleton participantId (testParticipant (dateMonthsAgo 12) WellChildEncounter)
                        )
                , wellChildEncountersByParticipant =
                    Dict.singleton participantId
                        (Success <|
                            Dict.fromList <|
                                List.map
                                    (\( monthsAgo, _, _ ) ->
                                        ( encounterId monthsAgo
                                        , Backend.WellChildEncounter.Model.emptyWellChildEncounter participantId
                                            (dateMonthsAgo monthsAgo)
                                            Backend.WellChildEncounter.Model.PediatricCare
                                            Nothing
                                        )
                                    )
                                    visits
                        )
                , wellChildMeasurements =
                    Dict.fromList <|
                        List.map
                            (\( monthsAgo, cm, notes ) ->
                                ( encounterId monthsAgo
                                , Success
                                    { emptyWellChildMeasurements
                                        | headCircumference =
                                            wrapMeasurement (dateMonthsAgo monthsAgo)
                                                { headCircumference = HeadCircumferenceInCm cm
                                                , notes = EverySet.fromList notes
                                                }
                                    }
                                )
                            )
                            visits
            }
                |> resolvePreviousValuesSetForChild currentDate SiteRwanda childId
                |> .headCircumference
    in
    describe "previous head circumference"
        [ test "is the last measured value" <|
            \_ ->
                previousHeadCircumference [ ( 2, 44, [ NoMeasurementNotes ] ), ( 1, 45, [ NoMeasurementNotes ] ) ]
                    |> Expect.equal (Just 45)
        , test "skips a later visit where it was not taken" <|
            \_ ->
                previousHeadCircumference [ ( 2, 44, [ NoMeasurementNotes ] ), ( 1, 0, [ NoteNotTaken ] ) ]
                    |> Expect.equal (Just 44)
        , test "is empty when it was never taken" <|
            \_ ->
                previousHeadCircumference [ ( 1, 0, [ NoteNotTaken ] ) ]
                    |> Expect.equal Nothing
        ]


resolvePreviousMaybeValueTests : Test
resolvePreviousMaybeValueTests =
    let
        currentDate =
            Date.fromCalendarDate 2026 Time.Jul 27

        dateMonthsAgo monthsAgo =
            Date.add Date.Months -monthsAgo currentDate

        rowAt monthsAgo measurements =
            ( dateMonthsAgo monthsAgo
            , ( toEntityUuid ("prev-encounter-" ++ String.fromInt monthsAgo), measurements )
            )

        withTemperature temperature =
            let
                vitalsValue =
                    vitalsValueWith 120 80
            in
            { emptyWellChildMeasurements
                | vitals = wrapMeasurement currentDate { vitalsValue | bodyTemperature = Just temperature }
            }

        -- The history is passed most recent first, the order
        -- generatePreviousMeasurements produces.
        resolveTemperatureWith previous =
            resolvePreviousMaybeValue
                { id = toEntityUuid "current-encounter"
                , encounter =
                    Backend.WellChildEncounter.Model.emptyWellChildEncounter
                        (toEntityUuid "participant")
                        currentDate
                        Backend.WellChildEncounter.Model.PediatricCare
                        Nothing
                , participant = testParticipant currentDate WellChildEncounter
                , person = testPerson
                , measurements = emptyWellChildMeasurements
                , previousMeasurementsWithDates = previous
                , vaccinationHistory = Dict.empty
                , vaccinationProgress = Dict.empty
                }
                .vitals
                .bodyTemperature
    in
    describe "resolvePreviousMaybeValue"
        [ test "the previous value is the most recent recorded one" <|
            \_ ->
                resolveTemperatureWith
                    [ rowAt 1 (withTemperature 37.5)
                    , rowAt 2 (withTemperature 36.5)
                    ]
                    |> Expect.equal (Just 37.5)
        , test "an encounter where the value was not recorded is skipped" <|
            \_ ->
                resolveTemperatureWith
                    [ rowAt 1 { emptyWellChildMeasurements | vitals = wrapMeasurement (dateMonthsAgo 1) (vitalsValueWith 120 80) }
                    , rowAt 2 (withTemperature 36.5)
                    ]
                    |> Expect.equal (Just 36.5)
        ]


{-| What the Save action of a Nutrition Assessment task does with a measurement
that is outside the range it can take.

The measurement is named on a popup and nothing is saved. The task is left as it
is, so the measurement can be entered again.

-}
nutritionAssessmentGateTests : Test
nutritionAssessmentGateTests =
    let
        modelWith height muac weight =
            let
                data =
                    emptyModel.nutritionAssessmentData
            in
            { emptyModel
                | nutritionAssessmentData =
                    { data
                        | heightForm = { emptyHeightForm | height = height }
                        , muacForm = { emptyMuacForm | muac = muac }
                        , weightForm = { emptyWeightForm | weight = weight }
                    }
            }

        encounterId =
            toEntityUuid "encounter"

        run model msg =
            update (Date.fromCalendarDate 2026 Time.Jul 27)
                SiteRwanda
                encounterId
                emptyModelIndexedDb
                msg
                model

        preSave model msg =
            let
                ( updatedModel, _, appMsgs ) =
                    run model msg
            in
            ( updatedModel.warningPopupState, List.isEmpty appMsgs )

        encounterMsgs model msg =
            let
                ( _, _, appMsgs ) =
                    run model msg
            in
            appMsgs

        encounterMsg =
            Backend.Model.MsgWellChildEncounter encounterId
                >> App.Model.MsgIndexedDb

        person =
            toEntityUuid "person"

        withHeadCircumference headCircumference notTaken =
            let
                data =
                    emptyModel.nutritionAssessmentData

                form =
                    data.headCircumferenceForm
            in
            { emptyModel
                | nutritionAssessmentData =
                    { data
                        | headCircumferenceForm =
                            { form | headCircumference = Just headCircumference, measurementNotTaken = Just notTaken }
                    }
            }
    in
    describe "the Nutrition Assessment save gate"
        [ test "a height outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith (Just 1050) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementHeight ]), True )
        , test "a weight outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing Nothing (Just 850))
                    (PreSaveWeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementWeight ]), True )
        , test "a MUAC outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing (Just 125) Nothing)
                    (PreSaveMuac person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementMuac ]), True )
        , test "a height within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith (Just 105) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        , test "a MUAC within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith Nothing (Just 12.5) Nothing)
                    (PreSaveMuac person Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        , test "a head circumference typed as 0 names it and saves nothing" <|
            \_ ->
                preSave (withHeadCircumference 0 False)
                    (PreSaveHeadCircumference person (Just -30) Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementHeadCircumference ]), True )
        , test "a head circumference in millimetres names it and saves nothing" <|
            \_ ->
                preSave (withHeadCircumference 450 False)
                    (PreSaveHeadCircumference person (Just 200) Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementHeadCircumference ]), True )
        , test "a head circumference not taken shows no popup and goes on to save" <|
            \_ ->
                preSave (withHeadCircumference 0 True)
                    (PreSaveHeadCircumference person Nothing Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        , test "a head circumference within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (withHeadCircumference 45 False)
                    (PreSaveHeadCircumference person (Just 0.2) Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        , test "a head circumference with a normal z-score clears the cephaly warnings" <|
            \_ ->
                encounterMsgs (withHeadCircumference 45 False)
                    (PreSaveHeadCircumferenceWarning person (Just 0.2) Nothing Nothing)
                    |> List.member (encounterMsg Backend.WellChildEncounter.Model.ClearHeadCircumferenceWarnings)
                    |> Expect.equal True
        , test "a head circumference with a high z-score clears nothing" <|
            \_ ->
                encounterMsgs (withHeadCircumference 55 False)
                    (PreSaveHeadCircumferenceWarning person (Just 3.5) Nothing Nothing)
                    |> List.member (encounterMsg Backend.WellChildEncounter.Model.ClearHeadCircumferenceWarnings)
                    |> Expect.equal False
        ]


pregnancySummaryMeasurementsOutOfRangeTests : Test
pregnancySummaryMeasurementsOutOfRangeTests =
    let
        lengthForm available length =
            { emptyPregnancySummaryForm
                | birthLengthAvailable = available
                , birthLength = Maybe.map HeightInCm length
            }

        apgarForm available one five =
            { emptyPregnancySummaryForm
                | apgarScoresAvailable = available
                , apgarOneMin = one
                , apgarFiveMin = five
            }

        outOfRange =
            pregnancySummaryMeasurementsOutOfRange SiteRwanda
    in
    describe "pregnancySummaryMeasurementsOutOfRange"
        [ describe "the birth length"
            -- A newborn's length is around 50cm. The height range used elsewhere
            -- runs to 250cm, which is a grown adult, so birth length has its own.
            [ test "an ordinary birth length is in range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 50))
                        |> Expect.equal []
            , test "a length entered in metres is out of range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 0.5))
                        |> Expect.equal [ MeasurementBirthLength ]
            , test "a length entered in millimetres is out of range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 500))
                        |> Expect.equal [ MeasurementBirthLength ]
            , test "the ends of the range are accepted" <|
                \_ ->
                    ( outOfRange (lengthForm (Just True) (Just 15))
                    , outOfRange (lengthForm (Just True) (Just 60))
                    )
                        |> Expect.equal ( [], [] )
            , test "just outside either end is refused" <|
                \_ ->
                    ( outOfRange (lengthForm (Just True) (Just 14))
                    , outOfRange (lengthForm (Just True) (Just 61))
                    )
                        |> Expect.equal ( [ MeasurementBirthLength ], [ MeasurementBirthLength ] )
            , test "a length that has not been entered is not reported" <|
                \_ ->
                    outOfRange (lengthForm (Just True) Nothing)
                        |> Expect.equal []
            , test "a length is not reported when the form does not ask for one" <|
                -- The input is hidden then, so stopping on it would leave the
                -- form with nothing on screen to correct.
                \_ ->
                    outOfRange (lengthForm (Just False) (Just 0.5))
                        |> Expect.equal []
            ]
        , describe "the Apgar scores"
            -- A score out of 10. A quarter of the scores already stored are not
            -- one: they run to 36, 246, and 30000.
            [ test "an ordinary pair of scores is in range" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 8) (Just 9))
                        |> Expect.equal []
            , test "the ends of the range are accepted" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 0) (Just 10))
                        |> Expect.equal []
            , test "a score above 10 is refused, and says which one" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 36) (Just 9))
                        |> Expect.equal [ MeasurementApgarOneMinute ]
            , test "the five minute score is named on its own" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 8) (Just 30000))
                        |> Expect.equal [ MeasurementApgarFiveMinutes ]
            , test "both are named when both are wrong" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 11) (Just 246))
                        |> Expect.equal [ MeasurementApgarOneMinute, MeasurementApgarFiveMinutes ]
            , test "a score below zero is refused" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just -1) (Just 9))
                        |> Expect.equal [ MeasurementApgarOneMinute ]
            , test "scores that have not been entered are not reported" <|
                \_ ->
                    outOfRange (apgarForm (Just True) Nothing Nothing)
                        |> Expect.equal []
            , test "scores are not reported when the form does not ask for them" <|
                -- The inputs are hidden then, the same as the birth length.
                \_ ->
                    outOfRange (apgarForm (Just False) (Just 36) (Just 30000))
                        |> Expect.equal []
            ]
        , describe "the birth weight, which is asked for every time"
            [ test "a weight in grams is in range" <|
                \_ ->
                    outOfRange { emptyPregnancySummaryForm | birthWeight = Just (WeightInGrm 3200) }
                        |> Expect.equal []
            , test "a weight in kilograms is refused, though no question guards it" <|
                \_ ->
                    outOfRange { emptyPregnancySummaryForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal [ MeasurementBirthWeight ]
            ]
        , test "every measurement that is wrong is named, in the order the form asks them" <|
            -- So that the nurse corrects them together rather than being sent
            -- back once for each.
            \_ ->
                outOfRange
                    { emptyPregnancySummaryForm
                        | birthWeight = Just (WeightInGrm 3)
                        , apgarScoresAvailable = Just True
                        , apgarOneMin = Just 36
                        , apgarFiveMin = Just 36
                        , birthLengthAvailable = Just True
                        , birthLength = Just (HeightInCm 0.5)
                    }
                    |> Expect.equal
                        [ MeasurementApgarOneMinute
                        , MeasurementApgarFiveMinutes
                        , MeasurementBirthWeight
                        , MeasurementBirthLength
                        ]
        ]


resolveFirstEncounterDateAfterMilestoneTests : Test
resolveFirstEncounterDateAfterMilestoneTests =
    describe "resolveFirstEncounterDateAfterMilestone"
        [ test "picks the earliest encounter strictly AFTER the milestone, not an earlier one before it" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 15
                    , Date.fromCalendarDate 2024 Time.Sep 1
                    ]
                    |> Expect.equal (Just (Date.fromCalendarDate 2024 Time.Jul 15))
        , test "returns Nothing when every encounter is on or before the milestone" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 1
                    ]
                    |> Expect.equal Nothing
        , test "treats an encounter exactly on the milestone date as not after it" <|
            \_ ->
                resolveFirstEncounterDateAfterMilestone (Date.fromCalendarDate 2024 Time.Jul 1)
                    [ Date.fromCalendarDate 2024 Time.Jul 1 ]
                    |> Expect.equal Nothing
        ]


{-| The ECD sign group starting at 18 months means a child seen between 15 and
17 months should return at 18 months, not skip to 24 months.
-}
resolveNextDateForECDVisitTests : Test
resolveNextDateForECDVisitTests =
    let
        birthDate =
            Date.fromCalendarDate 2024 Time.Jan 1

        resolve current =
            resolveNextDateForECDVisit current birthDate True
    in
    describe "resolveNextDateForECDVisit"
        [ test "a 15-month-old is scheduled for the 18-month visit, not 24 months" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Apr 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "a 17-month-old is still scheduled for the 18-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jun 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "an 18-month-old moves on to the 24-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jul 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2026 Time.Jan 1))
        , test "the 15-month rung is unchanged for a younger child" <|
            \_ ->
                resolve (Date.fromCalendarDate 2024 Time.Nov 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Apr 1))
        , test "the years ladder is unchanged for a two-year-old" <|
            \_ ->
                resolve (Date.fromCalendarDate 2026 Time.Jan 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2027 Time.Jan 1))
        ]


dateOn : Int -> Time.Month -> Int -> Date
dateOn year month day =
    Date.fromCalendarDate year month day


{-| A child born late in a month has a milestone that falls on the last day of
a shorter month: born 31 March, the 6-month date is 30 September. On that day
the child has reached the milestone, so the next visit is the following rung.
The next visit must never be the day of the visit itself.

From 6 months up the ladder does not look at whether the current signs are
done, so every case here is checked with both answers.

-}
nextECDVisitOnMonthEndTests : Test
nextECDVisitOnMonthEndTests =
    let
        expectNext name bornOn seenOn expected =
            test name <|
                \_ ->
                    List.map (resolveNextDateForECDVisit seenOn bornOn) [ True, False ]
                        |> Expect.equal [ Just expected, Just expected ]
    in
    describe "resolveNextDateForECDVisit around month-end"
        [ describe "a milestone on a shorter month's last day counts as reached that day"
            [ expectNext "Born 31 Mar 2025, seen 30 Sep 2025: the next ECD visit is the 15-month date, 30 Jun 2026"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Sep 30)
                (dateOn 2026 Time.Jun 30)
            , expectNext "Born 31 Aug 2025, seen 28 Feb 2026: the next ECD visit is 30 Nov 2026"
                (dateOn 2025 Time.Aug 31)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.Nov 30)
            , expectNext "Born 30 Nov 2024, seen 28 Feb 2026: the next ECD visit is the 18-month date, 30 May 2026"
                (dateOn 2024 Time.Nov 30)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.May 30)
            , expectNext "Born 29 Feb 2024, seen 28 Feb 2026: the next ECD visit is the 3-year date, 28 Feb 2027"
                (dateOn 2024 Time.Feb 29)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2027 Time.Feb 28)
            , expectNext "6-month date on the 29th of a leap February moves on to the 15-month date"
                (dateOn 2023 Time.Aug 31)
                (dateOn 2024 Time.Feb 29)
                (dateOn 2024 Time.Nov 30)
            , expectNext "born on the 30th, 6-month date on 28 Feb moves on to the 15-month date"
                (dateOn 2025 Time.Aug 30)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.Nov 30)
            , expectNext "born on the 29th, 6-month date on 28 Feb moves on to the 15-month date"
                (dateOn 2025 Time.Aug 29)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.Nov 29)
            , expectNext "15-month date on the 30th moves on to the 18-month date"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2026 Time.Jun 30)
                (dateOn 2026 Time.Sep 30)
            , expectNext "18-month date on 28 Feb moves on to the 2-year date"
                (dateOn 2024 Time.Aug 31)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.Aug 31)
            , expectNext "3-year date on 28 Feb moves on to the 4-year date"
                (dateOn 2024 Time.Feb 29)
                (dateOn 2027 Time.Feb 28)
                (dateOn 2028 Time.Feb 29)
            ]
        , describe "the day before a milestone still schedules that milestone"
            [ expectNext "Born 15 Mar 2025, seen 14 Sep 2025 (one day before the milestone): still 15 Sep 2025"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Sep 14)
                (dateOn 2025 Time.Sep 15)

            -- The visit is on a month's last day, but the milestone is tomorrow.
            , expectNext "born on the 1st, seen on the last day of the month before the 6-month date"
                (dateOn 2025 Time.Apr 1)
                (dateOn 2025 Time.Sep 30)
                (dateOn 2025 Time.Oct 1)
            , expectNext "born on the 31st, seen the day before a 6-month date on the 30th"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Sep 29)
                (dateOn 2025 Time.Sep 30)
            , expectNext "born on the 31st, seen the day before a 6-month date on 28 Feb"
                (dateOn 2025 Time.Aug 31)
                (dateOn 2026 Time.Feb 27)
                (dateOn 2026 Time.Feb 28)

            -- In a leap year 28 February is not the last day of the month.
            , expectNext "28 Feb of a leap year is the day before a 6-month date on the 29th"
                (dateOn 2023 Time.Aug 31)
                (dateOn 2024 Time.Feb 28)
                (dateOn 2024 Time.Feb 29)
            , expectNext "born on the 30th, seen the day before a 15-month date on 28 Feb"
                (dateOn 2024 Time.Nov 30)
                (dateOn 2026 Time.Feb 27)
                (dateOn 2026 Time.Feb 28)
            , expectNext "born 29 Feb, seen the day before a 2-year date on 28 Feb"
                (dateOn 2024 Time.Feb 29)
                (dateOn 2026 Time.Feb 27)
                (dateOn 2026 Time.Feb 28)
            , expectNext "born 29 Feb, seen the day before the 4-year date on 29 Feb"
                (dateOn 2024 Time.Feb 29)
                (dateOn 2028 Time.Feb 28)
                (dateOn 2028 Time.Feb 29)
            ]
        , describe "month-end days that are not such a milestone are unchanged"
            [ expectNext "the day after a 6-month date on the 30th moves on to the 15-month date"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Oct 1)
                (dateOn 2026 Time.Jun 30)

            -- The birth day exists in the milestone month, so the milestone
            -- date is the true anniversary and was always counted as reached.
            , expectNext "born on the 30th, seen on a 6-month date that is the 30th"
                (dateOn 2025 Time.Mar 30)
                (dateOn 2025 Time.Sep 30)
                (dateOn 2026 Time.Jun 30)
            , expectNext "born on the 28th, seen on a 6-month date that is 28 Feb"
                (dateOn 2025 Time.Aug 28)
                (dateOn 2026 Time.Feb 28)
                (dateOn 2026 Time.Nov 28)

            -- 5, 8 and 13 months are not rungs of the ladder, so reaching them
            -- on a month's last day does not move the child on.
            , expectNext "reaching 5 months on a month's last day still schedules the 6-month date"
                (dateOn 2025 Time.Jan 31)
                (dateOn 2025 Time.Jun 30)
                (dateOn 2025 Time.Jul 31)
            , expectNext "reaching 8 months on a month's last day still schedules the 15-month date"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Nov 30)
                (dateOn 2026 Time.Jun 30)
            , expectNext "reaching 13 months on a month's last day still schedules the 15-month date"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2026 Time.Apr 30)
                (dateOn 2026 Time.Jun 30)
            ]
        , describe "Visits away from month-end are unchanged"
            [ expectNext "Visits away from month-end: before 6 months schedules the 6-month date"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Aug 10)
                (dateOn 2025 Time.Sep 15)
            , expectNext "Visits away from month-end: on the 6-month date schedules the 15-month date"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Sep 15)
                (dateOn 2026 Time.Jun 15)
            , expectNext "Visits away from month-end: the day before the 15-month date schedules it"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2026 Time.Jun 14)
                (dateOn 2026 Time.Jun 15)
            ]
        , describe "past the 4-year date the signs decide"
            -- The 4-year rung is the last one, so this is the only place from
            -- 6 months up where the answer depends on the signs.
            [ test "signs done on the 4-year date: no further ECD visit" <|
                \_ ->
                    resolveNextDateForECDVisit (dateOn 2028 Time.Feb 29) (dateOn 2024 Time.Feb 29) True
                        |> Expect.equal Nothing
            , test "signs outstanding on the 4-year date: 6 months from the visit" <|
                \_ ->
                    resolveNextDateForECDVisit (dateOn 2028 Time.Feb 29) (dateOn 2024 Time.Feb 29) False
                        |> Expect.equal (Just (dateOn 2028 Time.Aug 29))
            ]
        ]


{-| The 6-week and 14-week milestones count whole weeks, so they do not depend
on the length of any month. Signs for each appear one week early, which is why
5 and 13 weeks depend on whether the signs are done.

The month-end cases pin that a visit on a month's last day is not treated as
having reached a week milestone that is still a day away.

-}
nextECDVisitWeekMilestonesTests : Test
nextECDVisitWeekMilestonesTests =
    let
        expectNext name bornOn seenOn signsDone expected =
            test name <|
                \_ ->
                    resolveNextDateForECDVisit seenOn bornOn signsDone
                        |> Expect.equal (Just expected)

        bornJan1 =
            dateOn 2025 Time.Jan 1

        sixWeeks =
            dateOn 2025 Time.Feb 12

        fourteenWeeks =
            dateOn 2025 Time.Apr 9

        sixMonths =
            dateOn 2025 Time.Jul 1
    in
    describe "resolveNextDateForECDVisit: the 6-week and 14-week milestones are unchanged"
        [ expectNext "4 weeks old with signs done schedules the 6-week date"
            bornJan1
            (dateOn 2025 Time.Feb 4)
            True
            sixWeeks
        , expectNext "5 weeks old with signs done schedules the 14-week date"
            bornJan1
            (dateOn 2025 Time.Feb 5)
            True
            fourteenWeeks
        , expectNext "5 weeks old with signs outstanding schedules the 6-week date"
            bornJan1
            (dateOn 2025 Time.Feb 5)
            False
            sixWeeks
        , expectNext "on the 6-week date schedules the 14-week date"
            bornJan1
            sixWeeks
            False
            fourteenWeeks
        , expectNext "13 weeks old with signs done schedules the 6-month date"
            bornJan1
            (dateOn 2025 Time.Apr 8)
            True
            sixMonths
        , expectNext "13 weeks old with signs outstanding schedules the 14-week date"
            bornJan1
            (dateOn 2025 Time.Apr 8)
            False
            fourteenWeeks
        , expectNext "on the 14-week date schedules the 6-month date"
            bornJan1
            fourteenWeeks
            False
            sixMonths
        , describe "visits on a month's last day"
            [ expectNext "exactly 6 weeks old on the 30th schedules the 14-week date"
                (dateOn 2025 Time.Aug 19)
                (dateOn 2025 Time.Sep 30)
                False
                (dateOn 2025 Time.Nov 25)
            , expectNext "a day under 6 weeks on the 30th, signs outstanding, schedules the 6-week date"
                (dateOn 2025 Time.Aug 20)
                (dateOn 2025 Time.Sep 30)
                False
                (dateOn 2025 Time.Oct 1)
            , expectNext "a day under 6 weeks on the 30th, signs done, schedules the 14-week date"
                (dateOn 2025 Time.Aug 20)
                (dateOn 2025 Time.Sep 30)
                True
                (dateOn 2025 Time.Nov 26)
            , expectNext "exactly 14 weeks old on the 30th schedules the 6-month date"
                (dateOn 2025 Time.Jun 24)
                (dateOn 2025 Time.Sep 30)
                False
                (dateOn 2025 Time.Dec 24)
            , expectNext "a day under 14 weeks on the 30th, signs outstanding, schedules the 14-week date"
                (dateOn 2025 Time.Jun 25)
                (dateOn 2025 Time.Sep 30)
                False
                (dateOn 2025 Time.Oct 1)
            , expectNext "a day under 14 weeks on the 30th, signs done, schedules the 6-month date"
                (dateOn 2025 Time.Jun 25)
                (dateOn 2025 Time.Sep 30)
                True
                (dateOn 2025 Time.Dec 25)

            -- Reaching 3 months on a month's last day is 13 weeks, and 3
            -- months is not a rung.
            , expectNext "13 weeks old on a month-end 3-month date, signs outstanding, schedules the 14-week date"
                (dateOn 2025 Time.Aug 31)
                (dateOn 2025 Time.Nov 30)
                False
                (dateOn 2025 Time.Dec 7)
            , expectNext "13 weeks old on a month-end 3-month date, signs done, schedules the 6-month date"
                (dateOn 2025 Time.Aug 31)
                (dateOn 2025 Time.Nov 30)
                True
                (dateOn 2026 Time.Feb 28)
            ]
        ]


{-| Under 6 months the medication visit is set to the 6-month date. On a 6-month
date that is the last day of a shorter month the child has reached it, so the
result is Nothing and the regular medication schedule takes over; otherwise
Next Visit would show the day of the visit.
-}
resolveNextDateForMedicationVisitUnderSixMonthsTests : Test
resolveNextDateForMedicationVisitUnderSixMonthsTests =
    let
        expectResult name bornOn seenOn expected =
            test name <|
                \_ ->
                    resolveNextDateForMedicationVisitUnderSixMonths seenOn bornOn
                        |> Expect.equal expected
    in
    describe "resolveNextDateForMedicationVisitUnderSixMonths"
        [ describe "On such a day a child under the 6-month medication date is treated as having reached it, so Next Visit doesn't show today"
            [ expectResult "born 31 Mar 2025, seen 30 Sep 2025"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Sep 30)
                Nothing
            , expectResult "born 31 Aug 2025, seen 28 Feb 2026"
                (dateOn 2025 Time.Aug 31)
                (dateOn 2026 Time.Feb 28)
                Nothing
            , expectResult "born 30 Aug 2025, seen 28 Feb 2026"
                (dateOn 2025 Time.Aug 30)
                (dateOn 2026 Time.Feb 28)
                Nothing
            , expectResult "born 31 Aug 2023, seen 29 Feb 2024 of a leap year"
                (dateOn 2023 Time.Aug 31)
                (dateOn 2024 Time.Feb 29)
                Nothing
            ]
        , describe "a child who has not reached the 6-month date is scheduled for it"
            [ expectResult "born 15 Mar 2025, seen 14 Sep 2025 (one day before)"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Sep 14)
                (Just (dateOn 2025 Time.Sep 15))
            , expectResult "born on the 1st, seen on the last day of the month before the 6-month date"
                (dateOn 2025 Time.Apr 1)
                (dateOn 2025 Time.Sep 30)
                (Just (dateOn 2025 Time.Oct 1))
            , expectResult "born on the 31st, seen the day before a 6-month date on the 30th"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Sep 29)
                (Just (dateOn 2025 Time.Sep 30))
            , expectResult "28 Feb of a leap year is the day before a 6-month date on the 29th"
                (dateOn 2023 Time.Aug 31)
                (dateOn 2024 Time.Feb 28)
                (Just (dateOn 2024 Time.Feb 29))
            , expectResult "seen on the day of birth"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Mar 15)
                (Just (dateOn 2025 Time.Sep 15))
            ]
        , describe "a child who has reached the 6-month date is unchanged"
            [ expectResult "seen on a mid-month 6-month date"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2025 Time.Sep 15)
                Nothing
            , expectResult "born on the 28th, seen on a 6-month date that is 28 Feb"
                (dateOn 2025 Time.Aug 28)
                (dateOn 2026 Time.Feb 28)
                Nothing
            , expectResult "the day after a 6-month date on the 30th"
                (dateOn 2025 Time.Mar 31)
                (dateOn 2025 Time.Oct 1)
                Nothing
            , expectResult "well past 6 months"
                (dateOn 2025 Time.Mar 15)
                (dateOn 2026 Time.Mar 15)
                Nothing
            ]
        ]


{-| A saved Pregnancy Summary with every numeric filled in, so the tests can
check what happens when the nurse clears one and re-opens the encounter.
-}
savedPregnancySummary : PregnancySummaryValue
savedPregnancySummary =
    { expectedDateConcluded = Date.fromCalendarDate 2024 Time.Jan 1
    , deliveryComplications = EverySet.empty
    , signs = EverySet.fromList [ ApgarScores, BirthLength ]
    , apgarOneMin = Just 8
    , apgarFiveMin = Just 9
    , birthWeight = Just (WeightInGrm 3000)
    , birthLength = Just (HeightInCm 50)
    , birthDefects = EverySet.empty
    }


{-| The same record with its signs lost, which is what a decoder that cannot
read them leaves behind. The measurements are still there.
-}
savedWithoutSigns : PregnancySummaryValue
savedWithoutSigns =
    { savedPregnancySummary | signs = EverySet.singleton NoPregnancySummarySigns }


{-| Clearing a value (for example answering "Apgar scores available? No", which
empties the scores and marks them dirty) must stay cleared when the form is
rebuilt from the saved value. An untouched field still loads from the saved
value.
-}
pregnancySummaryFormWithDefaultTests : Test
pregnancySummaryFormWithDefaultTests =
    let
        resolve form =
            pregnancySummaryFormWithDefault form (Just savedPregnancySummary)
    in
    describe "pregnancySummaryFormWithDefault"
        [ test "cleared Apgar scores stay cleared when the field is dirty" <|
            \_ ->
                let
                    resolved =
                        resolve { emptyPregnancySummaryForm | apgarDirty = True }
                in
                ( resolved.apgarOneMin, resolved.apgarFiveMin )
                    |> Expect.equal ( Nothing, Nothing )
        , test "untouched Apgar scores load from the saved value" <|
            \_ ->
                let
                    resolved =
                        resolve emptyPregnancySummaryForm
                in
                ( resolved.apgarOneMin, resolved.apgarFiveMin )
                    |> Expect.equal ( Just 8, Just 9 )
        , test "an edited Apgar score wins over the saved value" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | apgarDirty = True, apgarOneMin = Just 5 }
                    |> .apgarOneMin
                    |> Expect.equal (Just 5)
        , test "a cleared birth weight stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | birthWeightDirty = True }
                    |> .birthWeight
                    |> Expect.equal Nothing
        , test "an untouched birth weight loads from the saved value" <|
            \_ ->
                resolve emptyPregnancySummaryForm
                    |> .birthWeight
                    |> Expect.equal (Just (WeightInGrm 3000))
        , test "a cleared birth length stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | birthLengthDirty = True }
                    |> .birthLength
                    |> Expect.equal Nothing
        , describe "a record whose signs were lost"
            -- The measurement and the sign that says it was asked for are
            -- stored apart, and the signs fall back to none when they cannot be
            -- read. The measurement is then live but its input is not drawn: it
            -- is written back out on every save, and the range check cannot
            -- reach it because the check asks only for what the form shows.
            [ test "still asks for the birth length it holds" <|
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm (Just savedWithoutSigns)
                        |> (\resolved -> ( resolved.birthLengthAvailable, resolved.birthLength ))
                        |> Expect.equal ( Just True, Just (HeightInCm 50) )
            , test "still asks for the Apgar scores it holds" <|
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm (Just savedWithoutSigns)
                        |> (\resolved -> ( resolved.apgarScoresAvailable, resolved.apgarOneMin, resolved.apgarFiveMin ))
                        |> Expect.equal ( Just True, Just 8, Just 9 )
            , test "asks for the Apgar scores when it holds only one of them" <|
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm
                        (Just { savedWithoutSigns | apgarOneMin = Nothing })
                        |> .apgarScoresAvailable
                        |> Expect.equal (Just True)
            , test "asks for nothing it does not hold" <|
                -- Otherwise the form would show empty inputs it never asked
                -- for, and count them as tasks still to be done.
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm
                        (Just
                            { savedWithoutSigns
                                | apgarOneMin = Nothing
                                , apgarFiveMin = Nothing
                                , birthLength = Nothing
                            }
                        )
                        |> (\resolved -> ( resolved.apgarScoresAvailable, resolved.birthLengthAvailable ))
                        |> Expect.equal ( Just False, Just False )
            , test "a sign with nothing stored is still asked for" <|
                -- The sign is the answer the nurse gave, so it stands on its
                -- own: were only the value asked, answering yes and saving
                -- before entering anything would hide the question again.
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm
                        (Just
                            { savedPregnancySummary
                                | apgarOneMin = Nothing
                                , apgarFiveMin = Nothing
                                , birthLength = Nothing
                            }
                        )
                        |> (\resolved -> ( resolved.apgarScoresAvailable, resolved.birthLengthAvailable ))
                        |> Expect.equal ( Just True, Just True )
            , test "the nurse saying no wins over what is stored" <|
                -- She has answered the question in front of her; the value is
                -- cleared alongside the answer.
                \_ ->
                    pregnancySummaryFormWithDefault
                        { emptyPregnancySummaryForm
                            | birthLengthAvailable = Just False
                            , birthLengthDirty = True
                        }
                        (Just savedWithoutSigns)
                        |> (\resolved -> ( resolved.birthLengthAvailable, resolved.birthLength ))
                        |> Expect.equal ( Just False, Nothing )
            , test "and the measurement it holds is then checked, which was the point" <|
                -- A length of 0.5 was being written back out untouched, because
                -- the check only asks for what the form shows.
                \_ ->
                    pregnancySummaryFormWithDefault emptyPregnancySummaryForm
                        (Just { savedWithoutSigns | birthLength = Just (HeightInCm 0.5), apgarFiveMin = Just 30000 })
                        |> pregnancySummaryMeasurementsOutOfRange SiteRwanda
                        |> Expect.equal [ MeasurementApgarFiveMinutes, MeasurementBirthLength ]
            ]
        ]


{-| The DTP-standalone task keeps its own form (`dtpStandaloneForm`), separate
from the DTP form (`dtpForm`). Saving it must store what `dtpStandaloneForm`
holds.
-}
dtpStandaloneSaveTests : Test
dtpStandaloneSaveTests =
    let
        today =
            Date.fromCalendarDate 2026 Time.Aug 31

        enteredDoses =
            EverySet.singleton VaccineDoseFirst

        enteredDates =
            EverySet.singleton today

        encounterId =
            toEntityUuid "encounter"

        personId =
            toEntityUuid "person"

        saveMsgs form saved =
            let
                data =
                    emptyModel.immunisationData

                ( _, _, appMsgs ) =
                    update today
                        SiteRwanda
                        encounterId
                        emptyModelIndexedDb
                        (SaveDTPStandaloneImmunisation personId saved Nothing)
                        { emptyModel | immunisationData = { data | dtpStandaloneForm = form } }
            in
            appMsgs

        expectedSaveMsg measurementId value =
            App.Model.MsgIndexedDb
                (Backend.Model.MsgWellChildEncounter encounterId
                    (Backend.WellChildEncounter.Model.SaveDTPStandaloneImmunisation personId measurementId value)
                )
    in
    describe "the DTP-standalone save"
        [ test "stores the doses, dates and note entered on the DTP-standalone form" <|
            \_ ->
                saveMsgs
                    { emptyVaccinationForm
                        | administeredDoses = Just enteredDoses
                        , administrationDates = Just enteredDates
                        , administrationNote = Just AdministeredToday
                    }
                    Nothing
                    |> Expect.equal
                        [ expectedSaveMsg Nothing
                            (VaccinationValue enteredDoses enteredDates AdministeredToday)
                        ]
        , test "on edit, takes doses and dates from the saved measurement and saves under its id" <|
            \_ ->
                let
                    measurementId =
                        toEntityUuid "measurement"

                    savedDoses =
                        EverySet.singleton VaccineDoseSecond

                    savedDates =
                        EverySet.singleton (Date.fromCalendarDate 2026 Time.Aug 24)

                    measurement =
                        { dateMeasured = today
                        , nurse = Nothing
                        , healthCenter = Nothing
                        , participantId = personId
                        , deleted = False
                        , encounterId = Just encounterId
                        , value = VaccinationValue savedDoses savedDates AdministeredPreviously
                        }
                in
                saveMsgs
                    { emptyVaccinationForm
                        | administrationNote = Just AdministeredToday
                        , administrationNoteDirty = True
                    }
                    (Just ( measurementId, measurement ))
                    |> Expect.equal
                        [ expectedSaveMsg (Just measurementId)
                            (VaccinationValue savedDoses savedDates AdministeredToday)
                        ]
        ]


ageInMonthsReachedTests : Test
ageInMonthsReachedTests =
    let
        march31 =
            Date.fromCalendarDate 2025 Time.Mar 31

        sep30 =
            Date.fromCalendarDate 2025 Time.Sep 30

        child =
            { testPerson | birthDate = Just march31 }

        expectVitaminA date =
            expectMedicationByAge date SiteRwanda child TaskVitaminA
    in
    describe "ageInMonthsReached"
        [ test "a month-end day that a milestone falls on completes the month" <|
            \_ ->
                ageInMonthsReached sep30 march31 |> Expect.equal 6
        , test "the day before stays one month short" <|
            \_ ->
                ageInMonthsReached (Date.fromCalendarDate 2025 Time.Sep 29) march31 |> Expect.equal 5
        , test "a mid-month birthday counts as usual" <|
            \_ ->
                ageInMonthsReached (Date.fromCalendarDate 2025 Time.Sep 15) (Date.fromCalendarDate 2025 Time.Mar 15)
                    |> Expect.equal 6
        , test "a month-end day of a birthday that exists in that month is not bumped" <|
            \_ ->
                ageInMonthsReached (Date.fromCalendarDate 2025 Time.Aug 31) march31 |> Expect.equal 5
        , test "Vitamin A is offered on the month-end day the child reaches 6 months" <|
            \_ ->
                expectVitaminA sep30 |> Expect.equal True
        , test "Vitamin A is not offered the day before" <|
            \_ ->
                expectVitaminA (Date.fromCalendarDate 2025 Time.Sep 29) |> Expect.equal False
        ]
