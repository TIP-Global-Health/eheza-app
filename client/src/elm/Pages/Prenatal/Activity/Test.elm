module Pages.Prenatal.Activity.Test exposing (all)

import AssocList as Dict
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BloodGpRsTestValue
        , BloodSmearResult(..)
        , CSectionScar(..)
        , CorePhysicalExamValue
        , DangerSign(..)
        , DangerSignsValue
        , FetalPresentation(..)
        , FollowUpOption(..)
        , GlucoseValue(..)
        , HIVPCRTestValue
        , HIVTestValue
        , HandsCPESign(..)
        , HeightInCm(..)
        , HemoglobinTestValue
        , HepatitisBTestValue
        , LungsCPESign(..)
        , MalariaTestValue
        , Measurement
        , MedicalHistoryInfectiousDisease(..)
        , MedicalHistoryMentalHealthIssue(..)
        , MedicalHistoryPhysicalCondition(..)
        , MedicalHistorySign(..)
        , MedicationDistributionSign(..)
        , MuacInCm(..)
        , ObstetricHistoryStep2Sign(..)
        , OccursInFamilySign(..)
        , PartnerHIVTestValue
        , PregnancySign(..)
        , PrenatalAssesment(..)
        , PrenatalHIVSign(..)
        , PrenatalMeasurements
        , PrenatalMentalHealthQuestion(..)
        , PrenatalMentalHealthQuestionOption(..)
        , PrenatalSymptom(..)
        , PrenatalSymptomQuestion(..)
        , PrenatalVaccineType(..)
        , PreviousDeliveryPeriod(..)
        , PreviousDeliverySign(..)
        , ProteinValue(..)
        , RandomBloodSugarTestValue
        , RecommendedTreatmentSign(..)
        , Rhesus(..)
        , SyphilisTestValue
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , UrineDipstickTestValue
        , VaccineDose(..)
        , ViralLoadStatus(..)
        , VitalsValue
        , WeightInKg(..)
        , emptyPrenatalMeasurements
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (HighRiskFactor(..))
import Backend.PrenatalActivity.Utils exposing (generateHighRiskAlertData)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (div)
import Measurement.Model exposing (RangedMeasurement(..), emptyHIVPCRResultForm, emptyVaccinationForm)
import Measurement.Utils exposing (knownAsPositiveUpdate, knownAsPositiveUpdateHIVTest, knownAsPositiveUpdatePartnerHIVTest, toHIVPCRResultValueWithDefault, toHIVPCRTestValueWithDefault, toHIVTestValueUniversalWithDefault, toHepatitisBTestValueWithDefault, toPartnerHIVTestValueWithDefault, toVaccinationValueWithDefault, vaccinationFormWithDefault)
import Pages.Prenatal.Activity.Model exposing (Msg(..), UltrasoundForm, emptyModel)
import Pages.Prenatal.Activity.Types exposing (GWGClassification(..), ImmunisationTask(..), NextStepsTask(..), PrePregnancyClassification(..), WarningPopupType(..))
import Pages.Prenatal.Activity.Update exposing (update)
import Pages.Prenatal.Activity.Utils exposing (activityCompleted, bmiToPrePregnancyClassification, generatePrenatalAssesmentForChw, generatePrenatalDiagnosesForNurse, immunisationTasksCompletedFromTotal, resolveGWGClassification, resolveGWGClassificationForHealthyStart, resolveGWGClassificationSincePreviousVisit, resolveNextStepsTasks, resolveWarningPopupContentForUrgentDiagnoses, setUltrasoundEDDDays, setUltrasoundEDDWeeks, setUltrasoundExecutionDate, setUltrasoundExecutionDateSelectorState, suicideRiskDiagnosedBySigns, toUltrasoundValueWithDefault, ultrasoundEDDDate, ultrasoundFormWithDefault, ultrasoundTasks, zscoreToPrePregnancyClassification)
import Pages.Prenatal.Encounter.Utils exposing (generateAssembledData, generateGravida, generatePara, skipObstetricHistorySecondStep)
import Pages.Prenatal.Model exposing (AssembledData, PrenatalEncounterPhase(..), PreviousEncounterData, emptyHealthEducationForm)
import Pages.Prenatal.ProgressReport.Model exposing (obstetricalDiagnoses)
import Pages.Prenatal.RecurrentActivity.Types
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.Utils exposing (diabetesDiagnoses, diagnosedHypertension, diagnosedPreviouslyAnyOf, diagnosesCausingHospitalReferralByPhase, diagnosisKnownAnyOf, healthEducationFormInputsAndTasksForNurse, resolveDiscordantCoupleStatus, resolvePartnerHIVTestResult, resolveRequiredMedicationsSet)
import RemoteData
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)
import TestFixtures
import Time
import Translate
import Translate.Model exposing (Language(..))



-- Expected classifications come from the WHO standards (the independent
-- oracle), not from the implementation:
--   * adult BMI categories: underweight <18.5, normal 18.5-25, overweight
--     25-30, obese >=30;
--   * BMI-for-age z-score (5-19y): thinness <-2, normal -2..+1, overweight
--     +1..+2, obese >+2.
-- Tests focus on the category boundaries, where a shifted cutoff would hide.


bmiToPrePregnancyClassificationTest : Test
bmiToPrePregnancyClassificationTest =
    describe "bmiToPrePregnancyClassification (WHO adult BMI categories)"
        [ test "17.0 -> underweight" <|
            \_ -> bmiToPrePregnancyClassification 17.0 |> Expect.equal PrePregnancyUnderWeight
        , test "18.4 (just below 18.5) -> underweight" <|
            \_ -> bmiToPrePregnancyClassification 18.4 |> Expect.equal PrePregnancyUnderWeight
        , test "18.5 (boundary) -> normal" <|
            \_ -> bmiToPrePregnancyClassification 18.5 |> Expect.equal PrePregnancyNormal
        , test "24.9 -> normal" <|
            \_ -> bmiToPrePregnancyClassification 24.9 |> Expect.equal PrePregnancyNormal
        , test "25.0 (boundary) -> overweight" <|
            \_ -> bmiToPrePregnancyClassification 25.0 |> Expect.equal PrePregnancyOverweight
        , test "29.9 -> overweight" <|
            \_ -> bmiToPrePregnancyClassification 29.9 |> Expect.equal PrePregnancyOverweight
        , test "30.0 (boundary) -> obese" <|
            \_ -> bmiToPrePregnancyClassification 30.0 |> Expect.equal PrePregnancyObesity
        , test "35.0 -> obese" <|
            \_ -> bmiToPrePregnancyClassification 35.0 |> Expect.equal PrePregnancyObesity
        ]


zscoreToPrePregnancyClassificationTest : Test
zscoreToPrePregnancyClassificationTest =
    describe "zscoreToPrePregnancyClassification (WHO BMI-for-age z-score bands)"
        [ test "-2.5 (thinness) -> underweight" <|
            \_ -> zscoreToPrePregnancyClassification -2.5 |> Expect.equal PrePregnancyUnderWeight
        , test "-2.0 (boundary) -> normal" <|
            \_ -> zscoreToPrePregnancyClassification -2.0 |> Expect.equal PrePregnancyNormal
        , test "+1.0 (boundary) -> normal" <|
            \_ -> zscoreToPrePregnancyClassification 1.0 |> Expect.equal PrePregnancyNormal
        , test "+1.5 -> overweight" <|
            \_ -> zscoreToPrePregnancyClassification 1.5 |> Expect.equal PrePregnancyOverweight
        , test "+2.0 (boundary) -> overweight" <|
            \_ -> zscoreToPrePregnancyClassification 2.0 |> Expect.equal PrePregnancyOverweight
        , test "+2.5 -> obese" <|
            \_ -> zscoreToPrePregnancyClassification 2.5 |> Expect.equal PrePregnancyObesity
        ]



-- HEALTHY START GESTATIONAL WEIGHT GAIN
--
-- Expected gain comes from the Healthy Start protocol (the independent
-- oracle): a woman not severely undernourished at booking is expected to gain
-- 60 g per day, one who was severely undernourished 73 g per day, and 23.5 g
-- per day covers the part of the period before 13 weeks. Gain is adequate when
-- it meets or exceeds the expected gain for the period.
--
-- Both weighings here fall after 13 weeks, so only the later rate applies:
-- 25 days at 60 g per day is an expected gain of 1.5 kg, 30 days one of 1.8 kg.


{-| Classify a gain from 60 kg to `currentWeight` over `days` ending on
`currentDate`. With an LMP 28 weeks back, both weighings are well past 13 weeks
of gestation.
-}
classifyHealthyStartGWG : PrePregnancyClassification -> Int -> Float -> Maybe GWGClassification
classifyHealthyStartGWG prePregnancyClassification days currentWeight =
    resolveGWGClassificationForHealthyStart currentDate
        prePregnancyClassification
        60.0
        (Date.add Date.Days -days currentDate)
        currentWeight
        (testAssembled28Weeks emptyPrenatalMeasurements)


resolveGWGClassificationForHealthyStartTest : Test
resolveGWGClassificationForHealthyStartTest =
    describe "resolveGWGClassificationForHealthyStart (Healthy Start expected daily gain)"
        [ test "over 25 days, a gain of 1.0 kg is below the expected 1.5 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "over 25 days, a gain of exactly the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 61.5
                    |> Expect.equal (Just GWGAdequate)
        , test "over 30 days, a gain of exactly the expected 1.8 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 30 61.8
                    |> Expect.equal (Just GWGAdequate)
        , test "over 25 days, a gain of 2.5 kg is above the expected 1.5 kg -> adequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 62.5
                    |> Expect.equal (Just GWGAdequate)
        , test "1.0 kg lost over the period -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyNormal 25 59.0
                    |> Expect.equal (Just GWGInadequate)
        , test "severely undernourished at booking: over 25 days, 1.5 kg is short of the expected 1.825 kg -> inadequate" <|
            \_ ->
                classifyHealthyStartGWG PrePregnancyUnderWeight 25 61.5
                    |> Expect.equal (Just GWGInadequate)
        , test "a gain of exactly the expected amount is adequate at every five-day interval up to 20 weeks" <|
            \_ ->
                -- At 60 g per day a five-day interval expects 0.3 kg, so these
                -- are the intervals where a woman exactly on target shows a
                -- whole tenth of a kilogram on the scale. The list is the
                -- intervals that came back anything other than adequate.
                List.range 1 28
                    |> List.filter
                        (\fiveDayBlocks ->
                            classifyHealthyStartGWG PrePregnancyNormal
                                (fiveDayBlocks * 5)
                                (60.0 + 0.3 * toFloat fiveDayBlocks)
                                /= Just GWGAdequate
                        )
                    |> Expect.equal []
        ]



-- HEALTHY START WEIGHT GAIN SINCE THE PREVIOUS VISIT
--
-- The observation period runs from the date of the previous nurse visit where
-- weight was recorded to the date of the current visit. Two other dates are
-- carried on purpose and must not move the period:
--   * the day the previous weight record was last saved (`dateMeasured`), which
--     every re-save or edit restamps;
--   * the day the current visit's weight is saved (`filledOn`), later than
--     the visit when the encounter is reopened. `hsVerdict` stores it as the
--     `dateMeasured` of the current visit's own Nutrition measurement, which
--     holds the weight being entered.
--     Tests about a reopened visit also set the current encounter's `endDate`.
-- The current visit's date lives on the current encounter (`startDate`), the
-- previous visit's date on `PreviousEncounterData.startDate`.
--
-- Rates (the Healthy Start protocol, as in the block above): 23.5 g per day
-- before 13 weeks from LMP, 60 g per day from 13 weeks onward, 73 g per day
-- from 13 weeks onward for a woman severely undernourished at booking.
--
-- Main example: previous visit 1 June 2020 at 60.0 kg, current visit 13 July
-- 2020, 42 days apart, both well past 13 weeks (LMP 1 February 2020, so 13
-- weeks falls on 2 May). Expected gain 42 x 0.06 = 2.52 kg.


{-| LMP for the main example: 13 weeks falls on 2 May 2020, before every
visit date used with it.
-}
hsExampleLmp : NominalDate
hsExampleLmp =
    Date.fromCalendarDate 2020 Time.Feb 1


hsMay1 : NominalDate
hsMay1 =
    Date.fromCalendarDate 2020 Time.May 1


hsJune1 : NominalDate
hsJune1 =
    Date.fromCalendarDate 2020 Time.Jun 1


hsJune11 : NominalDate
hsJune11 =
    Date.fromCalendarDate 2020 Time.Jun 11


hsJune20 : NominalDate
hsJune20 =
    Date.fromCalendarDate 2020 Time.Jun 20


hsJune22 : NominalDate
hsJune22 =
    Date.fromCalendarDate 2020 Time.Jun 22


hsJune25 : NominalDate
hsJune25 =
    Date.fromCalendarDate 2020 Time.Jun 25


hsJune29 : NominalDate
hsJune29 =
    Date.fromCalendarDate 2020 Time.Jun 29


hsJuly13 : NominalDate
hsJuly13 =
    Date.fromCalendarDate 2020 Time.Jul 13


hsJuly20 : NominalDate
hsJuly20 =
    Date.fromCalendarDate 2020 Time.Jul 20


{-| Nutrition measurements holding `weight`, as saved on `savedOn`.
-}
hsNutritionWithWeight : NominalDate -> Float -> PrenatalMeasurements
hsNutritionWithWeight savedOn weight =
    { emptyPrenatalMeasurements
        | nutrition =
            TestFixtures.wrapMeasurement savedOn
                { height = HeightInCm 160
                , weight = WeightInKg weight
                , muac = MuacInCm 25
                }
    }


{-| A previous nurse visit held on `visitDate`, whose weight record was last
saved on `savedOn`.
-}
hsPreviousVisitWithWeight : NominalDate -> NominalDate -> Float -> PreviousEncounterData
hsPreviousVisitWithWeight visitDate savedOn weight =
    { startDate = visitDate
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , measurements = hsNutritionWithWeight savedOn weight
    }


{-| A previous nurse visit held on `visitDate` where no weight was recorded:
no Nutrition measurement at all.
-}
hsPreviousVisitWithoutWeight : NominalDate -> PreviousEncounterData
hsPreviousVisitWithoutWeight visitDate =
    { startDate = visitDate
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , measurements = emptyPrenatalMeasurements
    }


{-| The current nurse visit, held on `visitDate`, with the given LMP and
previous nurse visits (oldest first, as `generateAssembledData` builds them).
-}
hsCurrentVisit : NominalDate -> NominalDate -> List PreviousEncounterData -> AssembledData
hsCurrentVisit lmp visitDate previousVisits =
    let
        base =
            testAssembled emptyPrenatalMeasurements

        encounter =
            base.encounter
    in
    { base
        | encounter = { encounter | startDate = visitDate }
        , globalLmpDate = Just lmp
        , nursePreviousEncountersData = previousVisits
    }


{-| The main example's current visit (13 July), after one previous visit on
1 June at 60.0 kg whose weight record was last saved on `savedOn`.
-}
hsExampleVisit : NominalDate -> AssembledData
hsExampleVisit savedOn =
    hsCurrentVisit hsExampleLmp hsJuly13 [ hsPreviousVisitWithWeight hsJune1 savedOn 60.0 ]


{-| The verdict when the current visit's weight, `currentWeight`, is saved on
`filledOn`. The save is recorded as the current visit's own Nutrition
measurement, dated `filledOn`. This is the only call of the function under test.
-}
hsVerdict : PrePregnancyClassification -> NominalDate -> AssembledData -> Float -> Maybe GWGClassification
hsVerdict prePregnancyClassification filledOn assembled currentWeight =
    let
        measurements =
            assembled.measurements

        withCurrentWeight =
            { assembled
                | measurements =
                    { measurements | nutrition = (hsNutritionWithWeight filledOn currentWeight).nutrition }
            }
    in
    resolveGWGClassificationSincePreviousVisit prePregnancyClassification currentWeight withCurrentWeight


{-| The current visit, reopened and ended again on `endDate`.
-}
hsEndedOn : NominalDate -> AssembledData -> AssembledData
hsEndedOn endDate assembled =
    let
        encounter =
            assembled.encounter
    in
    { assembled | encounter = { encounter | endDate = Just endDate } }


{-| Day `n` counted from `hsSplitLmp`. Day 91 is exactly 13 weeks.
-}
hsSplitDay : Int -> NominalDate
hsSplitDay n =
    Date.add Date.Days n hsSplitLmp


{-| LMP for the 13-week split scenarios.
-}
hsSplitLmp : NominalDate
hsSplitLmp =
    Date.fromCalendarDate 2020 Time.Mar 2


resolveGWGClassificationSincePreviousVisitTest : Test
resolveGWGClassificationSincePreviousVisitTest =
    describe "resolveGWGClassificationSincePreviousVisit (Healthy Start observation period)"
        [ test "At a Healthy Start visit, weight gain is judged over the days from the previous nurse visit where weight was recorded to the date of the current visit" <|
            \_ ->
                -- Previous visit 1 June, its weight record last saved 11 June;
                -- current visit 13 July, form filled 20 July. The period is
                -- still 1 June to 13 July, 42 days, expecting 2.52 kg: a gain
                -- of 2.52 kg is adequate, a gain of 2.4 kg is not.
                let
                    assembled =
                        hsExampleVisit hsJune11
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly20 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 assembled 62.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "Re-saving or editing the previous visit's weight on a later day does not change the period used at the next visit" <|
            \_ ->
                -- The 1 June weight was saved again, unchanged, on 29 June.
                -- The period stays 42 days (2.52 kg expected), so 1.0 kg is
                -- not adequate; a 14-day period would have expected 0.84 kg.
                hsVerdict PrePregnancyNormal hsJuly13 (hsExampleVisit hsJune29) 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "previous weight edited on a later day: the edited weight is measured over the period from the previous visit date" <|
            \_ ->
                -- The 1 June weight was corrected to 60.5 kg on 29 June. Over
                -- 42 days 2.52 kg is expected: 63.0 kg (2.5 kg gained) falls
                -- short, 63.1 kg (2.6 kg gained) meets it.
                let
                    assembled =
                        hsCurrentVisit hsExampleLmp hsJuly13 [ hsPreviousVisitWithWeight hsJune1 hsJune29 60.5 ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 63.0 |> Expect.equal (Just GWGInadequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 63.1 |> Expect.equal (Just GWGAdequate)
                    ]
                    ()
        , test "previous weight re-saved after the current visit's date does not change the period" <|
            \_ ->
                -- The 1 June weight was saved again on 20 July, a week after
                -- the current visit of 13 July began. The period is still
                -- 1 June to 13 July.
                hsVerdict PrePregnancyNormal hsJuly13 (hsExampleVisit hsJuly20) 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "Reopening the current visit on a later day and saving its weight gives the same verdict as saving it on the visit day" <|
            \_ ->
                -- A gain of exactly the expected 2.52 kg, saved on the visit
                -- day (13 July), and saved on 20 July after reopening, the
                -- visit then ended again that day.
                let
                    assembled =
                        hsExampleVisit hsJune1
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsEndedOn hsJuly20 assembled) 62.52 |> Expect.equal (Just GWGAdequate)
                    ]
                    ()
        , test "current visit ended on the visit day, reopened and its weight saved a week later: same verdict as on the visit day" <|
            \_ ->
                hsVerdict PrePregnancyNormal hsJuly20 (hsEndedOn hsJuly13 (hsExampleVisit hsJune1)) 62.52
                    |> Expect.equal (Just GWGAdequate)
        , test "Earlier visits with no weight recorded are skipped; the period starts at the most recent earlier visit that has one" <|
            \_ ->
                -- Visits on 1 May (58.0 kg) and 1 June (60.0 kg), then 22 June
                -- with no weight. The period starts on 1 June at 60.0 kg:
                -- 42 days, 2.52 kg expected.
                let
                    assembled =
                        hsCurrentVisit hsExampleLmp
                            hsJuly13
                            [ hsPreviousVisitWithWeight hsMay1 hsMay1 58.0
                            , hsPreviousVisitWithWeight hsJune1 hsJune1 60.0
                            , hsPreviousVisitWithoutWeight hsJune22
                            ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "visit without weight skipped, and the weighed visit's record was re-saved after it: period still starts on the weighed visit's date" <|
            \_ ->
                -- As above, but the 1 June weight record was last saved on
                -- 25 June, after the unweighed 22 June visit.
                let
                    assembled =
                        hsCurrentVisit hsExampleLmp
                            hsJuly13
                            [ hsPreviousVisitWithWeight hsJune1 hsJune25 60.0
                            , hsPreviousVisitWithoutWeight hsJune22
                            ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "the most recent weighed visit is chosen by visit date, even when an older visit's weight was re-saved later" <|
            \_ ->
                -- The 1 May weight (58.0 kg) was re-saved on 20 June, after
                -- the 1 June visit (60.0 kg). The period still starts on
                -- 1 June at 60.0 kg.
                let
                    assembled =
                        hsCurrentVisit hsExampleLmp
                            hsJuly13
                            [ hsPreviousVisitWithWeight hsMay1 hsJune20 58.0
                            , hsPreviousVisitWithWeight hsJune1 hsJune1 60.0
                            ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "earlier nurse visits exist but none recorded a weight -> no verdict" <|
            \_ ->
                hsVerdict PrePregnancyNormal
                    hsJuly13
                    (hsCurrentVisit hsExampleLmp hsJuly13 [ hsPreviousVisitWithoutWeight hsJune1, hsPreviousVisitWithoutWeight hsJune22 ])
                    61.0
                    |> Expect.equal Nothing
        , test "a weight recorded at an earlier CHW visit does not start the period; it starts at the previous nurse visit" <|
            \_ ->
                -- A CHW visit on 29 June recorded 61.0 kg. The period still
                -- runs from the 1 June nurse visit at 60.0 kg.
                let
                    base =
                        hsExampleVisit hsJune1

                    assembled =
                        { base | chwPreviousMeasurementsWithDates = [ ( hsJune29, ChwFirstEncounter, hsNutritionWithWeight hsJune29 61.0 ) ] }
                in
                hsVerdict PrePregnancyNormal hsJuly13 assembled 62.4
                    |> Expect.equal (Just GWGInadequate)
        , test "The 13-week split follows the visit dates: days before 13 weeks use the early-pregnancy rate, days from 13 weeks onward the later rate, even when the form is filled after 13 weeks" <|
            \_ ->
                -- Previous visit at 11 weeks (day 77, 55.0 kg), current visit
                -- at 15 weeks (day 105), form filled at 17 weeks (day 119).
                -- 14 days x 0.0235 + 14 days x 0.06 = 1.169 kg expected:
                -- 1.2 kg gained is adequate, 1.1 kg is not.
                let
                    assembled =
                        hsCurrentVisit hsSplitLmp (hsSplitDay 105) [ hsPreviousVisitWithWeight (hsSplitDay 77) (hsSplitDay 77) 55.0 ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 119) assembled 56.2 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 119) assembled 56.1 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "both visits before 13 weeks, form filled after 13 weeks: only the early-pregnancy rate applies" <|
            \_ ->
                -- Previous visit at 9 weeks (day 63, 55.0 kg), current visit at
                -- 12 weeks (day 84), form filled at 14 weeks (day 98).
                -- 21 days x 0.0235 = 0.4935 kg expected: 0.5 kg gained is
                -- adequate, 0.4 kg is not.
                let
                    assembled =
                        hsCurrentVisit hsSplitLmp (hsSplitDay 84) [ hsPreviousVisitWithWeight (hsSplitDay 63) (hsSplitDay 63) 55.0 ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 98) assembled 55.5 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 98) assembled 55.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "current visit exactly at 13 weeks, form filled 3 days later: every day of the period is before 13 weeks" <|
            \_ ->
                -- Previous visit at 10 weeks (day 70, 55.0 kg), current visit
                -- on day 91 (13 weeks), form filled on day 94. 21 days x
                -- 0.0235 = 0.4935 kg expected: 0.5 kg gained is adequate.
                hsVerdict PrePregnancyNormal
                    (hsSplitDay 94)
                    (hsCurrentVisit hsSplitLmp (hsSplitDay 91) [ hsPreviousVisitWithWeight (hsSplitDay 70) (hsSplitDay 70) 55.0 ])
                    55.5
                    |> Expect.equal (Just GWGAdequate)
        , test "previous visit exactly at 13 weeks: every day of the period uses the later rate" <|
            \_ ->
                -- Previous visit on day 91 (55.0 kg), current visit on day 112.
                -- 21 days x 0.06 = 1.26 kg expected: 1.3 kg gained is
                -- adequate, 1.2 kg is not.
                let
                    assembled =
                        hsCurrentVisit hsSplitLmp (hsSplitDay 112) [ hsPreviousVisitWithWeight (hsSplitDay 91) (hsSplitDay 91) 55.0 ]
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 112) assembled 56.3 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal (hsSplitDay 112) assembled 56.2 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "previous visit before 13 weeks, its weight re-saved after 13 weeks: the split still follows the previous visit's date" <|
            \_ ->
                -- Previous visit at 11 weeks (day 77, 55.0 kg), its weight
                -- re-saved on day 98; current visit on day 105. As in the
                -- split test, 1.169 kg is expected, so 1.1 kg gained falls
                -- short.
                hsVerdict PrePregnancyNormal
                    (hsSplitDay 105)
                    (hsCurrentVisit hsSplitLmp (hsSplitDay 105) [ hsPreviousVisitWithWeight (hsSplitDay 77) (hsSplitDay 98) 55.0 ])
                    56.1
                    |> Expect.equal (Just GWGInadequate)
        , test "Example: previous visit 1 June at 60.0 kg, current visit 13 July, 61.0 kg is not adequate, even if the 1 June weight was re-saved on 29 June or the current weight was entered on 20 July" <|
            \_ ->
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 (hsExampleVisit hsJune1) 61.0 |> Expect.equal (Just GWGInadequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 (hsExampleVisit hsJune29) 61.0 |> Expect.equal (Just GWGInadequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsExampleVisit hsJune1) 61.0 |> Expect.equal (Just GWGInadequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsExampleVisit hsJune29) 61.0 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "Example: the expected gain over 1 June to 13 July is 2.52 kg, also when the 1 June weight was re-saved on 29 June and the current weight entered on 20 July" <|
            \_ ->
                -- 2.52 kg gained meets the expected gain; 2.51 kg falls short.
                let
                    assembled =
                        hsExampleVisit hsJune29
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly20 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 assembled 62.51 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "severely undernourished at booking: over 1 June to 13 July 3.066 kg is expected, also when the 1 June weight was re-saved on 29 June" <|
            \_ ->
                -- 42 days x 0.073 = 3.066 kg: 3.1 kg gained is adequate,
                -- 3.0 kg is not. A 14-day period would expect only 1.022 kg.
                let
                    assembled =
                        hsExampleVisit hsJune29
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyUnderWeight hsJuly13 assembled 63.1 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyUnderWeight hsJuly13 assembled 63.0 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "Gain is adequate when the actual gain is equal to or greater than the expected gain" <|
            \_ ->
                -- Over 1 June to 13 July, 2.52 kg is expected. Equal, greater
                -- and far greater gains are adequate (Healthy Start has no
                -- "excessive" verdict); 10 g short is not.
                let
                    assembled =
                        hsExampleVisit hsJune1
                in
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 63.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 66.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly13 assembled 62.51 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        , test "The first visit of a pregnancy shows no weight-gain verdict" <|
            \_ ->
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly13 (hsCurrentVisit hsExampleLmp hsJuly13 []) 61.0 |> Expect.equal Nothing
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsCurrentVisit hsExampleLmp hsJuly13 []) 61.0 |> Expect.equal Nothing
                    ]
                    ()
        , test "first nurse visit after CHW visits that recorded a weight: no weight-gain verdict" <|
            \_ ->
                let
                    base =
                        hsCurrentVisit hsExampleLmp hsJuly13 []

                    assembled =
                        { base | chwPreviousMeasurementsWithDates = [ ( hsJune1, ChwFirstEncounter, hsNutritionWithWeight hsJune1 60.0 ) ] }
                in
                hsVerdict PrePregnancyNormal hsJuly13 assembled 61.0
                    |> Expect.equal Nothing
        ]



-- GESTATIONAL WEIGHT GAIN ON SITES WITHOUT HEALTHY START
--
-- The verdict compares the gain since before pregnancy with the gain expected
-- at the visit's pregnancy week: a first-trimester amount up to week 12, then
-- a weekly amount for every week after 12. For a normal pre-pregnancy BMI that
-- is 1 kg, then 0.42 kg per week. The ratio gain / expected grades it:
-- below 0.7 severely inadequate, below 0.9 inadequate, up to 1.25 adequate,
-- above that excessive.
--
-- Every scenario here holds the visit on one day and opens or saves its
-- Nutrition Assessment on a later one. The visit's date is the encounter's
-- start date. The later day is the date of the visit's own Nutrition
-- measurement, as a re-save records it.
-- The verdict must follow the visit's date, not the later day.


{-| LMP for these scenarios. `gwgWeek 20` is exactly 20 weeks after it.
-}
gwgLmp : NominalDate
gwgLmp =
    Date.fromCalendarDate 2020 Time.Jan 6


gwgWeek : Int -> NominalDate
gwgWeek weeks =
    Date.add Date.Weeks weeks gwgLmp


{-| Pre-pregnancy weight used throughout: a current weight of 64.0 kg is a
gain of 4.0 kg.
-}
gwgPrePregnancyWeight : Float
gwgPrePregnancyWeight =
    60.0


{-| A nurse visit held on `visitDate`, with no earlier visits.
-}
gwgVisit : NominalDate -> AssembledData
gwgVisit visitDate =
    hsCurrentVisit gwgLmp visitDate []


{-| The verdict when the visit's weight, `currentWeight`, is entered on
`filledOn`. The entry is recorded as the visit's own Nutrition measurement,
dated `filledOn`. This is the only call of the function under test.
-}
gwgVerdict : PrePregnancyClassification -> NominalDate -> AssembledData -> Float -> Maybe GWGClassification
gwgVerdict prePregnancyClassification filledOn assembled currentWeight =
    let
        measurements =
            assembled.measurements

        withCurrentWeight =
            { assembled
                | measurements =
                    { measurements | nutrition = (hsNutritionWithWeight filledOn currentWeight).nutrition }
            }
    in
    resolveGWGClassification prePregnancyClassification gwgPrePregnancyWeight currentWeight withCurrentWeight


resolveGWGClassificationTest : Test
resolveGWGClassificationTest =
    describe "resolveGWGClassification (sites without Healthy Start: gain since before pregnancy)"
        [ test "On a site without Healthy Start, the weight-gain verdict for a visit is graded at the pregnancy week of the visit's date, not of the day the form is opened or saved" <|
            \_ ->
                -- Normal BMI, visit at week 20, form opened at week 24.
                -- Week 20 expects 1 + 8 x 0.42 = 4.36 kg; week 24 would
                -- expect 6.04 kg. 6.0 kg gained is 1.38 of the week-20
                -- target: excessive. Against week 24 it would read adequate.
                gwgVerdict PrePregnancyNormal (gwgWeek 24) (gwgVisit (gwgWeek 20)) 66.0
                    |> Expect.equal (Just GWGExcessive)
        , test "Opening or re-saving an earlier visit's Nutrition Assessment, with the weights unchanged, gives the same verdict as on the visit day" <|
            \_ ->
                -- The same visit at week 20 with 4.0 kg gained, filled on the
                -- visit day and on four later days.
                let
                    assembled =
                        gwgVisit (gwgWeek 20)

                    onVisitDay =
                        gwgVerdict PrePregnancyNormal (gwgWeek 20) assembled 64.0
                in
                Expect.all
                    [ \_ -> onVisitDay |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (Date.add Date.Days 1 (gwgWeek 20)) assembled 64.0 |> Expect.equal onVisitDay
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 22) assembled 64.0 |> Expect.equal onVisitDay
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 30) assembled 64.0 |> Expect.equal onVisitDay
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 40) assembled 64.0 |> Expect.equal onVisitDay
                    ]
                    ()
        , test "Example: normal pre-pregnancy BMI, visit at week 20, 4.0 kg gained: Adequate, whether saved on the visit day or 4 weeks later" <|
            \_ ->
                -- 4.0 / 4.36 = 0.92: adequate. Against the week-24 target of
                -- 6.04 kg it would be 0.66: severely inadequate.
                let
                    assembled =
                        gwgVisit (gwgWeek 20)
                in
                Expect.all
                    [ \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 20) assembled 64.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) assembled 64.0 |> Expect.equal (Just GWGAdequate)
                    ]
                    ()
        , test "A visit in the first trimester (week 12 or earlier) is graded against the first-trimester target, even when re-saved after week 12" <|
            \_ ->
                -- Normal BMI: the first-trimester target is 1 kg, so 1.0 kg
                -- gained is adequate. Re-saved at week 16 the target would be
                -- 2.68 kg, at week 14 it would be 1.84 kg.
                Expect.all
                    [ \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 10) (gwgVisit (gwgWeek 10)) 61.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 16) (gwgVisit (gwgWeek 10)) 61.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 12) (gwgVisit (gwgWeek 12)) 61.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 14) (gwgVisit (gwgWeek 12)) 61.0 |> Expect.equal (Just GWGAdequate)
                    ]
                    ()
        , test "a visit at week 13, re-saved at week 17, is graded against the week-13 target, not the first-trimester one" <|
            \_ ->
                -- Week 13 expects 1.42 kg: 1.0 kg gained is 0.70, inadequate.
                -- The first-trimester target (1 kg) would make it adequate;
                -- the week-17 target (3.1 kg) severely inadequate.
                gwgVerdict PrePregnancyNormal (gwgWeek 17) (gwgVisit (gwgWeek 13)) 61.0
                    |> Expect.equal (Just GWGInadequate)
        , test "Changing the weight on the form still changes the verdict" <|
            \_ ->
                -- Visit at week 20 (4.36 kg expected), filled at week 24.
                let
                    assembled =
                        gwgVisit (gwgWeek 20)
                in
                Expect.all
                    [ \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) assembled 62.5 |> Expect.equal (Just GWGSeverelyInadequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) assembled 63.5 |> Expect.equal (Just GWGInadequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) assembled 64.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) assembled 66.0 |> Expect.equal (Just GWGExcessive)
                    ]
                    ()
        , test "The verdict stored on the visit, which the reports read, is the one shown on the form" <|
            \_ ->
                -- Saving stores whether the verdict shown is adequate. Saved
                -- at week 24 for a week-20 visit with 4.0 kg gained, that is
                -- the visit-day verdict, adequate, so the stored indicator
                -- reads adequate as it did on the visit day.
                let
                    assembled =
                        gwgVisit (gwgWeek 20)

                    storedAsAdequate filledOn =
                        gwgVerdict PrePregnancyNormal filledOn assembled 64.0
                            |> Maybe.map ((==) GWGAdequate)
                in
                Expect.all
                    [ \_ -> storedAsAdequate (gwgWeek 20) |> Expect.equal (Just True)
                    , \_ -> storedAsAdequate (gwgWeek 24) |> Expect.equal (Just True)
                    ]
                    ()
        , test "every pre-pregnancy classification: re-saved 4 weeks later, the verdict is the visit day's" <|
            \_ ->
                -- Visit at week 20, filled at week 24. Expected at week 20:
                -- underweight 2 + 8 x 0.51 = 6.08 kg, overweight 1 + 8 x 0.28
                -- = 3.24 kg, obese 0.5 + 8 x 0.22 = 2.26 kg. Each gain below
                -- is adequate there, and falls short of the week-24 target.
                let
                    assembled =
                        gwgVisit (gwgWeek 20)
                in
                Expect.all
                    [ \_ -> gwgVerdict PrePregnancyUnderWeight (gwgWeek 24) assembled 66.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyOverweight (gwgWeek 24) assembled 63.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyObesity (gwgWeek 24) assembled 62.5 |> Expect.equal (Just GWGAdequate)
                    ]
                    ()
        , test "visit reopened and ended again on a later day: still graded at the week of the visit's start date" <|
            \_ ->
                -- Visit started at week 20, ended again at week 24, form
                -- filled at week 26.
                gwgVerdict PrePregnancyNormal (gwgWeek 26) (hsEndedOn (gwgWeek 24) (gwgVisit (gwgWeek 20))) 64.0
                    |> Expect.equal (Just GWGAdequate)
        , test "form filled on the visit day: graded at the visit's week" <|
            \_ ->
                -- 4.0 kg at week 20: adequate; at week 24 the same gain on a
                -- week-24 visit is severely inadequate.
                Expect.all
                    [ \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 20) (gwgVisit (gwgWeek 20)) 64.0 |> Expect.equal (Just GWGAdequate)
                    , \_ -> gwgVerdict PrePregnancyNormal (gwgWeek 24) (gwgVisit (gwgWeek 24)) 64.0 |> Expect.equal (Just GWGSeverelyInadequate)
                    ]
                    ()
        , test "pregnancy with no LMP: no verdict" <|
            \_ ->
                let
                    base =
                        gwgVisit (gwgWeek 20)
                in
                gwgVerdict PrePregnancyNormal (gwgWeek 24) { base | globalLmpDate = Nothing } 64.0
                    |> Expect.equal Nothing
        , test "Sites with Healthy Start are unchanged" <|
            \_ ->
                -- Healthy Start grades the gain since the previous visit: from
                -- 1 June (60.0 kg) to 13 July, 42 days, 2.52 kg expected, also
                -- when the weight is entered on 20 July.
                Expect.all
                    [ \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsExampleVisit hsJune1) 62.52 |> Expect.equal (Just GWGAdequate)
                    , \_ -> hsVerdict PrePregnancyNormal hsJuly20 (hsExampleVisit hsJune1) 62.4 |> Expect.equal (Just GWGInadequate)
                    ]
                    ()
        ]



-- LAB-DRIVEN DIAGNOSIS FIXTURES
--
-- End-to-end tests for `generatePrenatalDiagnosesForNurse` on an initial-phase
-- nurse encounter (`NurseEncounter`). The oracle is the ANC Nurse tab of the
-- clinical sheet: a positive disease lab produces that disease's diagnosis.
-- The disease-level mapping is the oracle; the exact `*InitialPhase` variant
-- is structural (this is an initial-phase encounter, and all lab results are
-- entered with the "immediate result" prerequisite).


{-| The reference "current date" for the lab-driven diagnosis tests.
-}
currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| LMP ~20 weeks before `currentDate`, giving EGA ~20 weeks: past the week-12
gates that open the disease diagnoses, and mid-pregnancy (not postpartum).
-}
lmpDate : NominalDate
lmpDate =
    Date.add Date.Weeks -20 currentDate


{-| A dummy date used as `dateMeasured`/`executionDate` for measurements.
-}
dummyDate : NominalDate
dummyDate =
    currentDate


{-| Wrap a measurement `value` into the shape the `PrenatalMeasurements`
fields require, with `dummyDate` as `dateMeasured`.
-}
wrapMeasurement : value -> Maybe ( EntityUuid id, Measurement encounter value )
wrapMeasurement value =
    TestFixtures.wrapMeasurement dummyDate value


{-| The shared adult female fixture, but born 1990: age 30 at `currentDate`.
-}
testPerson : Person
testPerson =
    let
        base =
            TestFixtures.testPerson
    in
    { base | birthDate = Just (Date.fromCalendarDate 1990 Time.Jan 1) }


{-| An initial-phase nurse encounter (`NurseEncounter`), with no prior
diagnoses.
-}
testEncounter : PrenatalEncounter
testEncounter =
    { participant = toEntityUuid "dummy-participant"
    , startDate = currentDate
    , endDate = Nothing
    , encounterType = NurseEncounter
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , indicators = EverySet.empty
    , nextVisitDate = Nothing
    , deleted = False
    , shard = Nothing
    }


testParticipant : IndividualEncounterParticipant
testParticipant =
    TestFixtures.testParticipant currentDate AntenatalEncounter


{-| One reusable `AssembledData` for an initial-phase nurse encounter at
EGA ~20 weeks, parameterized only by the measurements under test.
-}
testAssembled : PrenatalMeasurements -> AssembledData
testAssembled measurements =
    { id = toEntityUuid "dummy-encounter"
    , encounter = testEncounter
    , participant = testParticipant
    , person = testPerson
    , measurements = measurements
    , nursePreviousEncountersData = []
    , chwPreviousMeasurementsWithDates = []
    , globalLmpDate = Just lmpDate
    , globalObstetricHistory = Nothing
    , vaccinationHistory = Dict.empty
    , vaccinationProgress = Dict.empty
    }


{-| LMP ~28 weeks before `currentDate`, putting EGA safely mid-window for the
moderate-preeclampsia [20,37) gate (away from either boundary).
-}
lmpDate28Weeks : NominalDate
lmpDate28Weeks =
    Date.add Date.Weeks -28 currentDate


{-| The `testAssembled` fixture with a CHW antenatal encounter type
(`ChwFirstEncounter`, a regular -- not postpartum -- CHW encounter). The CHW
assessment reads danger signs from the `.signs` set, which the non-postpartum
branch of `noDangerSigns` consults.
-}
chwAssembled : PrenatalMeasurements -> AssembledData
chwAssembled measurements =
    let
        base =
            testAssembled measurements

        encounter =
            base.encounter
    in
    { base | encounter = { encounter | encounterType = ChwFirstEncounter } }


{-| The `testAssembled` fixture with EGA ~28 weeks, used by the
moderate-preeclampsia group so the result is insensitive to the exact gate
boundary.
-}
testAssembled28Weeks : PrenatalMeasurements -> AssembledData
testAssembled28Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate28Weeks }


{-| LMP ~16 weeks before `currentDate`, giving EGA ~16 weeks (< 20), used by the
chronic-hypertension group: chronic hypertension is the EGA < 20 branch of the
hypertension matchers, gestational is the EGA >= 20 branch.
-}
lmpDate16Weeks : NominalDate
lmpDate16Weeks =
    Date.add Date.Weeks -16 currentDate


{-| The `testAssembled` fixture with EGA ~16 weeks (< 20 weeks).
-}
testAssembled16Weeks : PrenatalMeasurements -> AssembledData
testAssembled16Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate16Weeks }


{-| LMP ~38 weeks before `currentDate`, giving EGA ~38 weeks (>= 37), used by the
EGA37+ pre-eclampsia group. At EGA >= 37 the recurrent moderate/severe
pre-eclampsia diagnoses land on the `*EGA37Plus` emergency-referral variants.
-}
lmpDate38Weeks : NominalDate
lmpDate38Weeks =
    Date.add Date.Weeks -38 currentDate


{-| The `testAssembled` fixture with EGA ~38 weeks (>= 37 weeks).
-}
testAssembled38Weeks : PrenatalMeasurements -> AssembledData
testAssembled38Weeks measurements =
    let
        base =
            testAssembled measurements
    in
    { base | globalLmpDate = Just lmpDate38Weeks }



-- LAB VALUE BUILDERS / SETTERS
--
-- Each lab diagnosis at the initial phase requires the test to have been
-- "performed" (executionNote in {TestNoteRunToday, TestNoteRunPreviously,
-- TestNoteRunConfirmedByLabTech}),
-- a positive result, AND the `PrerequisiteImmediateResult` prerequisite
-- (so the result is available immediately, not deferred to case management).


immediateResultPrerequisites : Maybe (EverySet TestPrerequisite)
immediateResultPrerequisites =
    Just (EverySet.singleton PrerequisiteImmediateResult)


{-| What a test sent to the lab stores: the immediate-result question was
asked and answered No. Distinct from `Nothing`, which is what a test stores
when the question was never asked at all - a partner known to be positive, a
result taken from history, or a record from before the question existed.
-}
deferredResultPrerequisites : Maybe (EverySet TestPrerequisite)
deferredResultPrerequisites =
    Just (EverySet.singleton NoTestPrerequisites)


{-| HIV test with the given execution note, prerequisites and result. No
partner/HIV signs (so this never trips discordant-partnership).
-}
hivTestValueCustom : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> TestResult -> HIVTestValue
hivTestValueCustom executionNote prerequisites result =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = prerequisites
    , testResult = Just result
    , hivSigns = Nothing
    }


{-| HIV test, run today with the given positive/negative result, immediate
result.
-}
hivTestValueWith : TestResult -> HIVTestValue
hivTestValueWith =
    hivTestValueCustom TestNoteRunToday immediateResultPrerequisites


syphilisTestValueWith : TestResult -> SyphilisTestValue encounterId
syphilisTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result

    -- No symptoms -> plain Syphilis (not WithComplications / Neurosyphilis).
    , symptoms = Nothing
    , originatingEncounter = Nothing
    }


hepatitisBTestValueWith : TestResult -> HepatitisBTestValue encounterId
hepatitisBTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result
    , originatingEncounter = Nothing
    }


{-| Malaria RDT, run today with the given positive/negative result, immediate
result. Blood smear not taken (RDT path is used directly).
-}
malariaTestValueWith : TestResult -> MalariaTestValue
malariaTestValueWith result =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Just result
    , bloodSmearResult = BloodSmearNotTaken
    , bloodSmearOrdered = False
    }


withHIVTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTest result measurements =
    { measurements | hivTest = wrapMeasurement (hivTestValueWith result) }


withSyphilisTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withSyphilisTest result measurements =
    { measurements | syphilisTest = wrapMeasurement (syphilisTestValueWith result) }


withHepatitisBTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withHepatitisBTest result measurements =
    { measurements | hepatitisBTest = wrapMeasurement (hepatitisBTestValueWith result) }


withMalariaTest : TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withMalariaTest result measurements =
    { measurements | malariaTest = wrapMeasurement (malariaTestValueWith result) }


withCoartem : PrenatalMeasurements -> PrenatalMeasurements
withCoartem measurements =
    { measurements
        | medicationDistribution =
            wrapMeasurement
                { distributionSigns = EverySet.empty
                , nonAdministrationSigns = EverySet.empty
                , recommendedTreatmentSigns = Just (EverySet.singleton TreatmentCoartem)
                , avoidingGuidanceReason = Nothing
                , reinforceTreatmentSigns = Nothing
                }
    }



-- NON-IMMEDIATE-RESULT LAB VALUE BUILDERS / SETTERS
--
-- Identical to the positive immediate-result builders above (performed
-- executionNote + positive result), EXCEPT `testPrerequisites = Nothing`.
--
-- This is the documented representation for a "non-immediate" result:
-- `labTestWithImmediateResult` does `Maybe.andThen .testPrerequisites`, so a
-- `Nothing` prerequisite set makes `immediateResult` False, while
-- `testedPositiveAt` (which only reads executionNote + testResult) stays True.
-- A positive lab entered this way therefore fails the `*InitialPhase` matcher
-- (which requires `immediateResult`) and instead routes to the
-- `*RecurrentPhase` matcher (positive test WITHOUT the immediate-result
-- requirement) on the SAME initial-phase nurse encounter.


hivTestValueNonImmediate : HIVTestValue
hivTestValueNonImmediate =
    hivTestValueCustom TestNoteRunToday Nothing TestPositive


syphilisTestValueNonImmediate : SyphilisTestValue encounterId
syphilisTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , symptoms = Nothing
    , originatingEncounter = Nothing
    }


hepatitisBTestValueNonImmediate : HepatitisBTestValue encounterId
hepatitisBTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , originatingEncounter = Nothing
    }


malariaTestValueNonImmediate : MalariaTestValue
malariaTestValueNonImmediate =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , testResult = Just TestPositive
    , bloodSmearResult = BloodSmearNotTaken
    , bloodSmearOrdered = False
    }


withHIVTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNonImmediate measurements =
    { measurements | hivTest = wrapMeasurement hivTestValueNonImmediate }


withSyphilisTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withSyphilisTestNonImmediate measurements =
    { measurements | syphilisTest = wrapMeasurement syphilisTestValueNonImmediate }


withHepatitisBTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withHepatitisBTestNonImmediate measurements =
    { measurements | hepatitisBTest = wrapMeasurement hepatitisBTestValueNonImmediate }


withMalariaTestNonImmediate : PrenatalMeasurements -> PrenatalMeasurements
withMalariaTestNonImmediate measurements =
    { measurements | malariaTest = wrapMeasurement malariaTestValueNonImmediate }



-- DISCORDANT-PARTNERSHIP BUILDERS
--
-- The diagnosis has two sources: the partner's own HIV test, and the partner
-- signs recorded on the patient's own HIV test. The second question is drawn
-- on the HIV test form while the partner's own result is unavailable, so both
-- sources are live. The Initial/Recurrent split follows the source that
-- matched, so these builders vary the execution note and prerequisites of each
-- test on its own.


{-| Partner HIV test with the given execution note and prerequisites, positive,
partner not taking ARVs - the discordant-partnership condition. A partner known
to be positive is not tested, so no result is recorded for that note, and the
immediate-result question is never asked - the prerequisites stay `Nothing`.
-}
partnerHIVTestValuePositive : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PartnerHIVTestValue
partnerHIVTestValuePositive executionNote prerequisites =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = prerequisites
    , testResult =
        if executionNote == TestNoteKnownAsPositive then
            Nothing

        else
            Just TestPositive
    , hivSigns = Just (EverySet.singleton NoPrenatalHIVSign)
    }


withPartnerHIVTestPositive : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVTestPositive executionNote prerequisites measurements =
    { measurements | partnerHIVTest = wrapMeasurement (partnerHIVTestValuePositive executionNote prerequisites) }


{-| Partner HIV test entered by a lab technician, with the partner signs in the
state the given step leaves them: the lab technician's own save marks them
pending, since only the nurse answers the follow up questions, and the nurse's
follow ups replace that marker with the answers.
-}
withPartnerHIVTestByLabTech : TestResult -> EverySet PrenatalHIVSign -> PrenatalMeasurements -> PrenatalMeasurements
withPartnerHIVTestByLabTech result hivSigns =
    withPartnerHIVTest TestNoteRunConfirmedByLabTech deferredResultPrerequisites (Just result) (Just hivSigns)


withPartnerHIVTest :
    TestExecutionNote
    -> Maybe (EverySet TestPrerequisite)
    -> Maybe TestResult
    -> Maybe (EverySet PrenatalHIVSign)
    -> PrenatalMeasurements
    -> PrenatalMeasurements
withPartnerHIVTest executionNote prerequisites result hivSigns measurements =
    { measurements
        | partnerHIVTest =
            wrapMeasurement
                { executionNote = executionNote
                , executionDate = Just dummyDate
                , testPrerequisites = prerequisites
                , testResult = result
                , hivSigns = hivSigns
                }
    }


{-| Patient's HIV test, negative, carrying the given partner signs.
-}
withHIVTestNegativeWithSigns : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> EverySet PrenatalHIVSign -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNegativeWithSigns executionNote prerequisites hivSigns measurements =
    let
        value =
            hivTestValueCustom executionNote prerequisites TestNegative
    in
    { measurements | hivTest = wrapMeasurement { value | hivSigns = Just hivSigns } }


withHIVTestNegative : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestNegative executionNote prerequisites measurements =
    { measurements | hivTest = wrapMeasurement (hivTestValueCustom executionNote prerequisites TestNegative) }


{-| Patient's HIV test, negative, carrying the partner signs: partner positive,
not taking ARVs.
-}
withHIVTestPartnerPositiveSigns : TestExecutionNote -> Maybe (EverySet TestPrerequisite) -> PrenatalMeasurements -> PrenatalMeasurements
withHIVTestPartnerPositiveSigns executionNote prerequisites =
    withHIVTestNegativeWithSigns executionNote prerequisites (EverySet.singleton PartnerHIVPositive)


{-| Patient's HIV test answered as known as positive, still carrying the
negative result and the partner signs of the test that answer replaced.
-}
withHIVTestKnownAsPositiveCarryingSigns : PrenatalMeasurements -> PrenatalMeasurements
withHIVTestKnownAsPositiveCarryingSigns =
    withHIVTestNegativeWithSigns TestNoteKnownAsPositive immediateResultPrerequisites (EverySet.singleton PartnerHIVPositive)


{-| Hemoglobin test, run today with the given count, immediate result. The
anemia/malaria-with-anemia diagnoses gate on `immediateResult .hemoglobinTest`,
so the `PrerequisiteImmediateResult` prerequisite is required just like the
disease labs.
-}
hemoglobinTestValueWith : Float -> HemoglobinTestValue
hemoglobinTestValueWith count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , hemoglobinCount = Just count
    }


withHemoglobin : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHemoglobin count measurements =
    { measurements | hemoglobinTest = wrapMeasurement (hemoglobinTestValueWith count) }


{-| Hemoglobin test with the given count entered NON-immediately
(`testPrerequisites = Nothing`). `resolveHemoglobinCount` reads `hemoglobinCount`
directly, so the count is still available, but `immediateResult .hemoglobinTest`
is False -- so the `*InitialPhase` anemia matcher (which requires the immediate
result) fails and the diagnosis routes to the `*RecurrentPhase` variant.
-}
hemoglobinTestValueNonImmediate : Float -> HemoglobinTestValue
hemoglobinTestValueNonImmediate count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , hemoglobinCount = Just count
    }


withHemoglobinNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHemoglobinNonImmediate count measurements =
    { measurements | hemoglobinTest = wrapMeasurement (hemoglobinTestValueNonImmediate count) }


{-| Vitals with the given systolic/diastolic blood pressure.
-}
withVitals : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withVitals sys dia measurements =
    { measurements | vitals = wrapMeasurement (TestFixtures.vitalsValueWith sys dia) }


{-| Vitals with a normal initial reading (120/80, so the _initial_ BP is not
itself high) and the given REPEATED systolic/diastolic blood pressure. The
recurrent-phase preeclampsia/hypertension matchers read `sysRepeated`/
`diaRepeated`; `repeatedTestForMarginalBloodPressure` needs BOTH present and
fires when `diaRepeated >= 90 || sysRepeated >= 140`.
-}
vitalsValueRepeatedWith : Float -> Float -> VitalsValue
vitalsValueRepeatedWith sysRepeated diaRepeated =
    { sys = Just 120
    , dia = Just 80
    , heartRate = Nothing
    , respiratoryRate = Nothing
    , bodyTemperature = Nothing
    , sysRepeated = Just sysRepeated
    , diaRepeated = Just diaRepeated
    }


withVitalsRepeated : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
withVitalsRepeated sysRepeated diaRepeated measurements =
    { measurements | vitals = wrapMeasurement (vitalsValueRepeatedWith sysRepeated diaRepeated) }


{-| Urine dipstick with the given protein level, immediate result.
`highUrineProteinInitialPhase` also gates on `PrerequisiteImmediateResult`.
-}
urineDipstickValueWith : ProteinValue -> UrineDipstickTestValue
urineDipstickValueWith protein =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , protein = Just protein
    , ph = Nothing
    , glucose = Nothing
    , leukocytes = Nothing
    , nitrite = Nothing
    , urobilinogen = Nothing
    , haemoglobin = Nothing
    , ketone = Nothing
    , bilirubin = Nothing
    }


withUrineProtein : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
withUrineProtein protein measurements =
    { measurements | urineDipstickTest = wrapMeasurement (urineDipstickValueWith protein) }


{-| Random blood sugar test with the given sugar count, entered NON-immediately.

`diabetesBySugarCount` reads `PrerequisiteFastFor12h`: when present it diagnoses
diabetes at sugarCount > 126; here the prerequisite set holds ONLY
`PrerequisiteFastFor12h` (NOT `PrerequisiteImmediateResult`), so a count above
126 satisfies `diabetesBySugarCount` while `immediateResult .randomBloodSugarTest`
stays False. The `*InitialPhase` diabetes matcher (which requires the immediate
result via `diabetesDiagnosedInitialPhase`) therefore fails and the diagnosis
routes to the `*RecurrentPhase` variant, which gates on `diabetesDiagnosedAnyPhase`
(no immediate-result requirement).

-}
randomBloodSugarValueNonImmediate : Float -> RandomBloodSugarTestValue encounterId
randomBloodSugarValueNonImmediate count =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Just (EverySet.singleton PrerequisiteFastFor12h)
    , sugarCount = Just count
    , originatingEncounter = Nothing
    }


withRandomBloodSugarNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withRandomBloodSugarNonImmediate count measurements =
    { measurements | randomBloodSugarTest = wrapMeasurement (randomBloodSugarValueNonImmediate count) }


{-| Blood group / Rhesus test with the given rhesus result, entered
NON-immediately. `rhesusNegativeDiagnosed` reads `.rhesus` directly (no immediate
requirement), but `immediateResult .bloodGpRsTest` is False, so the
`DiagnosisRhesusNegativeInitialPhase` matcher fails and the diagnosis routes to
`DiagnosisRhesusNegativeRecurrentPhase`.
-}
bloodGpRsValueNonImmediate : Rhesus -> BloodGpRsTestValue encounterId
bloodGpRsValueNonImmediate rhesus =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , bloodGroup = Nothing
    , rhesus = Just rhesus
    , originatingEncounter = Nothing
    }


withBloodGpRsNonImmediate : Rhesus -> PrenatalMeasurements -> PrenatalMeasurements
withBloodGpRsNonImmediate rhesus measurements =
    { measurements | bloodGpRsTest = wrapMeasurement (bloodGpRsValueNonImmediate rhesus) }


{-| HIV PCR test carrying the given viral load, entered NON-immediately.
`hivDetectableViralLoadDiagnosed` reads `.hivViralLoad` and fires when it is
20 or above (a detectable viral load); with `testPrerequisites = Nothing`,
`immediateResult .hivPCRTest` is False, so the
`DiagnosisHIVDetectableViralLoadInitialPhase` matcher fails and the diagnosis
routes to `DiagnosisHIVDetectableViralLoadRecurrentPhase`.
-}
hivPCRValueNonImmediate : Float -> HIVPCRTestValue
hivPCRValueNonImmediate viralLoad =
    { executionNote = TestNoteRunToday
    , executionDate = Just dummyDate
    , testPrerequisites = Nothing
    , hivViralLoadStatus = Just ViralLoadDetectable
    , hivViralLoad = Just viralLoad
    }


withHIVPCRNonImmediate : Float -> PrenatalMeasurements -> PrenatalMeasurements
withHIVPCRNonImmediate viralLoad measurements =
    { measurements | hivPCRTest = wrapMeasurement (hivPCRValueNonImmediate viralLoad) }


{-| Core physical exam carrying the given lungs/abdomen severe-preeclampsia
signs (everything else empty). `severePreeclampsiaSigns` reads
`Wheezes`/`Crackles` in `lungs` or `TPRightUpper` in `abdomen`.
-}
corePhysicalExamValueWith : EverySet LungsCPESign -> EverySet AbdomenCPESign -> CorePhysicalExamValue
corePhysicalExamValueWith lungs abdomen =
    { hairHead = EverySet.empty
    , eyes = EverySet.empty
    , heart = EverySet.empty
    , heartMurmur = False
    , neck = EverySet.empty
    , lungs = lungs
    , abdomen = abdomen
    , hands = EverySet.empty
    , legs = EverySet.empty
    }


{-| Core physical exam carrying a severe-preeclampsia sign (`TPRightUpper` in
the abdomen), so `severePreeclampsiaSigns` fires.
-}
withSeverePreeclampsiaSigns : PrenatalMeasurements -> PrenatalMeasurements
withSeverePreeclampsiaSigns measurements =
    { measurements
        | corePhysicalExam =
            wrapMeasurement
                (corePhysicalExamValueWith EverySet.empty (EverySet.singleton TPRightUpper))
    }


{-| Danger-signs measurement carrying the given antenatal danger signs.
-}
dangerSignsValueWith : EverySet DangerSign -> DangerSignsValue
dangerSignsValueWith signs =
    { signs = signs
    , postpartumMother = EverySet.empty
    , postpartumChild = EverySet.empty
    }


withDangerSigns : EverySet DangerSign -> PrenatalMeasurements -> PrenatalMeasurements
withDangerSigns signs measurements =
    { measurements | dangerSigns = wrapMeasurement (dangerSignsValueWith signs) }


{-| Run the function under test on an initial-phase nurse encounter with the
given measurements. Hemoglobin is left unset throughout, so malaria stays at
the plain (non-anemia) variant.
-}
diagnoseNurse : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse measurements =
    generatePrenatalDiagnosesForNurse (testAssembled measurements)


{-| Like `diagnoseNurse`, after one earlier nurse encounter, four weeks back,
holding the given diagnoses and measurements.
-}
diagnoseNurseAfter : List PrenatalDiagnosis -> PrenatalMeasurements -> PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurseAfter previousDiagnoses previousMeasurements measurements =
    let
        assembled =
            testAssembled measurements

        previousEncounter =
            { startDate = Date.add Date.Weeks -4 currentDate
            , diagnoses = EverySet.fromList previousDiagnoses
            , pastDiagnoses = EverySet.empty
            , measurements = previousMeasurements
            }
    in
    generatePrenatalDiagnosesForNurse
        { assembled | nursePreviousEncountersData = [ previousEncounter ] }


{-| The four lab-driven disease diagnoses under test, in their plain
initial-phase form.
-}
diseaseDiagnoses : List PrenatalDiagnosis
diseaseDiagnoses =
    [ DiagnosisHIVInitialPhase
    , DiagnosisSyphilisInitialPhase
    , DiagnosisMalariaInitialPhase
    , DiagnosisHepatitisBInitialPhase
    ]


generatePrenatalDiagnosesForNurseLabsTest : Test
generatePrenatalDiagnosesForNurseLabsTest =
    describe "generatePrenatalDiagnosesForNurse - lab-driven disease diagnoses (initial-phase nurse encounter, ANC Nurse sheet)"
        [ test "HIV Lab = Positive -> DiagnosisHIVInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisHIVInitialPhase
                    |> Expect.equal True
        , test "Syphilis Lab = Positive -> DiagnosisSyphilisInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withSyphilisTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisSyphilisInitialPhase
                    |> Expect.equal True
        , test "Malaria RDT = Positive -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "Hep B Lab = Positive -> DiagnosisHepatitisBInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHepatitisBTest TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisHepatitisBInitialPhase
                    |> Expect.equal True
        , test "all four disease labs negative -> none of the four disease diagnoses present (ANC Nurse sheet negative controls)" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVTest TestNegative
                            |> withSyphilisTest TestNegative
                            |> withMalariaTest TestNegative
                            |> withHepatitisBTest TestNegative
                            |> diagnoseNurse
                in
                List.filter (\diagnosis -> EverySet.member diagnosis diagnoses) diseaseDiagnoses
                    |> Expect.equal []
        ]



-- GROUP 1 -- CHW ASSESSMENT
--
-- `generatePrenatalAssesmentForChw` collapses to: any danger sign present ->
-- HighRisk, otherwise Normal. Oracle: ANC CHW tab ("any danger sign present ->
-- refer"). Uses a regular CHW antenatal encounter (`ChwFirstEncounter`), whose
-- danger signs come from the `.signs` set of the danger-signs measurement.


generatePrenatalAssesmentForChwTest : Test
generatePrenatalAssesmentForChwTest =
    describe "generatePrenatalAssesmentForChw - CHW danger-sign assessment (ANC CHW tab)"
        [ test "no danger-signs measurement -> AssesmentNormalPregnancy" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> chwAssembled
                    |> generatePrenatalAssesmentForChw
                    |> Expect.equal AssesmentNormalPregnancy
        , test "danger sign present (VaginalBleeding) -> AssesmentHighRiskPregnancy" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withDangerSigns (EverySet.singleton VaginalBleeding)
                    |> chwAssembled
                    |> generatePrenatalAssesmentForChw
                    |> Expect.equal AssesmentHighRiskPregnancy
        ]



-- GROUP 2 -- ANEMIA (nurse)
--
-- Hb comes from `measurements.hemoglobinTest`. Oracle: WHO pregnancy / Labs
-- tab -- severe <7, moderate 7..<11, none >=11. Anemia only fires when malaria
-- is NOT positive (here malaria is left unset, so never positive).


generatePrenatalDiagnosesForNurseAnemiaTest : Test
generatePrenatalDiagnosesForNurseAnemiaTest =
    describe "generatePrenatalDiagnosesForNurse - anemia by hemoglobin count (no malaria; WHO Labs tab)"
        [ test "Hb 9 (7..<11), no malaria -> DiagnosisModerateAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHemoglobin 9
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisModerateAnemiaInitialPhase
                    |> Expect.equal True
        , test "Hb 6 (<7), no malaria, no anemia-complication danger signs -> DiagnosisSevereAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHemoglobin 6
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisSevereAnemiaInitialPhase
                    |> Expect.equal True
        , test "Hb 12 (>=11) -> neither moderate nor severe anemia present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobin 12
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                , EverySet.member DiagnosisSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( False, False )
        ]



-- GROUP 3 -- MALARIA WITH ANEMIA (nurse)
--
-- Positive malaria RDT combined with an anemic Hb. Hb 7..<11 -> moderate
-- (MalariaWithAnemia); Hb <7 -> MalariaWithSevereAnemia. Plain anemia does
-- not fire because anemia requires malaria to be negative (expected).


generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest : Test
generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest =
    let
        -- RDT positive and Hb 9: malaria with mild to moderate anemia.
        currentMalariaWithAnemia =
            emptyPrenatalMeasurements
                |> withMalariaTest TestPositive
                |> withHemoglobin 9

        -- Every malaria diagnosis, so a second one firing alongside shows up.
        malariaVariants =
            EverySet.toList
                >> List.filter
                    (\diagnosis ->
                        List.member diagnosis
                            [ DiagnosisMalariaInitialPhase
                            , DiagnosisMalariaRecurrentPhase
                            , DiagnosisMalariaMedicatedContinuedInitialPhase
                            , DiagnosisMalariaMedicatedContinuedRecurrentPhase
                            , DiagnosisMalariaWithAnemiaInitialPhase
                            , DiagnosisMalariaWithAnemiaRecurrentPhase
                            , DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase
                            , DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase
                            , DiagnosisMalariaWithSevereAnemiaInitialPhase
                            , DiagnosisMalariaWithSevereAnemiaRecurrentPhase
                            ]
                    )
    in
    describe "generatePrenatalDiagnosesForNurse - malaria combined with anemia"
        [ test "malaria RDT positive + Hb 9 (7..<11) -> DiagnosisMalariaWithAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 9
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaWithAnemiaInitialPhase
                    |> Expect.equal True
        , test "malaria RDT positive + Hb 6 (<7) -> DiagnosisMalariaWithSevereAnemiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 6
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaWithSevereAnemiaInitialPhase
                    |> Expect.equal True
        , test "after a treated malaria with severe anemia, Hb 9 -> the medicated-continued diagnosis only" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ]
        , test "after a treated malaria with severe anemia, Hb 9 at a recurrent phase -> the medicated-continued diagnosis only" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTestNonImmediate
                    |> withHemoglobinNonImmediate 9
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaRecurrentPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase ]
        , test "after a treated malaria with anemia, Hb 9 -> the medicated-continued diagnosis only" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithAnemiaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ]
        , test "after an untreated malaria with severe anemia, Hb 9 -> a new episode" <|
            \_ ->
                currentMalariaWithAnemia
                    |> diagnoseNurseAfter [ DiagnosisMalariaWithSevereAnemiaInitialPhase ] emptyPrenatalMeasurements
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaWithAnemiaInitialPhase ]
        , test "after a treated malaria, Hb 12 -> the medicated-continued diagnosis only" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMalariaTest TestPositive
                    |> withHemoglobin 12
                    |> diagnoseNurseAfter [ DiagnosisMalariaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    |> malariaVariants
                    |> Expect.equal [ DiagnosisMalariaMedicatedContinuedInitialPhase ]
        ]



-- GROUP 4 -- MODERATE PRE-ECLAMPSIA (nurse)
--
-- `DiagnosisModeratePreeclampsiaInitialPhase` requires EGA in [20,37) AND high
-- blood pressure AND (high urine protein OR edema). FINDING-adjacent nuance:
-- the code's `highBloodPressureCondition dia sys = dia >= 110 || sys >= 160`
-- uses the SEVERE BP threshold (>=160/110), NOT the clinical >=140/90 starting
-- point at which pre-eclampsia screening normally begins. EGA is set to ~28
-- weeks so the result is insensitive to the [20,37) boundaries.


diagnoseNurse28Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse28Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled28Weeks measurements)


{-| Run the function under test at EGA ~16 weeks (< 20).
-}
diagnoseNurse16Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse16Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled16Weeks measurements)


{-| Run the function under test at EGA ~38 weeks (>= 37).
-}
diagnoseNurse38Weeks : PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseNurse38Weeks measurements =
    generatePrenatalDiagnosesForNurse (testAssembled38Weeks measurements)


generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest : Test
generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest =
    describe "generatePrenatalDiagnosesForNurse - moderate pre-eclampsia (EGA ~28 weeks)"
        [ test "BP 165/115 (>=160/110) + urine protein +1 -> DiagnosisModeratePreeclampsiaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal True

        -- FINDING: marginal BP 145/95 is hypertensive by the clinical
        -- >=140/90 definition, but the code's "moderate pre-eclampsia" gate
        -- demands the SEVERE >=160/110 threshold, so no diagnosis fires here.
        , test "BP 145/95 (marginal, <160/110) + urine protein +1 -> DiagnosisModeratePreeclampsiaInitialPhase NOT present (severe BP threshold)" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 145 95
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal False
        ]



-- GROUP A -- RECURRENT-PHASE DISEASE DIAGNOSES (nurse)
--
-- KEY MECHANISM: every non-postpartum nurse encounter offers BOTH the
-- `*InitialPhase` and `*RecurrentPhase` variant of each disease in its
-- candidate list (`resolveLabResultsAndExaminationDiagnoses`). The Initial
-- variant requires the lab's result to be immediate
-- (`testedPositiveAt ... && immediateResult ...`); the Recurrent variant
-- matches a positive test WITHOUT that requirement, additionally gated on
-- `not (diagnosed the Initial variant)`. So a positive lab entered WITHOUT the
-- `PrerequisiteImmediateResult` prerequisite (here: `testPrerequisites =
-- Nothing`) fails Initial and lands on Recurrent on the SAME initial-phase
-- `NurseEncounter`.
--
-- Oracle = ANC Nurse tab: a positive disease lab produces that disease's
-- diagnosis. The disease-level mapping (HIV+ -> an HIV diagnosis) is the
-- oracle; that a NON-immediate result lands on the Recurrent (not Initial)
-- variant is the structural assertion.


generatePrenatalDiagnosesForNurseRecurrentLabsTest : Test
generatePrenatalDiagnosesForNurseRecurrentLabsTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase disease diagnoses (positive lab WITHOUT immediate result)"
        [ test "HIV positive, non-immediate -> DiagnosisHIVRecurrentPhase present AND DiagnosisHIVInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHIVRecurrentPhase diagnoses
                , EverySet.member DiagnosisHIVInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Syphilis positive, non-immediate -> DiagnosisSyphilisRecurrentPhase present AND DiagnosisSyphilisInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withSyphilisTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisSyphilisRecurrentPhase diagnoses
                , EverySet.member DiagnosisSyphilisInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Malaria RDT positive, non-immediate -> DiagnosisMalariaRecurrentPhase present AND DiagnosisMalariaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Hep B positive, non-immediate -> DiagnosisHepatitisBRecurrentPhase present AND DiagnosisHepatitisBInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHepatitisBTestNonImmediate
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHepatitisBRecurrentPhase diagnoses
                , EverySet.member DiagnosisHepatitisBInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP B -- RECURRENT MODERATE PRE-ECLAMPSIA (nurse)
--
-- `moderatePreeclampsiaByMeasurementsRecurrentPhase` is
--   (highBloodPressure && highUrineProtein)
--     || (repeatedTestForMarginalBloodPressure && (edema || highUrineProtein)).
-- Here the *initial* reading is normal (120/80, so `highBloodPressure` is
-- False and the Initial-phase variant does not fire), but the REPEATED reading
-- is marginal (`sysRepeated` 145 >= 140), so `repeatedTestForMarginalBloodPressure`
-- is True; combined with urine protein +1 this satisfies the recurrent
-- measurement condition. EGA ~28 weeks sits inside the [20,37) gate.
--
-- This fires on the plain initial-phase `NurseEncounter`: that encounter's
-- candidate list includes `DiagnosisModeratePreeclampsiaRecurrentPhase`, the
-- Initial variant does not fire (normal initial BP), there is no prior
-- diagnosis, and in `applyDiagnosesHierarchy` moderate-preeclampsia-recurrent
-- (rank 11) outranks the competing gestational-hypertension-after-recheck
-- (rank 1) that the marginal repeated BP also produces.


generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent moderate pre-eclampsia (EGA ~28 weeks)"
        [ test "repeated BP marginal (sysRepeated 145) + urine protein +1, normal initial BP -> DiagnosisModeratePreeclampsiaRecurrentPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhase
                    |> Expect.equal True
        ]



-- GROUP C -- RECURRENT-PHASE ANEMIA / MALARIA-WITH-ANEMIA (nurse)
--
-- Same mechanism as GROUP A: the hemoglobin (and malaria) result is entered
-- NON-immediately (`testPrerequisites = Nothing`). `resolveHemoglobinCount`
-- reads the count regardless, so the anemia condition still matches, but
-- `immediateResult .hemoglobinTest` is False, so the `*InitialPhase` matcher
-- fails and the diagnosis routes to the `*RecurrentPhase` variant.
--
-- Oracle = WHO Labs tab: severe Hb < 7, moderate 7..<11; combined with a
-- positive malaria RDT -> the malaria-with-(severe-)anemia diagnoses.


generatePrenatalDiagnosesForNurseAnemiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseAnemiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase anemia / malaria-with-anemia (non-immediate labs)"
        [ test "Hb 9 (7..<11) non-immediate, no malaria -> DiagnosisModerateAnemiaRecurrentPhase present AND DiagnosisModerateAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobinNonImmediate 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisModerateAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "Hb 6 (<7) non-immediate, no malaria, no anemia-complication signs -> DiagnosisSevereAnemiaRecurrentPhase present AND DiagnosisSevereAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHemoglobinNonImmediate 6
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisSevereAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "malaria RDT positive + Hb 9 (7..<11), both non-immediate -> DiagnosisMalariaWithAnemiaRecurrentPhase present AND DiagnosisMalariaWithAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> withHemoglobinNonImmediate 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaWithAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "malaria RDT positive + Hb 6 (<7), both non-immediate -> DiagnosisMalariaWithSevereAnemiaRecurrentPhase present AND DiagnosisMalariaWithSevereAnemiaInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withMalariaTestNonImmediate
                            |> withHemoglobinNonImmediate 6
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithSevereAnemiaRecurrentPhase diagnoses
                , EverySet.member DiagnosisMalariaWithSevereAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP D -- RECURRENT-PHASE DIABETES / RHESUS (nurse, non-immediate labs)
--
-- Diabetes: `diabetesBySugarCount` reads `PrerequisiteFastFor12h` (here the
-- ONLY prerequisite, so the result is NON-immediate); a count > 126 satisfies
-- it. The `*InitialPhase` matcher gates on `immediateResult`, so it fails, and
-- the diagnosis routes to the recurrent variant. The EGA gate splits the
-- recurrent diabetes diagnosis: EGA <= 20 -> DiagnosisDiabetesRecurrentPhase;
-- EGA > 20 -> DiagnosisGestationalDiabetesRecurrentPhase. `diagnoseNurse` is at
-- EGA exactly 20 (<= 20), so the code produces DiagnosisDiabetesRecurrentPhase.
--
-- Rhesus: `rhesusNegativeDiagnosed` reads `.rhesus == RhesusNegative` directly;
-- non-immediate entry routes to DiagnosisRhesusNegativeRecurrentPhase.


generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest : Test
generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent-phase diabetes / rhesus (non-immediate labs)"
        [ test "random blood sugar 200 (fasting, > 126) non-immediate, EGA <= 20 -> DiagnosisDiabetesRecurrentPhase present AND DiagnosisDiabetesInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withRandomBloodSugarNonImmediate 200
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisDiabetesRecurrentPhase diagnoses
                , EverySet.member DiagnosisDiabetesInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        , test "blood group / Rh test rhesus = RhesusNegative, non-immediate -> DiagnosisRhesusNegativeRecurrentPhase present AND DiagnosisRhesusNegativeInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withBloodGpRsNonImmediate RhesusNegative
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisRhesusNegativeRecurrentPhase diagnoses
                , EverySet.member DiagnosisRhesusNegativeInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]



-- GROUP E -- HYPERTENSION AFTER RECHECK (nurse)
--
-- A repeated marginal BP reading (`sysRepeated` 145 >= 140, normal initial
-- reading) with no proteinuria fires `repeatedTestForMarginalBloodPressure`,
-- which (without proteinuria/edema) yields hypertension-after-recheck rather
-- than recurrent pre-eclampsia. The EGA gate splits the two variants:
--   * EGA < 20  -> DiagnosisChronicHypertensionAfterRecheck
--   * EGA >= 20 -> DiagnosisGestationalHypertensionAfterRecheck
-- That EGA cutoff (chronicHypertensionByMeasurementsAfterRecheck uses
-- egaWeeks < 20; gestationalHypertensionByMeasurementsAfterRecheck uses
-- egaWeeks >= 20) is the ONLY thing distinguishing chronic from gestational
-- here -- no prior-encounter / medical-history data is needed.
--
-- These are the surviving (top) blood-pressure diagnosis after
-- `applyHypertensionlikeDiagnosesHierarchy`: with normal initial BP and no
-- proteinuria, no higher-ranked BP diagnosis competes.


generatePrenatalDiagnosesForNurseHypertensionRecheckTest : Test
generatePrenatalDiagnosesForNurseHypertensionRecheckTest =
    describe "generatePrenatalDiagnosesForNurse - hypertension after recheck (repeated marginal BP, no proteinuria)"
        [ test "EGA >= 20, repeated BP marginal (sysRepeated 145), normal initial BP, no proteinuria -> DiagnosisGestationalHypertensionAfterRecheck present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisGestationalHypertensionAfterRecheck
                    |> Expect.equal True
        , test "EGA < 20 (~16 weeks), repeated BP marginal (sysRepeated 145), normal initial BP, no proteinuria -> DiagnosisChronicHypertensionAfterRecheck present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> diagnoseNurse16Weeks
                    |> EverySet.member DiagnosisChronicHypertensionAfterRecheck
                    |> Expect.equal True
        ]



-- GROUP F -- SEVERE PRE-ECLAMPSIA RECURRENT (nurse, EGA < 37)
--
-- `severePreeclampsiaRecurrentPhase` =
--   (initial BP >=160/110 OR repeated BP >=160/110)
--     && highUrineProtein && severePreeclampsiaSigns.
-- The initial reading is normal (120/80) and the REPEATED reading is severe
-- (165/115 >= 160/110), so the recurrent (not initial) severe-BP branch fires;
-- combined with urine protein and a severe-preeclampsia core-physical-exam sign
-- (TPRightUpper in the abdomen) the diagnosis fires. EGA ~28 weeks (< 37) keeps
-- it off the EGA37+ emergency-referral variant.


generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent severe pre-eclampsia (EGA ~28 weeks, < 37)"
        [ test "repeated BP severe (165/115 >=160/110) + urine protein +1 + severe-preeclampsia sign, normal initial BP -> DiagnosisSeverePreeclampsiaRecurrentPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 165 115
                    |> withUrineProtein ProteinPlus1
                    |> withSeverePreeclampsiaSigns
                    |> diagnoseNurse28Weeks
                    |> EverySet.member DiagnosisSeverePreeclampsiaRecurrentPhase
                    |> Expect.equal True
        ]



-- GROUP G -- RECURRENT HIV DETECTABLE VIRAL LOAD (nurse)
--
-- `hivDetectableViralLoadDiagnosed` reads `.hivViralLoad` and fires when it is
-- >= 20 (a detectable viral load). Entered non-immediately
-- (`testPrerequisites = Nothing`), `immediateResult .hivPCRTest` is False, so
-- the `*InitialPhase` matcher fails and the diagnosis routes to the
-- `*RecurrentPhase` variant.


generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest : Test
generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent HIV detectable viral load (non-immediate PCR)"
        [ test "HIV PCR viral load 1000 (>= 20, detectable), non-immediate -> DiagnosisHIVDetectableViralLoadRecurrentPhase present AND DiagnosisHIVDetectableViralLoadInitialPhase NOT present" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withHIVPCRNonImmediate 1000
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisHIVDetectableViralLoadRecurrentPhase diagnoses
                , EverySet.member DiagnosisHIVDetectableViralLoadInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]


{-| A viral load answered "undetectable" is under 20 copies, so it is saved
without a number. A number typed before stays on the form, so a mistaken Yes can
be undone, and a record that still holds one is not diagnosed from it.
-}
hivViralLoadUndetectableTest : Test
hivViralLoadUndetectableTest =
    let
        savedDetectable =
            hivPCRValueNonImmediate 5000

        valueAfter msgs =
            msgs
                |> List.foldl
                    (\msg model ->
                        update dummyDate SiteRwanda (toEntityUuid "encounter") emptyModelIndexedDb msg model
                            |> (\( updated, _, _ ) -> updated)
                    )
                    emptyModel
                |> .laboratoryData
                |> .hivPCRTestForm
                |> toHIVPCRTestValueWithDefault (Just savedDetectable)

        statusAndLoad value =
            ( value.hivViralLoadStatus, value.hivViralLoad )

        detectableDiagnosed value =
            { emptyPrenatalMeasurements | hivPCRTest = wrapMeasurement value }
                |> diagnoseNurse
                |> EverySet.member DiagnosisHIVDetectableViralLoadRecurrentPhase
    in
    describe "HIV PCR viral load answered undetectable"
        [ test "first visit: Yes on a saved detectable result saves no number" <|
            \_ ->
                valueAfter [ SetHIVViralLoadUndetectable True ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadUndetectable, Nothing ))
        , test "first visit: Yes then No on a saved detectable result keeps its number" <|
            \_ ->
                valueAfter [ SetHIVViralLoadUndetectable True, SetHIVViralLoadUndetectable False ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadDetectable, Just 5000 ))
        , test "first visit: a typed number survives a mistaken Yes" <|
            \_ ->
                valueAfter [ SetHIVViralLoad "300", SetHIVViralLoadUndetectable True, SetHIVViralLoadUndetectable False ]
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadDetectable, Just 300 ))
        , test "lab results: a saved detectable result answered undetectable saves no number" <|
            \_ ->
                { emptyHIVPCRResultForm | hivViralLoadStatus = Just ViralLoadUndetectable }
                    |> toHIVPCRResultValueWithDefault (Just savedDetectable)
                    |> Maybe.map statusAndLoad
                    |> Expect.equal (Just ( Just ViralLoadUndetectable, Nothing ))
        , test "a record answered undetectable is not diagnosed from the number it holds" <|
            \_ ->
                ( detectableDiagnosed { savedDetectable | hivViralLoadStatus = Just ViralLoadUndetectable }
                , detectableDiagnosed savedDetectable
                )
                    |> Expect.equal ( False, True )
        ]


{-| The discordant-partnership diagnosis carries an Initial and a Recurrent
variant, and only the Recurrent one prescribes PrEP at the recurrent phase. The
variant has to follow the test that produced the match: the partner's own test
when the match came from there, and the patient's test when it came from the
partner signs recorded on it. A match from the partner's test also needs the
patient's own result, so that result must be known at the initial phase too -
run with an immediate result, or taken from the patient's history.
-}
generatePrenatalDiagnosesForNurseDiscordantPartnershipTest : Test
generatePrenatalDiagnosesForNurseDiscordantPartnershipTest =
    let
        discordantPartnershipPhases measurements =
            let
                diagnoses =
                    diagnoseNurse measurements
            in
            ( EverySet.member DiagnosisDiscordantPartnershipInitialPhase diagnoses
            , EverySet.member DiagnosisDiscordantPartnershipRecurrentPhase diagnoses
            )
    in
    describe "generatePrenatalDiagnosesForNurse - discordant partnership phase"
        [ test "partner test positive and immediate, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner test positive and NOT immediate, patient HIV negative and immediate -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive and immediate, patient HIV negative and NOT immediate -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday deferredResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive and immediate, patient HIV negative from history -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunPreviously Nothing
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive after a point of care test, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner known to be HIV positive after the test was sent to the lab, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteKnownAsPositive deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner test positive from history, patient HIV negative and immediate -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestPositive TestNoteRunPreviously Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's immediate HIV test, no partner test -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's HIV test taken from history -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunPreviously Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        , test "partner signs on the patient's HIV test sent to the lab -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday deferredResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive entered by a lab technician, the nurse has not answered the follow ups yet -> no diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner test positive entered by a lab technician, nurse answers partner not on ARVs -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "both tests sent to the lab, partner positive and patient negative entered there -> Recurrent phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunConfirmedByLabTech deferredResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, True )
        , test "partner test positive entered by a lab technician, nurse answers partner on ARVs with a surpressed viral load -> no diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "patient known as positive, partner signs of the replaced negative test still on it -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestKnownAsPositiveCarryingSigns
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "patient known as positive, partner test positive -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestKnownAsPositiveCarryingSigns
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner then tested negative -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner test then sent to the lab -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday deferredResultPrerequisites Nothing Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs not on ARVs on the patient's HIV test, partner tested positive, on ARVs with a surpressed viral load -> neither phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday
                        immediateResultPrerequisites
                        (Just TestPositive)
                        (Just <| EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> discordantPartnershipPhases
                    |> Expect.equal ( False, False )
        , test "partner signs on the patient's HIV test, partner test not run for lack of reagents -> Initial phase" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteLackOfReagents Nothing Nothing Nothing
                    |> discordantPartnershipPhases
                    |> Expect.equal ( True, False )
        ]


{-| The partner's HIV result decides whether the nurse is asked for the
partner's status by hand: that question is drawn on the patient's own HIV test
form only while the result resolves to `TestIndeterminate`. A result a lab
technician entered carries its own execution note, and has to resolve like any
other, so the question the lab already answered is not asked again.
-}
resolvePartnerHIVTestResultTest : Test
resolvePartnerHIVTestResultTest =
    describe "resolvePartnerHIVTestResult"
        [ test "positive result entered by a lab technician -> TestPositive" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestPositive
        , test "negative result entered by a lab technician -> TestNegative" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestByLabTech TestNegative (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestNegative
        , test "positive result the nurse ran today -> TestPositive" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestPositive
        , test "no partner test -> TestIndeterminate" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> resolvePartnerHIVTestResult
                    |> Expect.equal TestIndeterminate
        ]


{-| The line the progress report shows for a discordant couple states what the
partner's ARV and viral load status is. A lab technician can enter the partner's
result but not those answers, so until the nurse answers them there is no status
to state.
-}
resolveDiscordantCoupleStatusTest : Test
resolveDiscordantCoupleStatusTest =
    describe "resolveDiscordantCoupleStatus"
        [ test "partner positive by a lab technician, follow ups not answered yet -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton PrenatalHIVSignPendingInput)
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        , test "partner positive by a lab technician, nurse answers partner not on ARVs -> not taking ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.singleton NoPrenatalHIVSign)
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus False False)
        , test "partner positive by a lab technician, nurse answers partner on ARVs and surpressed -> taking ARVs, surpressed" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTestByLabTech TestPositive (EverySet.fromList [ PartnerTakingARV, PartnerSurpressedViralLoad ])
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus True True)
        , test "partner signs on the patient's own HIV test -> status from those signs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal (Just <| Translate.DiscordantCoupleStatus False False)
        , test "partner signs on the patient's own HIV test, partner then tested negative -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestPartnerPositiveSigns TestNoteRunToday immediateResultPrerequisites
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        , test "no partner test and no partner signs -> no status" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                    |> resolveDiscordantCoupleStatus
                    |> Expect.equal Nothing
        ]


{-| The partner signs on the patient's own HIV test are asked only while the
partner has no test result. Once the partner is tested, the partner's test is
what the medication helper text and the HIV health education follow.
-}
partnerSignsAfterPartnerTestedTest : Test
partnerSignsAfterPartnerTestedTest =
    let
        medicationHelpers measurements =
            let
                assembled =
                    testAssembled measurements
            in
            resolveRequiredMedicationsSet English
                currentDate
                PrenatalEncounterPhaseInitial
                { assembled
                    | encounter =
                        { testEncounter | diagnoses = EverySet.singleton DiagnosisDiscordantPartnershipInitialPhase }
                }
                |> List.map (\( helper, _, _ ) -> helper)

        hivEducationTasks measurements =
            healthEducationFormInputsAndTasksForNurse English
                PrenatalEncounterPhaseInitial
                (\_ _ -> ())
                (testAssembled measurements)
                emptyHealthEducationForm
                |> Tuple.second
                |> List.length

        partnerOnARVsSurpressed =
            EverySet.fromList [ PartnerHIVPositive, PartnerTakingARV, PartnerSurpressedViralLoad ]
    in
    describe "partner signs on the patient's HIV test once the partner is tested"
        [ test "partner signs on ARVs, partner tested positive and not on ARVs -> helper for a partner not on ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> withPartnerHIVTestPositive TestNoteRunToday immediateResultPrerequisites
                    |> medicationHelpers
                    |> Expect.equal [ Translate.MedicationDistributionHelperDiscordantPartnershipNoARVs ]
        , test "partner signs on ARVs, no partner test -> helper for a partner on ARVs" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> medicationHelpers
                    |> Expect.equal [ Translate.MedicationDistributionHelperDiscordantPartnership ]
        , test "partner signs on ARVs and surpressed, partner tested negative -> partner testing education asked" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> withPartnerHIVTest TestNoteRunToday immediateResultPrerequisites (Just TestNegative) Nothing
                    |> hivEducationTasks
                    |> Expect.equal
                        (emptyPrenatalMeasurements
                            |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                            |> hivEducationTasks
                        )
        , test "partner signs on ARVs and surpressed, no partner test -> partner testing education not asked" <|
            \_ ->
                (emptyPrenatalMeasurements
                    |> withHIVTestNegativeWithSigns TestNoteRunToday immediateResultPrerequisites partnerOnARVsSurpressed
                    |> hivEducationTasks
                )
                    |> Expect.equal
                        ((emptyPrenatalMeasurements
                            |> withHIVTestNegative TestNoteRunToday immediateResultPrerequisites
                            |> hivEducationTasks
                         )
                            - 1
                        )
        ]



-- GROUP H -- EGA37+ RECURRENT PRE-ECLAMPSIA (nurse, EGA >= 37)
--
-- The same recurrent moderate / severe pre-eclampsia recipes as GROUP B / F,
-- but at EGA ~38 weeks (>= 37). At that EGA the recurrent pre-eclampsia
-- diagnoses are produced by `matchEmergencyReferalPrenatalDiagnosis` as the
-- `*EGA37Plus` emergency-referral variants instead.
--
--   * Moderate: repeated marginal BP (sysRepeated 145) + urine protein, normal
--     initial BP -> DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus.
--   * Severe: repeated severe BP (165/115) + urine protein + severe sign,
--     normal initial BP -> DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus.
--
-- The EGA37+ variants are the surviving (top) blood-pressure diagnosis after
-- `applyHypertensionlikeDiagnosesHierarchy` (ranks 31 / 41).


generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest : Test
generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest =
    describe "generatePrenatalDiagnosesForNurse - recurrent pre-eclampsia EGA37+ (EGA ~38 weeks, >= 37)"
        [ test "repeated BP marginal (sysRepeated 145) + urine protein +1, normal initial BP, EGA >= 37 -> DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 80
                    |> withUrineProtein ProteinPlus1
                    |> diagnoseNurse38Weeks
                    |> EverySet.member DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
                    |> Expect.equal True
        , test "repeated BP severe (165/115 >=160/110) + urine protein +1 + severe-preeclampsia sign, normal initial BP, EGA >= 37 -> DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 165 115
                    |> withUrineProtein ProteinPlus1
                    |> withSeverePreeclampsiaSigns
                    |> diagnoseNurse38Weeks
                    |> EverySet.member DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
                    |> Expect.equal True
        ]


suicideRiskDiagnosedBySignsTest : Test
suicideRiskDiagnosedBySignsTest =
    -- Oracle: the suicide-risk screening item (question 10). Any positive
    -- response (Option1/2/3) flags risk; Option0 (none) does not; an unanswered
    -- question 10 yields Nothing.
    let
        withQuestion10 option =
            Dict.fromList [ ( MentalHealthQuestion10, option ) ]
    in
    describe "suicideRiskDiagnosedBySigns"
        [ test "Q10 = Option0 (none) -> Just False" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption0) |> Expect.equal (Just False)
        , test "Q10 = Option1 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption1) |> Expect.equal (Just True)
        , test "Q10 = Option2 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption2) |> Expect.equal (Just True)
        , test "Q10 = Option3 -> Just True" <|
            \_ -> suicideRiskDiagnosedBySigns (withQuestion10 MentalHealthQuestionOption3) |> Expect.equal (Just True)
        , test "no question 10 answered -> Nothing" <|
            \_ -> suicideRiskDiagnosedBySigns Dict.empty |> Expect.equal Nothing
        , test "only other questions answered (Q10 absent) -> Nothing" <|
            \_ -> suicideRiskDiagnosedBySigns (Dict.fromList [ ( MentalHealthQuestion1, MentalHealthQuestionOption3 ) ]) |> Expect.equal Nothing
        ]


{-| Abnormal vaginal discharge splits into three diagnoses by the two follow up
questions. Answering no to both is trichomonas or bacterial vaginosis; each
diagnosis then has a "continued" variant, chosen by whether the patient was
given that same diagnosis at an earlier nurse encounter. The plain variant
prescribes medication, the continued one refers to hospital.
-}
vaginalDischargeContinuedTest : Test
vaginalDischargeContinuedTest =
    let
        withVaginalDischarge questions measurements =
            { measurements
                | symptomReview =
                    wrapMeasurement
                        { symptoms = EverySet.singleton AbnormalVaginalDischarge
                        , symptomQuestions = questions
                        , flankPainSign = Nothing
                        }
            }

        -- No vaginal itching and no partner urethral discharge.
        bacterialVaginosis =
            withVaginalDischarge EverySet.empty

        gonorrhea =
            withVaginalDischarge (EverySet.singleton SymptomQuestionPartnerUrethralDischarge)

        diagnoseNurseAfterDiagnoses previousDiagnoses =
            diagnoseNurseAfter previousDiagnoses emptyPrenatalMeasurements
    in
    describe "generatePrenatalDiagnosesForNurse - abnormal vaginal discharge, continued variants"
        [ test "first episode -> the plain diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal True
        , test "diagnosed at an earlier encounter -> the continued diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosisContinued
                    |> Expect.equal True
        , test "diagnosed at an earlier encounter -> the plain diagnosis is not given as well" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal False
        , test "an earlier gonorrhea does not make a first episode continued" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosisContinued
                    |> Expect.equal False
        , test "an earlier gonorrhea leaves a first episode at the plain diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> bacterialVaginosis
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisTrichomonasOrBacterialVaginosis
                    |> Expect.equal True
        , test "an earlier bacterial vaginosis does not make a first gonorrhea continued" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> gonorrhea
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisTrichomonasOrBacterialVaginosis ]
                    |> EverySet.member DiagnosisGonorrheaContinued
                    |> Expect.equal False
        , test "gonorrhea diagnosed at an earlier encounter -> the continued diagnosis" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> gonorrhea
                    |> diagnoseNurseAfterDiagnoses [ DiagnosisGonorrhea ]
                    |> EverySet.member DiagnosisGonorrheaContinued
                    |> Expect.equal True
        ]



-- HISTORY OF AN OLDER ENCOUNTER
--
-- A lab result can be entered for an encounter after a later encounter of the
-- pregnancy has started, and the older encounter is then diagnosed again. Its
-- history is the encounters started on an earlier day. There is one encounter a
-- day, so another encounter of the same day is not part of it either.


{-| A prenatal encounter as stored in the database: its id, start date, type,
diagnoses, the diagnoses reported to it from late lab results, and the
measurements taken at it.
-}
type alias StoredEncounter =
    { id : String
    , startDate : NominalDate
    , encounterType : PrenatalEncounterType
    , diagnoses : List PrenatalDiagnosis
    , pastDiagnoses : List PrenatalDiagnosis
    , measurements : PrenatalMeasurements
    }


storedNurseEncounter : String -> NominalDate -> List PrenatalDiagnosis -> PrenatalMeasurements -> StoredEncounter
storedNurseEncounter id startDate diagnoses measurements =
    { id = id
    , startDate = startDate
    , encounterType = NurseEncounter
    , diagnoses = diagnoses
    , pastDiagnoses = []
    , measurements = measurements
    }


{-| Assemble the encounter with the given id from a database holding the given
encounters of one pregnancy -- the way the pages and the assessment run after
a save build their data. EGA is fixed at ~20 weeks, as for `testAssembled`.
-}
assembledFromDb : String -> List StoredEncounter -> Maybe AssembledData
assembledFromDb encounterId storedEncounters =
    let
        encounters =
            List.map
                (\stored ->
                    ( toEntityUuid stored.id
                    , { testEncounter
                        | startDate = stored.startDate
                        , encounterType = stored.encounterType
                        , diagnoses = EverySet.fromList stored.diagnoses
                        , pastDiagnoses = EverySet.fromList stored.pastDiagnoses
                      }
                    )
                )
                storedEncounters

        db =
            { emptyModelIndexedDb
                | prenatalEncounters = Dict.fromList <| List.map (Tuple.mapSecond RemoteData.Success) encounters
                , prenatalEncountersByParticipant =
                    Dict.singleton testEncounter.participant (RemoteData.Success <| Dict.fromList encounters)
                , prenatalMeasurements =
                    Dict.fromList <|
                        List.map (\stored -> ( toEntityUuid stored.id, RemoteData.Success stored.measurements )) storedEncounters
                , individualParticipants = Dict.singleton testEncounter.participant (RemoteData.Success testParticipant)
                , people = Dict.singleton testParticipant.person (RemoteData.Success testPerson)
            }
    in
    generateAssembledData (toEntityUuid encounterId) db
        |> RemoteData.toMaybe
        |> Maybe.map (\assembled -> { assembled | globalLmpDate = Just lmpDate })


historyBeforeEncounterTest : Test
historyBeforeEncounterTest =
    let
        withSymptom symptom measurements =
            { measurements
                | symptomReview =
                    wrapMeasurement
                        { symptoms = EverySet.singleton symptom
                        , symptomQuestions = EverySet.empty
                        , flankPainSign = Nothing
                        }
            }

        weekAgo =
            Date.add Date.Weeks -1 currentDate

        weekLater =
            Date.add Date.Weeks 1 currentDate

        diagnosesOf encounterId storedEncounters =
            assembledFromDb encounterId storedEncounters
                |> Maybe.map (generatePrenatalDiagnosesForNurse >> EverySet.toList)

        hasDiagnosis diagnosis encounterId storedEncounters =
            diagnosesOf encounterId storedEncounters
                |> Maybe.map (List.member diagnosis)

        diagnosesAmong candidates encounterId storedEncounters =
            diagnosesOf encounterId storedEncounters
                |> Maybe.map (List.filter (\diagnosis -> List.member diagnosis candidates))

        pelvicPain =
            withSymptom PelvicPain emptyPrenatalMeasurements

        nauseaAndVomiting =
            withSymptom NauseaAndVomiting emptyPrenatalMeasurements
    in
    describe "generatePrenatalDiagnosesForNurse - history of an older encounter"
        [ test "pelvic pain at a LATER encounter does not make the older encounter's pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "older"
                    [ storedNurseEncounter "older" currentDate [] pelvicPain
                    , storedNurseEncounter "later" weekLater [] pelvicPain
                    ]
                    |> Expect.equal (Just False)
        , test "nausea and vomiting at a LATER encounter does not send the older encounter to hospital" <|
            \_ ->
                diagnosesAmong [ DiagnosisHyperemesisGravidumBySymptoms, DiagnosisSevereVomitingBySymptoms ]
                    "older"
                    [ storedNurseEncounter "older" currentDate [] nauseaAndVomiting
                    , storedNurseEncounter "later" weekLater [] nauseaAndVomiting
                    ]
                    |> Expect.equal (Just [])
        , test "malaria treated at a LATER encounter does not make the older encounter's malaria medicated-continued" <|
            \_ ->
                diagnosesAmong
                    [ DiagnosisMalariaInitialPhase
                    , DiagnosisMalariaMedicatedContinuedInitialPhase
                    , DiagnosisMalariaMedicatedContinuedRecurrentPhase
                    ]
                    "older"
                    [ storedNurseEncounter "older" currentDate [] (withMalariaTest TestPositive emptyPrenatalMeasurements)
                    , storedNurseEncounter "later" weekLater [ DiagnosisMalariaInitialPhase ] (withCoartem emptyPrenatalMeasurements)
                    ]
                    |> Expect.equal (Just [ DiagnosisMalariaInitialPhase ])
        , test "pelvic pain at an EARLIER encounter makes the pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "current"
                    [ storedNurseEncounter "earlier" weekAgo [] pelvicPain
                    , storedNurseEncounter "current" currentDate [] pelvicPain
                    ]
                    |> Expect.equal (Just True)
        , test "pelvic pain at another encounter of the SAME day does not make the pelvic pain continued" <|
            \_ ->
                hasDiagnosis DiagnosisPelvicPainContinued
                    "current"
                    [ storedNurseEncounter "same-day" currentDate [] pelvicPain
                    , storedNurseEncounter "current" currentDate [] pelvicPain
                    ]
                    |> Expect.equal (Just False)
        , test "only CHW encounters started on an earlier day are in the history" <|
            \_ ->
                let
                    storedChwEncounter id startDate encounterType =
                        { id = id
                        , startDate = startDate
                        , encounterType = encounterType
                        , diagnoses = []
                        , pastDiagnoses = []
                        , measurements = emptyPrenatalMeasurements
                        }
                in
                assembledFromDb "current"
                    [ storedChwEncounter "chw-earlier" weekAgo ChwFirstEncounter
                    , storedChwEncounter "chw-same-day" currentDate ChwSecondEncounter
                    , storedNurseEncounter "current" currentDate [] emptyPrenatalMeasurements
                    , storedChwEncounter "chw-later" weekLater ChwThirdPlusEncounter
                    ]
                    |> Maybe.map (.chwPreviousMeasurementsWithDates >> List.map (\( date, encounterType, _ ) -> ( date, encounterType )))
                    |> Expect.equal (Just [ ( weekAgo, ChwFirstEncounter ) ])
        ]



-- BLOOD SMEAR AS THE MALARIA DIAGNOSIS
--
-- When the rapid test cannot be run, a blood smear is taken instead and read
-- at the lab; issue #631 introduced it as "a different way to diagnose
-- malaria". Whoever reads the smear, the smear result is the diagnosis.
--
-- Two people can record that result, and they leave DIFFERENT execution notes.
-- A nurse entering it keeps the nurse's reason for not running the RDT; a lab
-- technician confirming the run replaces the note with
-- `TestNoteRunConfirmedByLabTech` (`toMalariaResultValue`). Neither of them
-- writes a `testResult`, because the form shows the smear input instead.


{-| A malaria test the nurse did not run as an RDT, with a blood smear read in
its place — under the execution note the given reader leaves behind.
-}
malariaTestValueWithBloodSmear : TestExecutionNote -> BloodSmearResult -> MalariaTestValue
malariaTestValueWithBloodSmear executionNote bloodSmearResult =
    { executionNote = executionNote
    , executionDate = Just dummyDate
    , testPrerequisites = immediateResultPrerequisites
    , testResult = Nothing
    , bloodSmearResult = bloodSmearResult
    , bloodSmearOrdered = True
    }


{-| A blood smear that also carries a rapid test result, the way a client that
does not know about the smear order writes one.
-}
withBloodSmearAndRapidTestResult : TestExecutionNote -> BloodSmearResult -> TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withBloodSmearAndRapidTestResult executionNote bloodSmearResult testResult measurements =
    let
        value =
            malariaTestValueWithBloodSmear executionNote bloodSmearResult
    in
    { measurements | malariaTest = wrapMeasurement { value | testResult = Just testResult } }


{-| The same record as written before `bloodSmearOrdered` existed: the smear
result is all that says a smear was taken.
-}
withLegacyBloodSmearAndRapidTestResult : TestExecutionNote -> BloodSmearResult -> TestResult -> PrenatalMeasurements -> PrenatalMeasurements
withLegacyBloodSmearAndRapidTestResult executionNote bloodSmearResult testResult measurements =
    let
        value =
            malariaTestValueWithBloodSmear executionNote bloodSmearResult
    in
    { measurements
        | malariaTest =
            wrapMeasurement { value | testResult = Just testResult, bloodSmearOrdered = False }
    }


withBloodSmear : TestExecutionNote -> BloodSmearResult -> PrenatalMeasurements -> PrenatalMeasurements
withBloodSmear executionNote bloodSmearResult measurements =
    { measurements
        | malariaTest = wrapMeasurement (malariaTestValueWithBloodSmear executionNote bloodSmearResult)
    }


generatePrenatalDiagnosesForNurseBloodSmearTest : Test
generatePrenatalDiagnosesForNurseBloodSmearTest =
    describe "generatePrenatalDiagnosesForNurse - malaria read from a blood smear"
        [ test "smear + under the nurse's reason -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteLackOfReagents BloodSmearPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear + under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear +++ under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase present" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlusPlusPlus
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal True
        , test "smear negative under the lab technician's confirmed-run note -> DiagnosisMalariaInitialPhase absent" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearNegative
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , test "a smear still awaited by the lab diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmear TestNoteLackOfReagents BloodSmearPendingInput
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , -- A record that ordered a smear did not run the rapid test, so a
          -- rapid test result on it is one nobody entered for it. An older
          -- client that still writes one must not diagnose through it.
          test "a positive rapid test result on a record that ordered a smear diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withBloodSmearAndRapidTestResult TestNoteRunConfirmedByLabTech BloodSmearNegative TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , -- Same record, written before the order was recorded: the smear
          -- result is the only thing that says a smear was taken.
          test "a positive rapid test result on a legacy smear record diagnoses nothing" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withLegacyBloodSmearAndRapidTestResult TestNoteRunConfirmedByLabTech BloodSmearNegative TestPositive
                    |> diagnoseNurse
                    |> EverySet.member DiagnosisMalariaInitialPhase
                    |> Expect.equal False
        , test "smear + read by the lab technician, with Hb 9, is malaria WITH anemia - not anemia alone" <|
            \_ ->
                let
                    diagnoses =
                        emptyPrenatalMeasurements
                            |> withBloodSmear TestNoteRunConfirmedByLabTech BloodSmearPlus
                            |> withHemoglobin 9
                            |> diagnoseNurse
                in
                ( EverySet.member DiagnosisMalariaWithAnemiaInitialPhase diagnoses
                , EverySet.member DiagnosisModerateAnemiaInitialPhase diagnoses
                )
                    |> Expect.equal ( True, False )
        ]


all : Test
all =
    describe "Prenatal Activity tests"
        [ measurementOutOfRangeTest
        , bmiToPrePregnancyClassificationTest
        , zscoreToPrePregnancyClassificationTest
        , resolveGWGClassificationForHealthyStartTest
        , resolveGWGClassificationSincePreviousVisitTest
        , resolveGWGClassificationTest
        , generatePrenatalDiagnosesForNurseLabsTest
        , generatePrenatalDiagnosesForNurseRecurrentLabsTest
        , generatePrenatalAssesmentForChwTest
        , generatePrenatalDiagnosesForNurseAnemiaTest
        , generatePrenatalDiagnosesForNurseMalariaWithAnemiaTest
        , generatePrenatalDiagnosesForNurseBloodSmearTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaTest
        , generatePrenatalDiagnosesForNurseModeratePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseAnemiaRecurrentTest
        , generatePrenatalDiagnosesForNurseDiabetesRhesusRecurrentTest
        , generatePrenatalDiagnosesForNurseHypertensionRecheckTest
        , generatePrenatalDiagnosesForNurseSeverePreeclampsiaRecurrentTest
        , generatePrenatalDiagnosesForNurseHIVViralLoadRecurrentTest
        , hivViralLoadUndetectableTest
        , generatePrenatalDiagnosesForNurseDiscordantPartnershipTest
        , resolvePartnerHIVTestResultTest
        , resolveDiscordantCoupleStatusTest
        , partnerSignsAfterPartnerTestedTest
        , generatePrenatalDiagnosesForNurseEGA37PlusPreeclampsiaRecurrentTest
        , suicideRiskDiagnosedBySignsTest
        , vaginalDischargeContinuedTest
        , historyBeforeEncounterTest
        , repeatedTapOnSavedTest
        , diagnosisKnownAnyOfTest
        , obstetricHistoryFirstPregnancyTest
        , nextVisitTaskTest
        , lateLabResultSameDayTest
        , lateLabResultHypertensionTest
        , lateLabResultDiabetesTest
        , lateLabResultModeratePreeclampsiaTest
        , lateLabResultHyperemesisTest
        , lateLabResultAddsNothingTest
        , lateLabResultPreeclampsiaRiskTest
        , lateLabResultLabTechAndHistoryTest
        , lateUrineProteinSeverePreeclampsiaTest
        , preeclampsiaRiskAfterTheVisitTest
        , ultrasoundFormTest
        , preeclampsiaRiskSevereAt37PlusTest
        , preeclampsiaRiskModerateAt37PlusTest
        , preeclampsiaRiskEclampsiaTest
        , preeclampsiaRiskLabResultsAt37PlusTest
        , preeclampsiaRiskBefore37Test
        , preeclampsiaRiskHypertensionAt37PlusTest
        , preeclampsiaRiskReferralUnchangedTest
        , tetanusDoseFallingDueTodayTest
        ]


{-| Next ANC Visit shows a date computed from the pregnancy's EGA, and only
saves that date. A pregnancy with no LMP has none, so the task must not be
asked: it could never be completed, and the visit could never be ended.

CHW visits 2 and 3 have no Pregnancy Dating activity, so an undated
pregnancy stays undated there.

-}
nextVisitTaskTest : Test
nextVisitTaskTest =
    let
        withEncounterType encounterType assembled =
            let
                encounter =
                    assembled.encounter
            in
            { assembled | encounter = { encounter | encounterType = encounterType } }

        undated assembled =
            { assembled | globalLmpDate = Nothing }

        asksNextVisit assembled =
            resolveNextStepsTasks currentDate assembled
                |> List.member NextStepsNextVisit

        -- The CHW's other Next Steps tasks on a visit without danger signs.
        withAppointmentAndFollowUp measurements =
            { measurements
                | appointmentConfirmation = wrapMeasurement { date = Date.add Date.Weeks 4 currentDate }
                , followUp =
                    wrapMeasurement
                        { options = EverySet.singleton OneDay
                        , resolutionDate = Nothing
                        , assesment = AssesmentNormalPregnancy
                        }
            }

        nextStepsCompleted assembled =
            activityCompleted currentDate SiteRwanda EverySet.empty assembled Backend.PrenatalActivity.Model.NextSteps
    in
    describe "Next Steps › Next ANC Visit"
        [ test "CHW visit 2 on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwSecondEncounter
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "CHW visit 3 on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwThirdPlusEncounter
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "a nurse visit on an undated pregnancy does not ask for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> undated
                    |> asksNextVisit
                    |> Expect.equal False
        , test "CHW visit 2 on an undated pregnancy completes Next Steps once its other tasks are saved" <|
            \_ ->
                testAssembled (withAppointmentAndFollowUp emptyPrenatalMeasurements)
                    |> withEncounterType ChwSecondEncounter
                    |> undated
                    |> nextStepsCompleted
                    |> Expect.equal True
        , test "CHW visit 2 on a dated pregnancy still needs Next ANC Visit to complete Next Steps" <|
            \_ ->
                testAssembled (withAppointmentAndFollowUp emptyPrenatalMeasurements)
                    |> withEncounterType ChwSecondEncounter
                    |> nextStepsCompleted
                    |> Expect.equal False
        , test "CHW visit 2 on a dated pregnancy asks for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> withEncounterType ChwSecondEncounter
                    |> asksNextVisit
                    |> Expect.equal True
        , test "a nurse visit on a dated pregnancy asks for it" <|
            \_ ->
                testAssembled emptyPrenatalMeasurements
                    |> asksNextVisit
                    |> Expect.equal True
        ]


{-| Nothing was checked on this encounter, so a mistyped height, weight, MUAC or
fundal height saved without a word. Each is named on a warning now, and nothing
is saved until it is entered again.

A height measured at an earlier encounter is carried over and its input is not
drawn, so it is not asked about: there would be nothing on screen to correct.

-}
measurementOutOfRangeTest : Test
measurementOutOfRangeTest =
    let
        person =
            toEntityUuid "person"

        preSaveNutrition site carriedOverHeight height weight muac =
            let
                data =
                    emptyModel.examinationData

                form =
                    data.nutritionAssessmentForm

                model =
                    { emptyModel
                        | examinationData =
                            { data
                                | nutritionAssessmentForm =
                                    { form | height = height, weight = weight, muac = muac }
                            }
                    }

                ( updatedModel, _, appMsgs ) =
                    update (Date.fromCalendarDate 2026 Time.Jul 28)
                        site
                        (toEntityUuid "encounter")
                        emptyModelIndexedDb
                        (PreSaveNutritionAssessment person Nothing carriedOverHeight Nothing Nothing)
                        model
            in
            -- What the warning names, and whether anything was saved.
            ( updatedModel.warningPopupState, not <| List.isEmpty appMsgs )

        preSaveObstetrical palpable fundalHeight =
            let
                data =
                    emptyModel.examinationData

                form =
                    data.obstetricalExamForm

                model =
                    { emptyModel
                        | examinationData =
                            { data
                                | obstetricalExamForm =
                                    { form | fundalPalpable = palpable, fundalHeight = fundalHeight }
                            }
                    }

                ( updatedModel, _, appMsgs ) =
                    update (Date.fromCalendarDate 2026 Time.Jul 28)
                        SiteRwanda
                        (toEntityUuid "encounter")
                        emptyModelIndexedDb
                        (PreSaveObstetricalExam person Nothing Nothing)
                        model
            in
            ( updatedModel.warningPopupState, not <| List.isEmpty appMsgs )

        named measurements =
            Just (WarningPopupMeasurementOutOfRange measurements)
    in
    describe "the ANC save gate"
        [ test "a height of 1050 cm is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 1050) Nothing Nothing
                    |> Expect.equal ( named [ MeasurementHeight ], False )
        , test "a weight of 850 kg is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing Nothing (Just 850) Nothing
                    |> Expect.equal ( named [ MeasurementWeight ], False )
        , test "a MUAC of 125, being millimetres, is named and saves nothing" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing Nothing Nothing (Just 125)
                    |> Expect.equal ( named [ MeasurementMuac ], False )
        , test "Burundi holds MUAC in centimetres too, so 12.5 is within range there" <|
            \_ ->
                preSaveNutrition SiteBurundi Nothing Nothing Nothing (Just 12.5)
                    |> Expect.equal ( Nothing, True )
        , test "every one that is wrong is named, not just the first" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 1050) (Just 850) (Just 125)
                    |> Expect.equal
                        ( named [ MeasurementHeight, MeasurementWeight, MeasurementMuac ], False )
        , test "a height carried over from an earlier encounter is not asked about" <|
            \_ ->
                preSaveNutrition SiteRwanda (Just 165) (Just 1050) Nothing Nothing
                    |> Expect.equal ( Nothing, True )
        , test "measurements within range save with no warning" <|
            \_ ->
                preSaveNutrition SiteRwanda Nothing (Just 165) (Just 65) (Just 25)
                    |> Expect.equal ( Nothing, True )
        , test "a fundal height of 120 cm is named and saves nothing" <|
            \_ ->
                preSaveObstetrical (Just True) (Just 120)
                    |> Expect.equal ( named [ MeasurementFundalHeight ], False )
        , test "a fundal height within range saves with no warning" <|
            \_ ->
                preSaveObstetrical (Just True) (Just 30)
                    |> Expect.equal ( Nothing, True )
        , test "nothing is asked when the uterus cannot be felt" <|
            \_ ->
                preSaveObstetrical (Just False) (Just 120)
                    |> Expect.equal ( Nothing, True )
        ]


{-| "Known as positive" fires on every tap, including one that repeats the
answer shown. On a test loaded from the record the form in the model is still
empty, so the update acts on the form as displayed; otherwise a repeated No
would clear a saved test.
-}
repeatedTapOnSavedTest : Test
repeatedTapOnSavedTest =
    let
        encounterId =
            toEntityUuid "encounter"

        savedHIVTest =
            hivTestValueWith TestNegative

        savedPartnerHIVTest =
            partnerHIVTestValuePositive TestNoteRunToday immediateResultPrerequisites

        savedHepatitisBTest =
            hepatitisBTestValueWith TestNegative

        db =
            { emptyModelIndexedDb
                | prenatalMeasurements =
                    Dict.singleton encounterId
                        (RemoteData.Success
                            { emptyPrenatalMeasurements
                                | hivTest = wrapMeasurement savedHIVTest
                                , partnerHIVTest = wrapMeasurement savedPartnerHIVTest
                                , hepatitisBTest = wrapMeasurement savedHepatitisBTest
                            }
                        )
            }

        dataAfter msg =
            update dummyDate SiteRwanda encounterId db msg emptyModel
                |> (\( model, _, _ ) -> model.laboratoryData)

        noteAndResult value =
            ( value.executionNote, value.testResult )
    in
    describe "a repeated tap on known as positive, on a test loaded from the record"
        [ test "HIV test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetHIVTestFormBoolInput knownAsPositiveUpdateHIVTest False)).hivTestForm
                    |> toHIVTestValueUniversalWithDefault (Just savedHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestNegative ))
        , test "HIV test: Yes still replaces the run" <|
            \_ ->
                (dataAfter (SetHIVTestFormBoolInput knownAsPositiveUpdateHIVTest True)).hivTestForm
                    |> toHIVTestValueUniversalWithDefault (Just savedHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteKnownAsPositive, Nothing ))
        , test "partner HIV test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetPartnerHIVTestFormBoolInput knownAsPositiveUpdatePartnerHIVTest False)).partnerHIVTestForm
                    |> toPartnerHIVTestValueWithDefault (Just savedPartnerHIVTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestPositive ))
        , test "hepatitis B test: No keeps the saved run and result" <|
            \_ ->
                (dataAfter (SetHepatitisBTestFormBoolInput knownAsPositiveUpdate False)).hepatitisBTestForm
                    |> toHepatitisBTestValueWithDefault (Just savedHepatitisBTest)
                    |> Maybe.map noteAndResult
                    |> Expect.equal (Just ( TestNoteRunToday, Just TestNegative ))
        ]



-- WHAT THE ENCOUNTER A LATE LAB RESULT REPORTS TO ALREADY KNOWS
--
-- A lab result entered for an older encounter from a later one reports the
-- diagnosis it adds back to the later encounter, unless that encounter already
-- knows it. The older encounter's history cannot hold a diagnosis made at a
-- visit after it, so the question is asked of the later encounter.


diagnosisKnownAnyOfTest : Test
diagnosisKnownAnyOfTest =
    let
        weekAgo =
            Date.add Date.Weeks -1 currentDate

        weekLater =
            Date.add Date.Weeks 1 currentDate

        rhesusNegative =
            [ DiagnosisRhesusNegativeInitialPhase, DiagnosisRhesusNegativeRecurrentPhase ]

        older =
            storedNurseEncounter "older" weekAgo [] emptyPrenatalMeasurements

        rhesusAtMiddle =
            storedNurseEncounter "middle" currentDate [ DiagnosisRhesusNegativeRecurrentPhase ] emptyPrenatalMeasurements

        current =
            storedNurseEncounter "current" weekLater [] emptyPrenatalMeasurements

        known diagnoses encounterId storedEncounters =
            assembledFromDb encounterId storedEncounters
                |> Maybe.map (diagnosisKnownAnyOf diagnoses)
    in
    describe "diagnosisKnownAnyOf - what the encounter a late lab result reports to already knows"
        [ test "a diagnosis made at a visit between the older encounter and the current one is known to the current one" <|
            \_ ->
                known rhesusNegative "current" [ older, rhesusAtMiddle, current ]
                    |> Expect.equal (Just True)
        , test "the older encounter's own history cannot see that visit, which is why the current one is asked" <|
            \_ ->
                assembledFromDb "older" [ older, rhesusAtMiddle, current ]
                    |> Maybe.map (diagnosedPreviouslyAnyOf rhesusNegative)
                    |> Expect.equal (Just False)
        , test "a diagnosis reported to a visit between the older encounter and the current one is known to the current one" <|
            \_ ->
                known rhesusNegative "current" [ older, { rhesusAtMiddle | diagnoses = [], pastDiagnoses = [ DiagnosisRhesusNegativeRecurrentPhase ] }, current ]
                    |> Expect.equal (Just True)
        , test "a diagnosis made at the current encounter itself is known" <|
            \_ ->
                known rhesusNegative "current" [ older, { current | diagnoses = [ DiagnosisRhesusNegativeRecurrentPhase ] } ]
                    |> Expect.equal (Just True)
        , test "a diagnosis reported to the current encounter by an earlier late result is known" <|
            \_ ->
                known diabetesDiagnoses "current" [ older, { current | pastDiagnoses = [ DiagnosisDiabetesRecurrentPhase ] } ]
                    |> Expect.equal (Just True)
        , test "a diagnosis made nowhere in the pregnancy is not known" <|
            \_ ->
                known rhesusNegative "current" [ older, current ]
                    |> Expect.equal (Just False)
        ]


{-| Step 1 corrected to a first pregnancy leaves the step 2 answers on the
encounter. They describe deliveries that never happened, so no reader counts them.
-}
obstetricHistoryFirstPregnancyTest : Test
obstetricHistoryFirstPregnancyTest =
    let
        firstPregnancy =
            { currentlyPregnant = True
            , termPregnancy = 0
            , preTermPregnancy = 0
            , stillbirthsAtTerm = 0
            , stillbirthsPreTerm = 0
            , abortions = 0
            , liveChildren = 0
            }

        secondPregnancy =
            { firstPregnancy | termPregnancy = 1, liveChildren = 1 }

        step2With signs period =
            { cSections = 0
            , cSectionReason = Nothing
            , previousDelivery = EverySet.singleton NoPreviousDeliverySign
            , previousDeliveryPeriod = EverySet.singleton period
            , signs = EverySet.fromList signs
            }

        withHistory step1 step2 =
            { emptyPrenatalMeasurements
                | obstetricHistory = wrapMeasurement step1
                , obstetricHistoryStep2 = wrapMeasurement step2
            }

        previousPreeclampsia =
            step2With [ ObstetricHistoryPreeclampsiaPreviousPregnancy ] Neither

        moreThan10Years =
            step2With [ NoObstetricHistoryStep2Sign ] MoreThan10Years

        convulsions =
            step2With [ ObstetricHistoryConvulsionsPreviousDelivery ] Neither

        highRisk =
            EverySet.member DiagnosisHighRiskOfPreeclampsiaInitialPhase

        moderateRisk =
            EverySet.member DiagnosisModerateRiskOfPreeclampsia

        convulsionsAlert measurements =
            generateHighRiskAlertData English measurements HighRiskConvulsionsPreviousDelivery
    in
    describe "obstetric history step 2 on a first pregnancy"
        [ test "previous preeclampsia gives high risk when step 1 records an earlier pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory secondPregnancy previousPreeclampsia)
                    |> highRisk
                    |> Expect.equal True
        , test "previous preeclampsia gives no high risk when step 1 says first pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory firstPregnancy previousPreeclampsia)
                    |> highRisk
                    |> Expect.equal False
        , test "the latest step 1 decides: a first pregnancy at the current visit drops step 2 saved at an earlier one" <|
            \_ ->
                diagnoseNurseAfter []
                    (withHistory secondPregnancy previousPreeclampsia)
                    { emptyPrenatalMeasurements | obstetricHistory = wrapMeasurement firstPregnancy }
                    |> highRisk
                    |> Expect.equal False
        , test "a first pregnancy recorded at an earlier visit is read the same way at a later one" <|
            \_ ->
                diagnoseNurseAfter [] (withHistory firstPregnancy previousPreeclampsia) emptyPrenatalMeasurements
                    |> highRisk
                    |> Expect.equal False
        , test "more than 10 years since delivery gives moderate risk when step 1 records an earlier pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory secondPregnancy moreThan10Years)
                    |> moderateRisk
                    |> Expect.equal True
        , test "more than 10 years since delivery gives no moderate risk when step 1 says first pregnancy" <|
            \_ ->
                diagnoseNurse (withHistory firstPregnancy moreThan10Years)
                    |> moderateRisk
                    |> Expect.equal False
        , test "convulsions at a previous delivery raise the high-risk alert when step 1 records an earlier pregnancy" <|
            \_ ->
                convulsionsAlert (withHistory secondPregnancy convulsions)
                    |> Expect.notEqual Nothing
        , test "convulsions at a previous delivery raise no high-risk alert when step 1 says first pregnancy" <|
            \_ ->
                convulsionsAlert (withHistory firstPregnancy convulsions)
                    |> Expect.equal Nothing
        , test "the skip rule is gravida 01 and para 0000 for every combination of counts" <|
            \_ ->
                let
                    counts =
                        [ 0, 1, 2, 10 ]

                    values =
                        List.concatMap
                            (\currentlyPregnant ->
                                List.concatMap
                                    (\term ->
                                        List.concatMap
                                            (\preTerm ->
                                                List.concatMap
                                                    (\atTerm ->
                                                        List.concatMap
                                                            (\stillPreTerm ->
                                                                List.concatMap
                                                                    (\abortions ->
                                                                        List.map
                                                                            (\liveChildren ->
                                                                                { currentlyPregnant = currentlyPregnant
                                                                                , termPregnancy = term
                                                                                , preTermPregnancy = preTerm
                                                                                , stillbirthsAtTerm = atTerm
                                                                                , stillbirthsPreTerm = stillPreTerm
                                                                                , abortions = abortions
                                                                                , liveChildren = liveChildren
                                                                                }
                                                                            )
                                                                            counts
                                                                    )
                                                                    counts
                                                            )
                                                            counts
                                                    )
                                                    counts
                                            )
                                            counts
                                    )
                                    counts
                            )
                            [ True, False ]
                in
                values
                    |> List.filter
                        (\value ->
                            skipObstetricHistorySecondStep (Just value)
                                /= (generateGravida value == "01" && generatePara value == "0000")
                        )
                    |> Expect.equal []
        ]



-- LATE LAB RESULTS: DIAGNOSES FOLLOW THE VISIT'S PREGNANCY WEEK
--
-- A nurse visit sends a test to the lab; its result is entered on a later
-- day, from the encounter's Lab results page (Case Management), by a lab
-- technician, or from a later visit's lab history. Every time a result is
-- saved, the app reassesses the whole encounter with
-- `generatePrenatalDiagnosesForNurse <the visit's assembled data>`
-- and stores the outcome; the diagnoses it adds over the stored set decide
-- the emergency-referral warning and the recurrent Next Steps.
--
-- Oracle (issue "Expected behaviour"): every week-dependent line is graded
-- at the pregnancy week of the VISIT (`assembled.encounter.startDate`), never
-- at the week of the day the result is entered. The week lines are:
--   * chronic (< 20) vs gestational (>= 20) hypertension;
--   * diabetes (<= 20) vs gestational diabetes (> 20);
--   * moderate preeclampsia [20, 37) vs its emergency EGA37+ variant;
--   * hyperemesis gravidum (< 20) vs severe vomiting (>= 20);
--   * the emergency lines at 22 and 37 weeks;
--   * the preeclampsia risk diagnoses, offered from week 12.
--
-- Each scenario is built the way the app builds it: the visit's measurements
-- are first assessed on the visit day (the set the nurse's device stores when
-- the encounter is completed), then the result is entered into the pending
-- test, the visit-day set is stored on the encounter, the encounter is
-- closed on the visit day, and the encounter is assessed again after the
-- entry. Ages are written as (weeks, days) of EGA on the visit day.


{-| The visit day of every late-result scenario.
-}
visitDay : NominalDate
visitDay =
    currentDate


{-| The LMP that puts `visitDay` at the given EGA (weeks, days).
-}
lmpForVisitAt : ( Int, Int ) -> NominalDate
lmpForVisitAt ( weeks, days ) =
    Date.add Date.Days -(weeks * 7 + days) visitDay


{-| A nurse encounter started on `visitDay` at the given EGA, after the given
earlier nurse visits, with the given diagnoses stored on it.
-}
visitAssembled : ( Int, Int ) -> List PreviousEncounterData -> EverySet PrenatalDiagnosis -> PrenatalMeasurements -> AssembledData
visitAssembled ega previousEncounters storedDiagnoses measurements =
    let
        base =
            testAssembled measurements

        encounter =
            base.encounter
    in
    { base
        | encounter = { encounter | startDate = visitDay, diagnoses = storedDiagnoses }
        , globalLmpDate = Just (lmpForVisitAt ega)
        , nursePreviousEncountersData = previousEncounters
    }


{-| `visit` is the visit's assembled data with the visit-day diagnoses stored
on it; `saved` is the encounter after the result is entered, with the
reassessed diagnoses stored on it, the state the Lab Results page reads.
-}
type alias LateResult =
    { visitDayDiagnoses : EverySet PrenatalDiagnosis
    , afterEntry : EverySet PrenatalDiagnosis
    , visit : AssembledData
    , saved : AssembledData
    }


{-| A visit at `ega`, after `previousEncounters`, with `visitMeasurements`
recorded (the lab test still pending). `enterResult` fills in the pending
result, and the encounter is reassessed with the visit-day diagnoses stored on
it. The days-later argument names when the result is entered; the assessment
reads no entry date.
-}
enterResultLaterAfter : List PreviousEncounterData -> ( Int, Int ) -> Int -> (PrenatalMeasurements -> PrenatalMeasurements) -> PrenatalMeasurements -> LateResult
enterResultLaterAfter previousEncounters ega _ enterResult visitMeasurements =
    let
        stored =
            generatePrenatalDiagnosesForNurse
                (visitAssembled ega previousEncounters EverySet.empty visitMeasurements)

        assembled =
            visitAssembled ega previousEncounters stored (enterResult visitMeasurements)

        encounter =
            assembled.encounter

        reassessed =
            { assembled | encounter = { encounter | endDate = Just visitDay } }

        afterEntry =
            generatePrenatalDiagnosesForNurse reassessed

        reassessedEncounter =
            reassessed.encounter
    in
    { visitDayDiagnoses = stored
    , afterEntry = afterEntry
    , visit = visitAssembled ega previousEncounters stored visitMeasurements
    , saved = { reassessed | encounter = { reassessedEncounter | diagnoses = afterEntry } }
    }


{-| `enterResultLaterAfter` for a first visit.
-}
enterResultLater : ( Int, Int ) -> Int -> (PrenatalMeasurements -> PrenatalMeasurements) -> PrenatalMeasurements -> LateResult
enterResultLater =
    enterResultLaterAfter []


{-| The diagnoses the late result added over the stored visit-day set: the
ones that bring up the emergency-referral warning and recurrent Next Steps.
-}
addedByLateResult : LateResult -> List PrenatalDiagnosis
addedByLateResult result =
    EverySet.diff result.afterEntry result.visitDayDiagnoses
        |> EverySet.toList


{-| Membership of each given diagnosis in a diagnosis set.
-}
membership : List PrenatalDiagnosis -> EverySet PrenatalDiagnosis -> List ( PrenatalDiagnosis, Bool )
membership diagnoses set =
    List.map (\diagnosis -> ( diagnosis, EverySet.member diagnosis set )) diagnoses



-- Pending tests and the results entered into them later. A test sent to the
-- lab is "run today" on the visit day, with no result and without the
-- immediate-result prerequisite.


pendingHemoglobinValue : HemoglobinTestValue
pendingHemoglobinValue =
    { executionNote = TestNoteRunToday
    , executionDate = Just visitDay
    , testPrerequisites = Nothing
    , hemoglobinCount = Nothing
    }


withPendingHemoglobin : PrenatalMeasurements -> PrenatalMeasurements
withPendingHemoglobin measurements =
    { measurements | hemoglobinTest = TestFixtures.wrapMeasurement visitDay pendingHemoglobinValue }


{-| A nurse enters the hemoglobin count on the encounter's Lab results page.
-}
enterHemoglobin : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterHemoglobin count measurements =
    { measurements
        | hemoglobinTest =
            TestFixtures.wrapMeasurement visitDay { pendingHemoglobinValue | hemoglobinCount = Just count }
    }


{-| A lab technician enters the hemoglobin count, confirming the test was run.
-}
enterHemoglobinByLabTech : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterHemoglobinByLabTech count measurements =
    { measurements
        | hemoglobinTest =
            TestFixtures.wrapMeasurement visitDay
                { pendingHemoglobinValue
                    | executionNote = TestNoteRunConfirmedByLabTech
                    , hemoglobinCount = Just count
                }
    }


{-| Fasting random blood sugar, sent to the lab: above 126 means diabetes.
-}
pendingRandomBloodSugarValue : RandomBloodSugarTestValue PrenatalEncounterId
pendingRandomBloodSugarValue =
    { executionNote = TestNoteRunToday
    , executionDate = Just visitDay
    , testPrerequisites = Just (EverySet.singleton PrerequisiteFastFor12h)
    , sugarCount = Nothing
    , originatingEncounter = Nothing
    }


withPendingRandomBloodSugar : PrenatalMeasurements -> PrenatalMeasurements
withPendingRandomBloodSugar measurements =
    { measurements | randomBloodSugarTest = TestFixtures.wrapMeasurement visitDay pendingRandomBloodSugarValue }


enterRandomBloodSugar : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterRandomBloodSugar count measurements =
    { measurements
        | randomBloodSugarTest =
            TestFixtures.wrapMeasurement visitDay { pendingRandomBloodSugarValue | sugarCount = Just count }
    }


{-| The result is entered from a later visit's lab history: it is saved on
the visit's own test, naming the later visit as the one it came from.
-}
enterRandomBloodSugarFromLaterVisit : Float -> PrenatalMeasurements -> PrenatalMeasurements
enterRandomBloodSugarFromLaterVisit count measurements =
    { measurements
        | randomBloodSugarTest =
            TestFixtures.wrapMeasurement visitDay
                { pendingRandomBloodSugarValue
                    | sugarCount = Just count
                    , originatingEncounter = Just (toEntityUuid "later-visit")
                }
    }


pendingUrineDipstickValue : UrineDipstickTestValue
pendingUrineDipstickValue =
    let
        immediate =
            urineDipstickValueWith ProteinPlus1
    in
    { immediate | testPrerequisites = Nothing, protein = Nothing }


withPendingUrineDipstick : PrenatalMeasurements -> PrenatalMeasurements
withPendingUrineDipstick measurements =
    { measurements | urineDipstickTest = TestFixtures.wrapMeasurement visitDay pendingUrineDipstickValue }


enterUrineProtein : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
enterUrineProtein protein measurements =
    { measurements
        | urineDipstickTest =
            TestFixtures.wrapMeasurement visitDay { pendingUrineDipstickValue | protein = Just protein }
    }


{-| Visit measurements: high blood pressure (165/115) and a hemoglobin test
sent to the lab.
-}
highBloodPressureVisit : PrenatalMeasurements
highBloodPressureVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withPendingHemoglobin


{-| Visit measurements: a danger sign and a hemoglobin test sent to the lab.
-}
dangerSignVisit : DangerSign -> PrenatalMeasurements
dangerSignVisit sign =
    emptyPrenatalMeasurements
        |> withDangerSigns (EverySet.singleton sign)
        |> withPendingHemoglobin


{-| Visit measurements: moderate preeclampsia found at the visit (high blood
pressure with urine protein +1, read at the visit) and a hemoglobin test
sent to the lab.
-}
moderatePreeclampsiaVisit : PrenatalMeasurements
moderatePreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withUrineProtein ProteinPlus1
        |> withPendingHemoglobin


{-| Obstetric history of a second pregnancy whose step 2 carries the given
sign and delivery period: preeclampsia at a previous pregnancy gives HIGH
risk of preeclampsia, more than 10 years since delivery gives MODERATE risk.
-}
withPreeclampsiaRiskHistory : ObstetricHistoryStep2Sign -> PreviousDeliveryPeriod -> PrenatalMeasurements -> PrenatalMeasurements
withPreeclampsiaRiskHistory sign period measurements =
    { measurements
        | obstetricHistory =
            wrapMeasurement
                { currentlyPregnant = True
                , termPregnancy = 1
                , preTermPregnancy = 0
                , stillbirthsAtTerm = 0
                , stillbirthsPreTerm = 0
                , abortions = 0
                , liveChildren = 1
                }
        , obstetricHistoryStep2 =
            wrapMeasurement
                { cSections = 0
                , cSectionReason = Nothing
                , previousDelivery = EverySet.singleton NoPreviousDeliverySign
                , previousDeliveryPeriod = EverySet.singleton period
                , signs = EverySet.singleton sign
                }
    }


highRiskOfPreeclampsiaVisit : PrenatalMeasurements
highRiskOfPreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withPreeclampsiaRiskHistory ObstetricHistoryPreeclampsiaPreviousPregnancy Neither
        |> withPendingHemoglobin


moderateRiskOfPreeclampsiaVisit : PrenatalMeasurements
moderateRiskOfPreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withPreeclampsiaRiskHistory NoObstetricHistoryStep2Sign MoreThan10Years
        |> withPendingHemoglobin


preeclampsiaRiskDiagnoses : List PrenatalDiagnosis
preeclampsiaRiskDiagnoses =
    [ DiagnosisHighRiskOfPreeclampsiaInitialPhase
    , DiagnosisHighRiskOfPreeclampsiaRecurrentPhase
    , DiagnosisModerateRiskOfPreeclampsia
    ]


lateLabResultSameDayTest : Test
lateLabResultSameDayTest =
    describe "late lab result: A lab result entered on the visit day gives the same diagnoses as it does today"
        [ test "high blood pressure at 19 weeks 5 days, result entered the same day: chronic hypertension, not gestational" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]
        , test "high blood pressure at 20 weeks 0 days, result entered the same day: gestational hypertension, not chronic" <|
            \_ ->
                enterResultLater ( 20, 0 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, False ), ( DiagnosisGestationalHypertensionImmediate, True ) ]
        , test "high blood sugar from a visit at 20 weeks 6 days, entered the same day: diabetes, not gestational diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 0 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        , test "high blood sugar from a visit at 21 weeks 0 days, entered the same day: gestational diabetes, not diabetes" <|
            \_ ->
                enterResultLater ( 21, 0 ) 0 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, True ) ]
        , test "moderate preeclampsia at 36 weeks 6 days, result entered the same day: moderate preeclampsia, not the EGA 37+ emergency" <|
            \_ ->
                enterResultLater ( 36, 6 ) 0 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]
        , test "moderate preeclampsia at 37 weeks 0 days, result entered the same day: the EGA 37+ emergency" <|
            \_ ->
                enterResultLater ( 37, 0 ) 0 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, False ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, True ) ]
        , test "severe vomiting at 19 weeks 6 days, result entered the same day: hyperemesis gravidum" <|
            \_ ->
                enterResultLater ( 19, 6 ) 0 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "severe vomiting at 20 weeks 0 days, result entered the same day: severe vomiting" <|
            \_ ->
                enterResultLater ( 20, 0 ) 0 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, False ), ( DiagnosisSevereVomiting, True ) ]
        , test "a normal result entered the same day adds nothing to the stored diagnoses" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 12) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal []
        , test "a low hemoglobin entered the same day adds only moderate anemia" <|
            \_ ->
                enterResultLater ( 19, 5 ) 0 (enterHemoglobin 9) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal [ DiagnosisModerateAnemiaRecurrentPhase ]
        , test "a pregnancy with no LMP: a high blood pressure gets no week-graded diagnosis on any day" <|
            \_ ->
                let
                    assembled =
                        visitAssembled ( 19, 5 ) [] EverySet.empty (enterHemoglobin 12 highBloodPressureVisit)

                    undated =
                        { assembled | globalLmpDate = Nothing }
                in
                [ visitDay, Date.add Date.Days 3 visitDay ]
                    |> List.map
                        (\_ ->
                            generatePrenatalDiagnosesForNurse undated
                                |> EverySet.toList
                                |> List.filter (\diagnosis -> List.member diagnosis [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ])
                        )
                    |> Expect.equal [ [], [] ]
        ]


{-| A random blood sugar test sent to the lab, and nothing else.
-}
withPendingRandomBloodSugar_ : PrenatalMeasurements
withPendingRandomBloodSugar_ =
    withPendingRandomBloodSugar emptyPrenatalMeasurements


lateLabResultHypertensionTest : Test
lateLabResultHypertensionTest =
    let
        chronicNotGestational =
            [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]

        hypertension =
            membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
    in
    describe "late lab result: chronic vs gestational hypertension"
        [ test "High blood pressure measured before week 20 stays chronic hypertension when a lab result for that visit is entered after week 20" <|
            -- The issue's own scenario: 19 weeks 5 days, result three days
            -- later at 20 weeks 1 day.
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "visit at 19 weeks 6 days, result entered the next day at 20 weeks 0 days: chronic hypertension stays" <|
            \_ ->
                enterResultLater ( 19, 6 ) 1 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "visit at 16 weeks, result entered ten weeks later: chronic hypertension stays" <|
            \_ ->
                enterResultLater ( 16, 0 ) 70 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "marginal blood pressure confirmed on recheck at 19 weeks 5 days stays chronic hypertension after recheck when a result is entered at 20 weeks 1 day" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitalsRepeated 145 95
                    |> withPendingHemoglobin
                    |> enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12)
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionAfterRecheck, DiagnosisGestationalHypertensionAfterRecheck ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionAfterRecheck, True ), ( DiagnosisGestationalHypertensionAfterRecheck, False ) ]
        , test "a second visit at 19 weeks 5 days (first visit at 15 weeks, no hypertension then): chronic hypertension stays after a result at 20 weeks 1 day" <|
            \_ ->
                let
                    firstVisit =
                        { startDate = Date.add Date.Weeks -4 visitDay
                        , diagnoses = EverySet.empty
                        , pastDiagnoses = EverySet.empty
                        , measurements = emptyPrenatalMeasurements
                        }
                in
                enterResultLaterAfter [ firstVisit ] ( 19, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal chronicNotGestational
        , test "guard: high blood pressure at 20 weeks 1 day is gestational hypertension, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 20, 1 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> .afterEntry
                    |> hypertension
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, False ), ( DiagnosisGestationalHypertensionImmediate, True ) ]
        ]


lateLabResultDiabetesTest : Test
lateLabResultDiabetesTest =
    let
        diabetes =
            membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]

        diabetesNotGestational =
            [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
    in
    describe "late lab result: diabetes vs gestational diabetes"
        [ test "A high blood sugar result from a visit at week 20 or earlier is diagnosed as diabetes, not gestational diabetes, even when it is entered after week 20" <|
            -- Visit at 20 weeks 6 days (still week 20), result entered at
            -- 21 weeks 2 days.
            \_ ->
                enterResultLater ( 20, 6 ) 3 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "visit at 20 weeks 6 days, result entered the next day at 21 weeks 0 days: diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 1 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "visit at 14 weeks, result entered eight weeks later at 22 weeks: diabetes" <|
            \_ ->
                enterResultLater ( 14, 0 ) 56 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal diabetesNotGestational
        , test "guard: a high blood sugar from a visit at 21 weeks 0 days is gestational diabetes when entered later" <|
            \_ ->
                enterResultLater ( 21, 0 ) 3 (enterRandomBloodSugar 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, True ) ]
        , test "guard: a normal blood sugar from a visit at week 20 entered after week 20 gives neither diabetes" <|
            \_ ->
                enterResultLater ( 20, 6 ) 3 (enterRandomBloodSugar 90) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> diabetes
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, False ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        ]


lateLabResultModeratePreeclampsiaTest : Test
lateLabResultModeratePreeclampsiaTest =
    describe "late lab result: moderate preeclampsia before week 37"
        [ test "Moderate preeclampsia found at a visit before week 37 is still diagnosed when a lab result for that visit is entered at week 37 or later" <|
            -- Visit at 36 weeks 5 days, result entered at 37 weeks 1 day.
            \_ ->
                enterResultLater ( 36, 5 ) 3 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisModeratePreeclampsiaInitialPhase
                    |> Expect.equal True
        , test "visit at 36 weeks 6 days, result entered the next day at 37 weeks 0 days: moderate preeclampsia, and not its EGA 37+ emergency variant" <|
            \_ ->
                enterResultLater ( 36, 6 ) 1 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]

        -- The visit records high blood pressure and sends urine to the lab;
        -- the protein result entered at 37 weeks 1 day completes moderate
        -- preeclampsia. The visit was at week 36, so it is the [20, 37)
        -- diagnosis, not the EGA 37+ emergency one.
        , test "high blood pressure at 36 weeks 5 days with urine protein +1 entered at 37 weeks 1 day: moderate preeclampsia, not the EGA 37+ emergency" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 36, 5 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaRecurrentPhase, DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaRecurrentPhase, True ), ( DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus, False ) ]
        , test "guard: moderate preeclampsia at 37 weeks 0 days stays the EGA 37+ emergency when a result is entered later" <|
            \_ ->
                enterResultLater ( 37, 0 ) 3 (enterHemoglobin 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, False ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, True ) ]
        ]


lateLabResultHyperemesisTest : Test
lateLabResultHyperemesisTest =
    let
        vomiting =
            membership [ DiagnosisHyperemesisGravidum, DiagnosisSevereVomiting ]
    in
    describe "late lab result: hyperemesis gravidum vs severe vomiting"
        [ test "A severe-vomiting danger sign at a visit before week 20 stays hyperemesis gravidum, not severe vomiting, when a lab result for that visit is entered after week 20" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "visit at 19 weeks 6 days, result entered the next day at 20 weeks 0 days: hyperemesis gravidum stays" <|
            \_ ->
                enterResultLater ( 19, 6 ) 1 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, True ), ( DiagnosisSevereVomiting, False ) ]
        , test "guard: severe vomiting at 20 weeks 0 days stays severe vomiting when a result is entered later" <|
            \_ ->
                enterResultLater ( 20, 0 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> .afterEntry
                    |> vomiting
                    |> Expect.equal [ ( DiagnosisHyperemesisGravidum, False ), ( DiagnosisSevereVomiting, True ) ]
        ]


{-| Every scenario here crosses one week line between the visit and the entry
day, and enters a normal hemoglobin (12), which by itself diagnoses nothing.
So whatever the reassessment adds over the stored visit-day set could only
come from the later week, and must be nothing: an added diagnosis is what
brings up the emergency-referral warning and recurrent Next Steps.
-}
lateLabResultAddsNothingTest : Test
lateLabResultAddsNothingTest =
    let
        addsNothing ega daysLater visitMeasurements =
            enterResultLater ega daysLater (enterHemoglobin 12) visitMeasurements
                |> addedByLateResult
                |> Expect.equal []
    in
    describe "late lab result: diagnoses added over the stored set"
        [ test "Entering a late lab result never brings up an emergency-referral warning, or recurrent Next Steps, for a diagnosis that only the later week would produce" <|
            -- Severe vomiting at 19 weeks 5 days, result at 20 weeks 1 day:
            -- the severe-vomiting emergency must not be added.
            \_ ->
                addsNothing ( 19, 5 ) 3 (dangerSignVisit SevereVomiting)
        , test "high blood pressure at 19 weeks 5 days, result at 20 weeks 1 day: no gestational hypertension added" <|
            \_ ->
                addsNothing ( 19, 5 ) 3 highBloodPressureVisit
        , test "moderate preeclampsia at 36 weeks 5 days, result at 37 weeks 1 day: no EGA 37+ emergency added" <|
            \_ ->
                addsNothing ( 36, 5 ) 3 moderatePreeclampsiaVisit
        , test "headache with blurred vision at 36 weeks 5 days, result at 37 weeks 1 day: no EGA 37+ severe preeclampsia emergency added" <|
            \_ ->
                addsNothing ( 36, 5 ) 3 (dangerSignVisit HeadacheBlurredVision)
        , test "vaginal bleeding at 22 weeks 5 days, result at 23 weeks 1 day: no placenta previa, abruption or uterine rupture added" <|
            \_ ->
                addsNothing ( 22, 5 ) 3 (dangerSignVisit VaginalBleeding)
        , test "abdominal pain at 21 weeks 5 days, result at 22 weeks 1 day: no obstructed labor or its follow-ons added" <|
            \_ ->
                addsNothing ( 21, 5 ) 3 (dangerSignVisit AbdominalPain)
        , test "leaking vaginal fluid at 37 weeks 5 days, result at 38 weeks 1 day: no PROM added" <|
            \_ ->
                addsNothing ( 37, 5 ) 3 (dangerSignVisit GushLeakingVaginalFluid)
        , test "a low hemoglobin entered at 20 weeks 1 day for high blood pressure at 19 weeks 5 days adds only moderate anemia" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 9) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal [ DiagnosisModerateAnemiaRecurrentPhase ]
        , test "the reassessment keeps the stored week-graded emergency: severe vomiting at 19 weeks 5 days keeps hyperemesis gravidum at 20 weeks 1 day" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobin 12) (dangerSignVisit SevereVomiting)
                    |> (\result -> EverySet.diff result.visitDayDiagnoses result.afterEntry |> EverySet.toList)
                    |> Expect.equal []
        ]


lateLabResultPreeclampsiaRiskTest : Test
lateLabResultPreeclampsiaRiskTest =
    let
        risks =
            membership preeclampsiaRiskDiagnoses
    in
    describe "late lab result: preeclampsia risk from week 12"
        [ test "The preeclampsia risk diagnoses, which start at week 12, follow the visit's week, not the entry day's" <|
            -- Previous-pregnancy preeclampsia (high risk) at a visit at 11
            -- weeks 5 days; result entered at 12 weeks 1 day.
            \_ ->
                enterResultLater ( 11, 5 ) 3 (enterHemoglobin 12) highRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> risks
                    |> List.filter Tuple.second
                    |> Expect.equal []
        , test "moderate risk (more than 10 years since delivery) at 11 weeks 6 days, result at 12 weeks 0 days: no risk diagnosis" <|
            \_ ->
                enterResultLater ( 11, 6 ) 1 (enterHemoglobin 12) moderateRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> risks
                    |> List.filter Tuple.second
                    |> Expect.equal []
        , test "high blood pressure at 11 weeks 5 days, result at 12 weeks 1 day: chronic hypertension stays, and no high risk of preeclampsia is added" <|
            \_ ->
                enterResultLater ( 11, 5 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> addedByLateResult
                    |> Expect.equal []
        , test "guard: previous-pregnancy preeclampsia at 12 weeks 0 days gives high risk, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 12, 0 ) 3 (enterHemoglobin 12) highRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisHighRiskOfPreeclampsiaInitialPhase
                    |> Expect.equal True
        , test "guard: more than 10 years since delivery at 12 weeks 0 days gives moderate risk, also when the result is entered later" <|
            \_ ->
                enterResultLater ( 12, 0 ) 3 (enterHemoglobin 12) moderateRiskOfPreeclampsiaVisit
                    |> .afterEntry
                    |> EverySet.member DiagnosisModerateRiskOfPreeclampsia
                    |> Expect.equal True
        ]


{-| The lab technician's device and a later visit's lab history run the same
reassessment on the visit's encounter; only the stored result differs in
shape (a lab technician's confirmation note, or the later visit named as the
result's origin).
-}
lateLabResultLabTechAndHistoryTest : Test
lateLabResultLabTechAndHistoryTest =
    describe "late lab result: lab technician and later visit's lab history"
        [ test "The same holds when the result is entered by a lab technician, and when it is entered from a later visit's lab history" <|
            -- Lab technician: hemoglobin for high blood pressure at 19 weeks
            -- 5 days, entered at 20 weeks 1 day.
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobinByLabTech 12) highBloodPressureVisit
                    |> .afterEntry
                    |> membership [ DiagnosisChronicHypertensionImmediate, DiagnosisGestationalHypertensionImmediate ]
                    |> Expect.equal [ ( DiagnosisChronicHypertensionImmediate, True ), ( DiagnosisGestationalHypertensionImmediate, False ) ]
        , test "lab technician: severe vomiting at 19 weeks 5 days with a result at 20 weeks 1 day adds nothing" <|
            \_ ->
                enterResultLater ( 19, 5 ) 3 (enterHemoglobinByLabTech 12) (dangerSignVisit SevereVomiting)
                    |> addedByLateResult
                    |> Expect.equal []
        , test "lab technician: moderate preeclampsia at 36 weeks 5 days stays after a result at 37 weeks 1 day" <|
            \_ ->
                enterResultLater ( 36, 5 ) 3 (enterHemoglobinByLabTech 12) moderatePreeclampsiaVisit
                    |> .afterEntry
                    |> membership [ DiagnosisModeratePreeclampsiaInitialPhase, DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                    |> Expect.equal [ ( DiagnosisModeratePreeclampsiaInitialPhase, True ), ( DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus, False ) ]
        , test "lab history: a high blood sugar from a visit at 18 weeks entered at the next visit at 22 weeks is diabetes, not gestational diabetes" <|
            \_ ->
                enterResultLater ( 18, 0 ) 28 (enterRandomBloodSugarFromLaterVisit 130) withPendingRandomBloodSugar_
                    |> .afterEntry
                    |> membership [ DiagnosisDiabetesRecurrentPhase, DiagnosisGestationalDiabetesRecurrentPhase ]
                    |> Expect.equal [ ( DiagnosisDiabetesRecurrentPhase, True ), ( DiagnosisGestationalDiabetesRecurrentPhase, False ) ]
        , test "lab history: a normal blood sugar for high blood pressure at 19 weeks 5 days, entered at the next visit at 23 weeks 5 days, keeps chronic hypertension and adds nothing" <|
            \_ ->
                let
                    result =
                        emptyPrenatalMeasurements
                            |> withVitals 165 115
                            |> withPendingRandomBloodSugar
                            |> enterResultLater ( 19, 5 ) 28 (enterRandomBloodSugarFromLaterVisit 90)
                in
                ( EverySet.member DiagnosisChronicHypertensionImmediate result.afterEntry
                , addedByLateResult result
                )
                    |> Expect.equal ( True, [] )
        ]



-- LATE URINE PROTEIN: SEVERE PREECLAMPSIA FOUND IN LAB RESULTS
--
-- Severe preeclampsia is blood pressure of 160/110 or more, a severe sign
-- from the physical exam (wheezes, crackles or right upper quadrant
-- tenderness), and urine protein +1 or more; or headache with blurred vision.
--
-- The scenario under test: the nurse records the blood pressure and the sign
-- at the visit, and sends the urine dipstick to the lab (no immediate
-- result). The protein result is entered later on the encounter's Lab Results
-- page. Saving it reassesses the encounter; the diagnoses it adds over the
-- stored set decide the emergency warning on Lab Results
-- (`Pages.Prenatal.RecurrentActivity.Utils.diagnosisRequiresEmergencyReferal`),
-- and the stored set decides whether Lab Results' Next Steps asks for a
-- hospital referral (`diagnosesCausingHospitalReferralByPhase
-- PrenatalEncounterPhaseRecurrent`).
--
-- Oracle (issue "Expected behaviour"): the later protein result gives severe
-- preeclampsia, Lab Results' Next Steps asks to refer to hospital, and from 37
-- weeks saving the result shows the emergency warning. When the protein is
-- read at the visit, everything stays as it is today.
--
-- The tests name the disease (any of the four severe preeclampsia variants),
-- not a variant, except where "as today" pins today's variant.


severePreeclampsiaVariants : List PrenatalDiagnosis
severePreeclampsiaVariants =
    [ DiagnosisSeverePreeclampsiaInitialPhase
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhase
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    ]


{-| The severe preeclampsia variants among the given diagnoses.
-}
severePreeclampsiaAmong : List PrenatalDiagnosis -> List PrenatalDiagnosis
severePreeclampsiaAmong diagnoses =
    List.filter (\diagnosis -> List.member diagnosis diagnoses) severePreeclampsiaVariants


{-| How the urine dipstick form stores "no immediate result".
-}
notImmediatePrerequisites : Maybe (EverySet TestPrerequisite)
notImmediatePrerequisites =
    Just (EverySet.singleton NoTestPrerequisites)


urineDipstickSentToLabValue : UrineDipstickTestValue
urineDipstickSentToLabValue =
    { pendingUrineDipstickValue | testPrerequisites = notImmediatePrerequisites }


{-| The urine dipstick sent to the lab at the visit, stored as the form stores
it when the nurse answers that there is no immediate result.
-}
withUrineDipstickSentToLab : PrenatalMeasurements -> PrenatalMeasurements
withUrineDipstickSentToLab measurements =
    { measurements | urineDipstickTest = TestFixtures.wrapMeasurement visitDay urineDipstickSentToLabValue }


{-| The nurse enters the protein result on the encounter's Lab Results page.
-}
enterProteinInLabResults : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
enterProteinInLabResults protein measurements =
    { measurements
        | urineDipstickTest =
            TestFixtures.wrapMeasurement visitDay { urineDipstickSentToLabValue | protein = Just protein }
    }


{-| A lab technician enters the protein result, confirming the test was run.
-}
enterProteinByLabTech : ProteinValue -> PrenatalMeasurements -> PrenatalMeasurements
enterProteinByLabTech protein measurements =
    { measurements
        | urineDipstickTest =
            TestFixtures.wrapMeasurement visitDay
                { urineDipstickSentToLabValue
                    | executionNote = TestNoteRunConfirmedByLabTech
                    , protein = Just protein
                }
    }


{-| Visit measurements: the given blood pressure (sys, dia), a physical exam
with the given lungs and abdomen findings, and the urine dipstick sent to the
lab.
-}
awaitingProteinVisit : ( Float, Float ) -> EverySet LungsCPESign -> EverySet AbdomenCPESign -> PrenatalMeasurements
awaitingProteinVisit ( sys, dia ) lungs abdomen =
    { emptyPrenatalMeasurements | corePhysicalExam = wrapMeasurement (corePhysicalExamValueWith lungs abdomen) }
        |> withVitals sys dia
        |> withUrineDipstickSentToLab


{-| The issue's scenario: blood pressure 165/115, right upper quadrant
tenderness, and the urine dipstick sent to the lab.
-}
severeSignsAwaitingProteinVisit : PrenatalMeasurements
severeSignsAwaitingProteinVisit =
    awaitingProteinVisit ( 165, 115 ) (EverySet.singleton NormalLungs) (EverySet.singleton TPRightUpper)


{-| Visit measurements: blood pressure 165/115, right upper quadrant
tenderness, and urine protein +1 read at the visit (immediate result); a
hemoglobin test goes to the lab, so a result can still be entered later.
-}
severePreeclampsiaAtVisit : PrenatalMeasurements
severePreeclampsiaAtVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withSeverePreeclampsiaSigns
        |> withUrineProtein ProteinPlus1
        |> withPendingHemoglobin


{-| Visit measurements: headache with blurred vision, normal blood pressure,
and the given urine dipstick step.
-}
headacheBlurredVisionVisit : (PrenatalMeasurements -> PrenatalMeasurements) -> PrenatalMeasurements
headacheBlurredVisionVisit withUrine =
    emptyPrenatalMeasurements
        |> withDangerSigns (EverySet.singleton HeadacheBlurredVision)
        |> withVitals 120 80
        |> withPendingHemoglobin
        |> withUrine


{-| Severe preeclampsia in the reassessed set the late result stores.
-}
severePreeclampsiaAfterEntry : LateResult -> List PrenatalDiagnosis
severePreeclampsiaAfterEntry result =
    severePreeclampsiaAmong (EverySet.toList result.afterEntry)


{-| Severe preeclampsia among the diagnoses for which Lab Results' Next Steps
asks to refer to hospital.
-}
labResultsHospitalReferral : LateResult -> List PrenatalDiagnosis
labResultsHospitalReferral result =
    diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseRecurrent result.saved
        |> EverySet.toList
        |> severePreeclampsiaAmong


{-| The added diagnoses that show the emergency warning on Lab Results and
jump to its Next Steps.
-}
labResultsWarning : LateResult -> List PrenatalDiagnosis
labResultsWarning result =
    addedByLateResult result
        |> List.filter Pages.Prenatal.RecurrentActivity.Utils.diagnosisRequiresEmergencyReferal


{-| Severe preeclampsia among the diagnoses for which the visit's own Next
Steps asks to refer to hospital.
-}
visitHospitalReferral : LateResult -> List PrenatalDiagnosis
visitHospitalReferral result =
    diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseInitial result.visit
        |> EverySet.toList
        |> severePreeclampsiaAmong


{-| The visit-day diagnoses that show the emergency warning at the visit.
-}
visitWarning : LateResult -> List PrenatalDiagnosis
visitWarning result =
    EverySet.toList result.visitDayDiagnoses
        |> List.filter Pages.Prenatal.Encounter.Utils.diagnosisRequiresEmergencyReferal


{-| Today's progress report wording for severe preeclampsia, per language.
-}
progressReportWording : PrenatalDiagnosis -> ( String, String )
progressReportWording diagnosis =
    ( Translate.translate English (Translate.PrenatalDiagnosisForProgressReport diagnosis)
    , Translate.translate Kinyarwanda (Translate.PrenatalDiagnosisForProgressReport diagnosis)
    )


lateUrineProteinSeverePreeclampsiaTest : Test
lateUrineProteinSeverePreeclampsiaTest =
    describe "late urine protein: severe preeclampsia found in Lab Results"
        [ lateProteinBefore37WeeksTest
        , lateProteinFrom37WeeksTest
        , immediateProteinAtVisitTest
        , headacheBlurredVisionAtVisitTest
        , lateProteinTraceOrNegativeTest
        , lateProteinByLabTechTest
        , severePreeclampsiaProgressReportTest
        ]


lateProteinBefore37WeeksTest : Test
lateProteinBefore37WeeksTest =
    let
        referredInLabResults result =
            ( not (List.isEmpty (severePreeclampsiaAfterEntry result)), not (List.isEmpty (labResultsHospitalReferral result)) )
    in
    describe "before 37 weeks"
        [ test "Before 37 weeks: blood pressure of 160/110 or more, a severe sign (wheezes, crackles or right upper quadrant tenderness), and urine protein +1 or more entered later in Lab Results gives severe preeclampsia. Lab Results' Next Steps asks to refer to hospital." <|
            -- The issue's reproduction: 28 weeks, 165/115, right upper
            -- quadrant tenderness, protein +1 entered in Lab Results.
            \_ ->
                enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> referredInLabResults
                    |> Expect.equal ( True, True )
        , test "guard: the visit itself, with the protein still pending, finds no severe preeclampsia" <|
            \_ ->
                enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> .visitDayDiagnoses
                    |> EverySet.toList
                    |> severePreeclampsiaAmong
                    |> Expect.equal []
        , test "wheezes as the severe sign: severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                awaitingProteinVisit ( 165, 115 ) (EverySet.singleton Wheezes) (EverySet.singleton NormalAbdomen)
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                    |> referredInLabResults
                    |> Expect.equal ( True, True )
        , test "crackles as the severe sign: severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                awaitingProteinVisit ( 165, 115 ) (EverySet.singleton Crackles) (EverySet.singleton NormalAbdomen)
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                    |> referredInLabResults
                    |> Expect.equal ( True, True )
        , test "protein +2, +3 and +4 entered later each give severe preeclampsia with the Lab Results hospital referral" <|
            \_ ->
                [ ProteinPlus2, ProteinPlus3, ProteinPlus4 ]
                    |> List.map
                        (\protein ->
                            enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults protein) severeSignsAwaitingProteinVisit
                                |> referredInLabResults
                        )
                    |> Expect.equal [ ( True, True ), ( True, True ), ( True, True ) ]
        , test "blood pressure exactly 160/110: severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                awaitingProteinVisit ( 160, 110 ) (EverySet.singleton NormalLungs) (EverySet.singleton TPRightUpper)
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                    |> referredInLabResults
                    |> Expect.equal ( True, True )

        -- The other stored shape of "not an immediate result": no
        -- prerequisites recorded at all.
        , test "urine dipstick stored with no prerequisites at all, protein +1 entered later: severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withSeverePreeclampsiaSigns
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 28, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> referredInLabResults
                    |> Expect.equal ( True, True )

        -- A visit on the last day before week 37, the result entered at 37
        -- weeks 2 days: the visit's week decides, so this is the before-37
        -- case.
        , test "visit at 36 weeks 6 days, protein +1 entered after week 37: severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                enterResultLater ( 36, 6 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> referredInLabResults
                    |> Expect.equal ( True, True )

        -- Inferred boundary: the expected behaviour names the emergency
        -- warning only from 37 weeks, and today's visit-time severe
        -- preeclampsia before 37 weeks shows none.
        , test "before 37 weeks, saving the protein result shows no emergency warning (28 weeks, and a visit at 36 weeks 6 days)" <|
            \_ ->
                [ ( 28, 0 ), ( 36, 6 ) ]
                    |> List.map
                        (\ega ->
                            enterResultLater ega 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                                |> labResultsWarning
                        )
                    |> Expect.equal [ [], [] ]

        -- A subsequent visit: an earlier nurse visit four weeks back already
        -- diagnosed gestational hypertension.
        , test "subsequent visit after gestational hypertension was diagnosed: protein +1 entered later gives severe preeclampsia, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                let
                    previousVisit =
                        { startDate = Date.add Date.Weeks -4 visitDay
                        , diagnoses = EverySet.singleton DiagnosisGestationalHypertensionImmediate
                        , pastDiagnoses = EverySet.empty
                        , measurements = emptyPrenatalMeasurements |> withVitals 150 95
                        }
                in
                enterResultLaterAfter [ previousVisit ] ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> referredInLabResults
                    |> Expect.equal ( True, True )
        , test "guard: no severe sign in the physical exam (normal lungs and abdomen), protein +1 entered later: no severe preeclampsia" <|
            \_ ->
                awaitingProteinVisit ( 165, 115 ) (EverySet.singleton NormalLungs) (EverySet.singleton NormalAbdomen)
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                    |> severePreeclampsiaAfterEntry
                    |> Expect.equal []
        , test "guard: blood pressure 150/100 (below 160/110) with a severe sign, protein +1 entered later: no severe preeclampsia" <|
            \_ ->
                awaitingProteinVisit ( 150, 100 ) (EverySet.singleton NormalLungs) (EverySet.singleton TPRightUpper)
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                    |> severePreeclampsiaAfterEntry
                    |> Expect.equal []
        ]


lateProteinFrom37WeeksTest : Test
lateProteinFrom37WeeksTest =
    let
        warnedAndReferred result =
            ( not (List.isEmpty (severePreeclampsiaAmong (labResultsWarning result))), not (List.isEmpty (labResultsHospitalReferral result)) )
    in
    describe "37 weeks or later"
        [ test "The same at 37 weeks or later: saving the protein result takes the nurse to Lab Results' Next Steps with the emergency warning, and Next Steps asks to refer to hospital." <|
            -- 38 weeks, 165/115, right upper quadrant tenderness, protein +1
            -- entered in Lab Results.
            \_ ->
                enterResultLater ( 38, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> warnedAndReferred
                    |> Expect.equal ( True, True )
        , test "at exactly 37 weeks 0 days: emergency warning on saving the protein result, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                enterResultLater ( 37, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> warnedAndReferred
                    |> Expect.equal ( True, True )

        -- The warning's content is the one the nurse gets today when the
        -- protein is read at a 38-week visit: it names severe preeclampsia
        -- and gives the same referral instruction.
        , test "the Lab Results warning reads like today's warning for severe preeclampsia found at a visit at 38 weeks" <|
            \_ ->
                let
                    atVisit =
                        enterResultLater ( 38, 0 ) 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                            |> visitWarning
                            |> resolveWarningPopupContentForUrgentDiagnoses English
                in
                enterResultLater ( 38, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> labResultsWarning
                    |> resolveWarningPopupContentForUrgentDiagnoses English
                    |> Expect.equal atVisit
        , test "urine dipstick stored with no prerequisites at all, protein +1 entered later at 38 weeks: emergency warning, and Lab Results' Next Steps refers to hospital" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withSeverePreeclampsiaSigns
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 38, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> warnedAndReferred
                    |> Expect.equal ( True, True )
        ]


immediateProteinAtVisitTest : Test
immediateProteinAtVisitTest =
    describe "protein read at the visit"
        [ test "When the urine dipstick has an immediate result at the visit, severe preeclampsia is found at the visit, with the same referral and warning as today." <|
            -- Before 37 weeks today: the initial-phase diagnosis, a hospital
            -- referral in the visit's Next Steps, and no emergency warning.
            \_ ->
                let
                    result =
                        enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                in
                ( severePreeclampsiaAmong (EverySet.toList result.visitDayDiagnoses)
                , visitHospitalReferral result
                , visitWarning result
                )
                    |> Expect.equal
                        ( [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        , []
                        )
        , test "guard: immediate result at a visit at 38 weeks: the EGA 37+ severe preeclampsia, referred to hospital, with the emergency warning, as today" <|
            \_ ->
                let
                    result =
                        enterResultLater ( 38, 0 ) 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                in
                ( severePreeclampsiaAmong (EverySet.toList result.visitDayDiagnoses)
                , visitHospitalReferral result
                , severePreeclampsiaAmong (visitWarning result)
                )
                    |> Expect.equal
                        ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]
                        )

        -- Once found at the visit, a later unrelated result (a normal
        -- hemoglobin) must not raise severe preeclampsia a second time on Lab
        -- Results.
        , test "guard: severe preeclampsia found at the visit, a later normal hemoglobin adds nothing (28 and 38 weeks)" <|
            \_ ->
                [ ( 28, 0 ), ( 38, 0 ) ]
                    |> List.map
                        (\ega ->
                            enterResultLater ega 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                                |> addedByLateResult
                        )
                    |> Expect.equal [ [], [] ]
        ]


headacheBlurredVisionAtVisitTest : Test
headacheBlurredVisionAtVisitTest =
    let
        foundAtVisit result =
            severePreeclampsiaAmong (EverySet.toList result.visitDayDiagnoses)
    in
    describe "headache with blurred vision at the visit"
        [ test "Headache with blurred vision at the visit still gives severe preeclampsia at the visit, whatever the urine result." <|
            -- The urine dipstick read at the visit as negative, as +1, not
            -- done at all, and sent to the lab: each gives the initial-phase
            -- severe preeclampsia at the visit, as today.
            \_ ->
                [ withUrineProtein Protein0
                , withUrineProtein ProteinPlus1
                , identity
                , withUrineDipstickSentToLab
                ]
                    |> List.map
                        (\withUrine ->
                            headacheBlurredVisionVisit withUrine
                                |> enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12)
                                |> foundAtVisit
                        )
                    |> Expect.equal
                        [ [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhase ]
                        ]
        , test "guard: headache with blurred vision at a visit at 38 weeks gives the EGA 37+ severe preeclampsia with the emergency warning at the visit" <|
            \_ ->
                headacheBlurredVisionVisit withUrineDipstickSentToLab
                    |> enterResultLater ( 38, 0 ) 3 (enterHemoglobin 12)
                    |> visitWarning
                    |> severePreeclampsiaAmong
                    |> Expect.equal [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]

        -- Inferred: severe preeclampsia was already found, referred and (from
        -- 37 weeks) warned about at the visit, so a later protein result keeps
        -- it and adds no second severe preeclampsia on Lab Results.
        , test "headache with blurred vision at the visit, then a later protein result of negative or +1: severe preeclampsia stays, and no severe preeclampsia is added on Lab Results (28 and 38 weeks)" <|
            \_ ->
                [ ( ( 28, 0 ), Protein0 ), ( ( 28, 0 ), ProteinPlus1 ), ( ( 38, 0 ), Protein0 ), ( ( 38, 0 ), ProteinPlus1 ) ]
                    |> List.map
                        (\( ega, protein ) ->
                            let
                                result =
                                    headacheBlurredVisionVisit withUrineDipstickSentToLab
                                        |> enterResultLater ega 3 (enterProteinInLabResults protein)
                            in
                            ( not (List.isEmpty (severePreeclampsiaAfterEntry result))
                            , severePreeclampsiaAmong (addedByLateResult result)
                            )
                        )
                    |> Expect.equal [ ( True, [] ), ( True, [] ), ( True, [] ), ( True, [] ) ]
        ]


lateProteinTraceOrNegativeTest : Test
lateProteinTraceOrNegativeTest =
    describe "later protein result of trace or negative"
        [ test "A later urine protein result of trace or negative does not give severe preeclampsia." <|
            -- The protein scale has no separate trace value; "0" is the
            -- lowest result. Before 37 weeks and from 37 weeks.
            \_ ->
                [ ( 28, 0 ), ( 38, 0 ) ]
                    |> List.map
                        (\ega ->
                            enterResultLater ega 3 (enterProteinInLabResults Protein0) severeSignsAwaitingProteinVisit
                                |> severePreeclampsiaAfterEntry
                        )
                    |> Expect.equal [ [], [] ]
        , test "guard: a later negative protein result raises no severe preeclampsia warning and no severe preeclampsia hospital referral in Lab Results (28 and 38 weeks)" <|
            \_ ->
                [ ( 28, 0 ), ( 38, 0 ) ]
                    |> List.map
                        (\ega ->
                            let
                                result =
                                    enterResultLater ega 3 (enterProteinInLabResults Protein0) severeSignsAwaitingProteinVisit
                            in
                            ( severePreeclampsiaAmong (labResultsWarning result), labResultsHospitalReferral result )
                        )
                    |> Expect.equal [ ( [], [] ), ( [], [] ) ]

        -- The "no answer" shape: another result is entered while the protein
        -- is still pending.
        , test "guard: a normal hemoglobin entered while the protein is still pending gives no severe preeclampsia (28 and 38 weeks)" <|
            \_ ->
                [ ( 28, 0 ), ( 38, 0 ) ]
                    |> List.map
                        (\ega ->
                            severeSignsAwaitingProteinVisit
                                |> withPendingHemoglobin
                                |> enterResultLater ega 3 (enterHemoglobin 12)
                                |> severePreeclampsiaAfterEntry
                        )
                    |> Expect.equal [ [], [] ]
        ]


{-| Only the nurse's half of the criterion is reachable here: the lab
technician's device runs the same reassessment, and stores the same diagnoses
on the encounter. Whether the technician's screen shows a warning is decided
in `Backend.Update` (the `isLabTech` check), which tests cannot import.
-}
lateProteinByLabTechTest : Test
lateProteinByLabTechTest =
    describe "protein result entered by a lab technician"
        [ test "When a lab technician enters the protein result, there is no warning on the technician's screen. The nurse finds the hospital referral in Lab Results' Next Steps." <|
            \_ ->
                enterResultLater ( 28, 0 ) 3 (enterProteinByLabTech ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> labResultsHospitalReferral
                    |> List.isEmpty
                    |> Expect.equal False
        , test "lab technician enters protein +1 for a visit at 38 weeks: the nurse finds the hospital referral in Lab Results' Next Steps" <|
            \_ ->
                enterResultLater ( 38, 0 ) 3 (enterProteinByLabTech ProteinPlus1) severeSignsAwaitingProteinVisit
                    |> labResultsHospitalReferral
                    |> List.isEmpty
                    |> Expect.equal False
        ]


{-| The progress report lists the encounter's stored diagnoses that are in
`obstetricalDiagnoses`, labelled with `PrenatalDiagnosisForProgressReport`.
Today's label is "Severe Preeclampsia" (Kinyarwanda "Preklampusi Ikabije").
-}
severePreeclampsiaProgressReportTest : Test
severePreeclampsiaProgressReportTest =
    describe "progress report"
        [ test "The progress report shows severe preeclampsia in all these cases, with the same wording as today." <|
            -- Late protein before and from 37 weeks, protein read at the
            -- visit before and from 37 weeks, headache with blurred vision,
            -- and protein entered by a lab technician.
            \_ ->
                [ enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                , enterResultLater ( 38, 0 ) 3 (enterProteinInLabResults ProteinPlus1) severeSignsAwaitingProteinVisit
                , enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                , enterResultLater ( 38, 0 ) 3 (enterHemoglobin 12) severePreeclampsiaAtVisit
                , enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) (headacheBlurredVisionVisit withUrineDipstickSentToLab)
                , enterResultLater ( 28, 0 ) 3 (enterProteinByLabTech ProteinPlus1) severeSignsAwaitingProteinVisit
                ]
                    |> List.map
                        (severePreeclampsiaAfterEntry
                            >> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                            >> List.map progressReportWording
                        )
                    |> Expect.equal
                        (List.repeat 6 [ ( "Severe Preeclampsia", "Preklampusi Ikabije" ) ])
        ]



-- PREECLAMPSIA RISK FOUND AFTER THE VISIT: ASPIRIN
--
-- High risk of preeclampsia is diagnosed once per pregnancy, and Aspirin is
-- offered in the Next Steps of the phase where it is diagnosed. Hypertension
-- or diabetes found during the encounter gives the risk from week 12.
--
-- Two of those findings arrive after the visit's own activities are done:
--   * the blood pressure recheck: a borderline reading (140-159 / 90-109) at
--     the visit asks for a second reading, recorded in the encounter's
--     Examination after the visit is finished; when it is borderline again,
--     it is hypertension (chronic before week 20, gestational from week 20);
--   * a blood sugar test sent to the lab (or urine glucose, when no blood
--     sugar test was done), whose result is entered later in Lab Results.
--
-- Every save reassesses the whole encounter with
-- `generatePrenatalDiagnosesForNurse` and stores the outcome. The Next Steps
-- that follow the recheck or the result ask for the medications that
-- `resolveRequiredMedicationsSet English _ PrenatalEncounterPhaseRecurrent`
-- returns on the encounter carrying the stored outcome (`result.saved`); the
-- visit's own Next Steps ask for what `PrenatalEncounterPhaseInitial` returns
-- on the encounter carrying the visit-day set (`result.visit`).
--
-- Each scenario reuses `enterResultLater`: the visit measurements are assessed
-- on the visit day, the recheck or result is entered, the encounter (closed on
-- the visit day) is assessed again. A recheck is entered on the visit day.
--
-- Oracle: issue #2508, "Expected behaviour". A test named after a criterion
-- carries the criterion's words; the others are boundary cases around it.


{-| The two high risk of preeclampsia diagnoses the app knows today.
-}
highRiskOfPreeclampsiaVariants : List PrenatalDiagnosis
highRiskOfPreeclampsiaVariants =
    [ DiagnosisHighRiskOfPreeclampsiaInitialPhase
    , DiagnosisHighRiskOfPreeclampsiaRecurrentPhase
    ]


highRiskOfPreeclampsiaIn : EverySet PrenatalDiagnosis -> Bool
highRiskOfPreeclampsiaIn diagnoses =
    List.any (\diagnosis -> EverySet.member diagnosis diagnoses) highRiskOfPreeclampsiaVariants


{-| Any preeclampsia risk (high or moderate) in a diagnosis set.
-}
preeclampsiaRiskIn : EverySet PrenatalDiagnosis -> Bool
preeclampsiaRiskIn diagnoses =
    List.any (\diagnosis -> EverySet.member diagnosis diagnoses) preeclampsiaRiskDiagnoses


{-| Whether the Next Steps of the given phase offer Aspirin.
-}
aspirinOffered : PrenatalEncounterPhase -> AssembledData -> Bool
aspirinOffered phase assembled =
    resolveRequiredMedicationsSet English visitDay phase assembled
        |> List.any (\( _, medications, _ ) -> List.member Aspirin medications)


{-| Aspirin in the visit's own Next Steps, read with the visit-day diagnoses.
-}
aspirinAtVisit : LateResult -> Bool
aspirinAtVisit result =
    aspirinOffered PrenatalEncounterPhaseInitial result.visit


{-| Aspirin in the Next Steps that follow the recheck or Lab Results, read
with the diagnoses stored after the reassessment.
-}
aspirinAfterRecheckOrResult : LateResult -> Bool
aspirinAfterRecheckOrResult result =
    aspirinOffered PrenatalEncounterPhaseRecurrent result.saved


{-| Vitals holding the visit's blood pressure (sys, dia) and, once taken, the
recheck's.
-}
withBloodPressures : ( Float, Float ) -> Maybe ( Float, Float ) -> PrenatalMeasurements -> PrenatalMeasurements
withBloodPressures ( sys, dia ) recheck measurements =
    let
        visitReading =
            TestFixtures.vitalsValueWith sys dia
    in
    { measurements
        | vitals =
            TestFixtures.wrapMeasurement visitDay
                { visitReading
                    | sysRepeated = Maybe.map Tuple.first recheck
                    , diaRepeated = Maybe.map Tuple.second recheck
                }
    }


{-| A visit at `ega` with the borderline blood pressure `visitReading` (and
the other given measurements); after the visit, the recheck reads
`recheckReading`.
-}
recheckReadsAt : ( Int, Int ) -> ( Float, Float ) -> ( Float, Float ) -> PrenatalMeasurements -> LateResult
recheckReadsAt ega visitReading recheckReading visitMeasurements =
    enterResultLater ega
        0
        (withBloodPressures visitReading (Just recheckReading))
        (withBloodPressures visitReading Nothing visitMeasurements)


{-| The recheck confirms the visit's borderline reading.
-}
recheckConfirmsAt : ( Int, Int ) -> ( Float, Float ) -> PrenatalMeasurements -> LateResult
recheckConfirmsAt ega reading =
    recheckReadsAt ega reading reading


{-| The issue's reading: 145/95 at the visit and again at the recheck.
-}
recheckConfirms145Over95At : ( Int, Int ) -> LateResult
recheckConfirms145Over95At ega =
    recheckConfirmsAt ega ( 145, 95 ) emptyPrenatalMeasurements


{-| A visit at `ega` that sent a fasting blood sugar test to the lab; the
given count is entered later in Lab Results (above 126 is diabetes).
-}
bloodSugarResultLaterAt : ( Int, Int ) -> Float -> PrenatalMeasurements -> LateResult
bloodSugarResultLaterAt ega count visitMeasurements =
    enterResultLater ega 3 (enterRandomBloodSugar count) (withPendingRandomBloodSugar visitMeasurements)


{-| A blood sugar test that was not done, stored with the reason.
-}
randomBloodSugarNotDoneValue : RandomBloodSugarTestValue PrenatalEncounterId
randomBloodSugarNotDoneValue =
    { executionNote = TestNoteLackOfReagents
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , sugarCount = Nothing
    , originatingEncounter = Nothing
    }


withRandomBloodSugarNotDone : PrenatalMeasurements -> PrenatalMeasurements
withRandomBloodSugarNotDone measurements =
    { measurements | randomBloodSugarTest = TestFixtures.wrapMeasurement visitDay randomBloodSugarNotDoneValue }


{-| The nurse enters the urine glucose on the encounter's Lab Results page.
-}
enterUrineGlucoseInLabResults : GlucoseValue -> PrenatalMeasurements -> PrenatalMeasurements
enterUrineGlucoseInLabResults glucose measurements =
    { measurements
        | urineDipstickTest =
            TestFixtures.wrapMeasurement visitDay { urineDipstickSentToLabValue | glucose = Just glucose }
    }


{-| A visit at `ega` that sent the urine dipstick to the lab, with the blood
sugar test step given; the urine glucose is entered later in Lab Results.
-}
urineGlucoseResultLaterAt : ( Int, Int ) -> (PrenatalMeasurements -> PrenatalMeasurements) -> GlucoseValue -> LateResult
urineGlucoseResultLaterAt ega withBloodSugarStep glucose =
    emptyPrenatalMeasurements
        |> withBloodSugarStep
        |> withUrineDipstickSentToLab
        |> enterResultLater ega 3 (enterUrineGlucoseInLabResults glucose)


{-| A fasting blood sugar read at the visit (immediate result).
-}
withImmediateRandomBloodSugar : Float -> PrenatalMeasurements -> PrenatalMeasurements
withImmediateRandomBloodSugar count measurements =
    { measurements
        | randomBloodSugarTest =
            TestFixtures.wrapMeasurement visitDay
                { pendingRandomBloodSugarValue
                    | testPrerequisites = Just (EverySet.fromList [ PrerequisiteFastFor12h, PrerequisiteImmediateResult ])
                    , sugarCount = Just count
                }
    }


{-| An obstetrical exam finding twins.
-}
withTwins : PrenatalMeasurements -> PrenatalMeasurements
withTwins measurements =
    { measurements
        | obstetricalExam =
            wrapMeasurement
                { fundalPalpable = True
                , fundalHeight = Just (HeightInCm 30)
                , fetalPresentation = Twins
                , fetalMovement = True
                , fetalHeartRate = 140
                , cSectionScar = NoScar
                }
    }


{-| A medical history carrying the given sign.
-}
withMedicalHistorySign : MedicalHistorySign -> PrenatalMeasurements -> PrenatalMeasurements
withMedicalHistorySign sign measurements =
    { measurements
        | medicalHistory =
            wrapMeasurement
                { signs = EverySet.singleton sign
                , physicalConditions = EverySet.singleton NoMedicalHistoryPhysicalCondition
                , infectiousDiseases = EverySet.singleton NoMedicalHistoryInfectiousDisease
                , mentalHealthIssues = EverySet.singleton NoMedicalHistoryMentalHealthIssue
                , preeclampsiaInFamily = DoesNotOccur
                }
    }


{-| The histories that give high risk of preeclampsia at the visit, by name.
-}
highRiskHistories : List ( String, PrenatalMeasurements -> PrenatalMeasurements )
highRiskHistories =
    [ ( "preeclampsia in a previous pregnancy", withPreeclampsiaRiskHistory ObstetricHistoryPreeclampsiaPreviousPregnancy Neither )
    , ( "twins", withTwins )
    , ( "hypertension before pregnancy", withMedicalHistorySign HypertensionBeforePregnancy )
    , ( "diabetes before pregnancy", withMedicalHistorySign Diabetes )
    ]


{-| The names of the expected findings that are absent.
-}
missing : List ( String, Bool ) -> List String
missing =
    List.filter (Tuple.second >> not) >> List.map Tuple.first


{-| The names of the findings that are present.
-}
present : List ( String, Bool ) -> List String
present =
    List.filter Tuple.second >> List.map Tuple.first


preeclampsiaRiskAfterTheVisitTest : Test
preeclampsiaRiskAfterTheVisitTest =
    describe "preeclampsia risk found at the recheck or from a later result: Aspirin"
        [ riskAtRecheckFrom20WeeksTest
        , riskAtRecheckFrom12To19WeeksTest
        , riskFromLaterResultTest
        , noRiskBefore12WeeksTest
        , riskAtVisitAsTodayTest
        , riskFromHistoryStaysAtVisitTest
        , preeclampsiaFoundAfterVisitTest
        , preeclampsiaRiskProgressReportTest
        ]


riskAtRecheckFrom20WeeksTest : Test
riskAtRecheckFrom20WeeksTest =
    let
        -- What the recheck should bring, each named, so a failure lists the
        -- missing ones.
        outcome result =
            [ ( "gestational hypertension", EverySet.member DiagnosisGestationalHypertensionAfterRecheck result.afterEntry )
            , ( "high risk of preeclampsia", highRiskOfPreeclampsiaIn result.afterEntry )
            , ( "Aspirin after the recheck", aspirinAfterRecheckOrResult result )
            , ( "hypertension treatment after the recheck", diagnosedHypertension PrenatalEncounterPhaseRecurrent result.saved )
            ]
    in
    describe "recheck from week 20"
        [ test "At 20 weeks or later, a borderline blood pressure (140–159 / 90–109) confirmed at the recheck gives gestational hypertension and high risk of preeclampsia. The Next Steps after the recheck offer Aspirin, alongside the hypertension treatment." <|
            -- The issue's scenario: 145/95 at a visit at 28 weeks, and again
            -- at the recheck.
            \_ ->
                recheckConfirms145Over95At ( 28, 0 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "recheck at 20 weeks 0 days: gestational hypertension, high risk, Aspirin after the recheck" <|
            \_ ->
                recheckConfirms145Over95At ( 20, 0 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "recheck at 38 weeks: gestational hypertension, high risk, Aspirin after the recheck" <|
            \_ ->
                recheckConfirms145Over95At ( 38, 0 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "lowest borderline systolic (140/85 at visit and recheck) at 28 weeks: Aspirin after the recheck" <|
            \_ ->
                recheckConfirmsAt ( 28, 0 ) ( 140, 85 ) emptyPrenatalMeasurements
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "lowest borderline diastolic (130/90 at visit and recheck) at 28 weeks: Aspirin after the recheck" <|
            \_ ->
                recheckConfirmsAt ( 28, 0 ) ( 130, 90 ) emptyPrenatalMeasurements
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "highest borderline reading (159/109 at visit and recheck) at 28 weeks: Aspirin after the recheck" <|
            \_ ->
                recheckConfirmsAt ( 28, 0 ) ( 159, 109 ) emptyPrenatalMeasurements
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "the borderline visit reading alone gives no risk and no Aspirin at the visit: the recheck is still to come" <|
            \_ ->
                let
                    result =
                        recheckConfirms145Over95At ( 28, 0 )
                in
                ( preeclampsiaRiskIn result.visitDayDiagnoses, aspirinAtVisit result )
                    |> Expect.equal ( False, False )
        , test "guard: a normal recheck (135/85) after 145/95 at 28 weeks gives no hypertension, no risk and no Aspirin" <|
            \_ ->
                let
                    result =
                        recheckReadsAt ( 28, 0 ) ( 145, 95 ) ( 135, 85 ) emptyPrenatalMeasurements
                in
                ( present (outcome result), preeclampsiaRiskIn result.afterEntry )
                    |> Expect.equal ( [], False )
        ]


riskAtRecheckFrom12To19WeeksTest : Test
riskAtRecheckFrom12To19WeeksTest =
    let
        -- What the recheck should bring, each named, so a failure lists the
        -- missing ones.
        outcome result =
            [ ( "chronic hypertension", EverySet.member DiagnosisChronicHypertensionAfterRecheck result.afterEntry )
            , ( "high risk of preeclampsia", highRiskOfPreeclampsiaIn result.afterEntry )
            , ( "Aspirin after the recheck", aspirinAfterRecheckOrResult result )
            , ( "hypertension treatment after the recheck", diagnosedHypertension PrenatalEncounterPhaseRecurrent result.saved )
            ]
    in
    describe "recheck between weeks 12 and 19"
        [ test "Between 12 and 19 weeks, the same recheck gives chronic hypertension and high risk of preeclampsia, and Aspirin is offered in the same place." <|
            \_ ->
                recheckConfirms145Over95At ( 16, 0 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "recheck at 12 weeks 0 days: chronic hypertension, high risk, Aspirin after the recheck" <|
            \_ ->
                recheckConfirms145Over95At ( 12, 0 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        , test "recheck at 19 weeks 6 days: chronic hypertension, high risk, Aspirin after the recheck" <|
            \_ ->
                recheckConfirms145Over95At ( 19, 6 )
                    |> outcome
                    |> missing
                    |> Expect.equal []
        ]


riskFromLaterResultTest : Test
riskFromLaterResultTest =
    let
        -- (high risk of preeclampsia, Aspirin in Lab Results' Next Steps)
        outcome result =
            ( highRiskOfPreeclampsiaIn result.afterEntry
            , aspirinAfterRecheckOrResult result
            )
    in
    describe "later blood sugar or urine glucose result"
        [ test "At 12 weeks or later, a blood sugar test sent for later results whose result, entered in Lab Results, shows diabetes gives diabetes and high risk of preeclampsia. Lab Results' Next Steps offer Aspirin." <|
            -- Visit at 28 weeks; fasting blood sugar 130 entered later.
            \_ ->
                let
                    result =
                        bloodSugarResultLaterAt ( 28, 0 ) 130 emptyPrenatalMeasurements
                in
                ( EverySet.member DiagnosisGestationalDiabetesRecurrentPhase result.afterEntry
                , outcome result
                )
                    |> Expect.equal ( True, ( True, True ) )
        , test "The same applies to urine glucose +2 or more when no blood sugar test was done." <|
            -- Visit at 28 weeks; the blood sugar test was not done (lack of
            -- reagents); the urine dipstick went to the lab and urine glucose
            -- +2 is entered later.
            \_ ->
                let
                    result =
                        urineGlucoseResultLaterAt ( 28, 0 ) withRandomBloodSugarNotDone GlucosePlus2
                in
                ( EverySet.member DiagnosisGestationalDiabetesRecurrentPhase result.afterEntry
                , outcome result
                )
                    |> Expect.equal ( True, ( True, True ) )
        , test "Lab Results' Next Steps show the medication task for the Aspirin a later blood sugar result calls for" <|
            \_ ->
                bloodSugarResultLaterAt ( 28, 0 ) 130 emptyPrenatalMeasurements
                    |> .saved
                    |> Pages.Prenatal.RecurrentActivity.Utils.resolveNextStepsTasks visitDay False
                    |> List.member Pages.Prenatal.RecurrentActivity.Types.NextStepsMedicationDistribution
                    |> Expect.equal True
        , test "blood sugar from a visit at 14 weeks, entered later: diabetes, high risk, Aspirin in Lab Results' Next Steps" <|
            \_ ->
                let
                    result =
                        bloodSugarResultLaterAt ( 14, 0 ) 130 emptyPrenatalMeasurements
                in
                ( EverySet.member DiagnosisDiabetesRecurrentPhase result.afterEntry
                , outcome result
                )
                    |> Expect.equal ( True, ( True, True ) )
        , test "blood sugar from a visit at 12 weeks 0 days, entered later: high risk, Aspirin in Lab Results' Next Steps" <|
            \_ ->
                bloodSugarResultLaterAt ( 12, 0 ) 130 emptyPrenatalMeasurements
                    |> outcome
                    |> Expect.equal ( True, True )
        , test "urine glucose +4 entered later at 28 weeks, no blood sugar test done: high risk, Aspirin in Lab Results' Next Steps" <|
            \_ ->
                urineGlucoseResultLaterAt ( 28, 0 ) withRandomBloodSugarNotDone GlucosePlus4
                    |> outcome
                    |> Expect.equal ( True, True )
        , test "guard: urine glucose +1 entered later, no blood sugar test done: no diabetes, no risk, no Aspirin" <|
            \_ ->
                let
                    result =
                        urineGlucoseResultLaterAt ( 28, 0 ) withRandomBloodSugarNotDone GlucosePlus1
                in
                ( EverySet.member DiagnosisGestationalDiabetesRecurrentPhase result.afterEntry
                , outcome result
                )
                    |> Expect.equal ( False, ( False, False ) )
        , test "guard: urine glucose +2 entered later when the blood sugar test was done and normal: the blood sugar decides, no risk, no Aspirin" <|
            \_ ->
                urineGlucoseResultLaterAt ( 28, 0 ) (enterRandomBloodSugar 90) GlucosePlus2
                    |> outcome
                    |> Expect.equal ( False, False )
        , test "guard: a normal blood sugar entered later at 28 weeks: no risk, no Aspirin" <|
            \_ ->
                bloodSugarResultLaterAt ( 28, 0 ) 90 emptyPrenatalMeasurements
                    |> outcome
                    |> Expect.equal ( False, False )
        ]


noRiskBefore12WeeksTest : Test
noRiskBefore12WeeksTest =
    let
        -- (any preeclampsia risk, Aspirin after the recheck or result)
        outcome result =
            ( preeclampsiaRiskIn result.afterEntry
            , aspirinAfterRecheckOrResult result
            )
    in
    describe "before week 12"
        [ test "Before 12 weeks, neither the recheck nor a later result gives a preeclampsia risk." <|
            -- Recheck at 11 weeks 6 days; blood sugar and urine glucose from
            -- a visit at 11 weeks 6 days, entered later.
            \_ ->
                [ recheckConfirms145Over95At ( 11, 6 )
                , bloodSugarResultLaterAt ( 11, 6 ) 130 emptyPrenatalMeasurements
                , urineGlucoseResultLaterAt ( 11, 6 ) withRandomBloodSugarNotDone GlucosePlus2
                ]
                    |> List.map outcome
                    |> Expect.equal (List.repeat 3 ( False, False ))
        , test "guard: recheck at 11 weeks 6 days still gives chronic hypertension" <|
            \_ ->
                recheckConfirms145Over95At ( 11, 6 )
                    |> .afterEntry
                    |> EverySet.member DiagnosisChronicHypertensionAfterRecheck
                    |> Expect.equal True
        , test "guard: blood sugar from a visit at 8 weeks, entered later, still gives diabetes" <|
            \_ ->
                let
                    result =
                        bloodSugarResultLaterAt ( 8, 0 ) 130 emptyPrenatalMeasurements
                in
                ( EverySet.member DiagnosisDiabetesRecurrentPhase result.afterEntry, outcome result )
                    |> Expect.equal ( True, ( False, False ) )
        ]


riskAtVisitAsTodayTest : Test
riskAtVisitAsTodayTest =
    let
        -- (high risk among the visit-day diagnoses, Aspirin at the visit)
        atVisit result =
            ( highRiskOfPreeclampsiaIn result.visitDayDiagnoses
            , aspirinAtVisit result
            )
    in
    describe "risk found at the visit"
        [ test "A blood pressure of 160/110 or more at the visit, or a blood sugar result read at the visit that shows diabetes, still gives high risk of preeclampsia at the visit, with Aspirin in the visit's Next Steps, as today." <|
            -- 165/115 at 28 weeks and at 16 weeks; fasting blood sugar 130
            -- read at the visit at 28 weeks. A hemoglobin test goes to the
            -- lab so a result can be entered later.
            \_ ->
                [ enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                , enterResultLater ( 16, 0 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                , enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) (withImmediateRandomBloodSugar 130 (withPendingHemoglobin emptyPrenatalMeasurements))
                ]
                    |> List.map atVisit
                    |> Expect.equal (List.repeat 3 ( True, True ))
        , test "160/110 at the visit at 28 weeks, a later hemoglobin result: Lab Results' Next Steps do not offer Aspirin a second time" <|
            \_ ->
                enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                    |> aspirinAfterRecheckOrResult
                    |> Expect.equal False
        , test "blood sugar read at the visit at 28 weeks shows diabetes, a later hemoglobin result: Lab Results' Next Steps do not offer Aspirin a second time" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withPendingHemoglobin
                    |> withImmediateRandomBloodSugar 130
                    |> enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12)
                    |> aspirinAfterRecheckOrResult
                    |> Expect.equal False
        ]


riskFromHistoryStaysAtVisitTest : Test
riskFromHistoryStaysAtVisitTest =
    let
        -- Each history at a visit at 28 weeks, followed by a borderline
        -- reading confirmed at the recheck, or by a blood sugar result
        -- showing diabetes entered later.
        scenarios =
            List.concatMap
                (\( name, withHistory ) ->
                    [ ( name ++ ", then the recheck", recheckConfirmsAt ( 28, 0 ) ( 145, 95 ) (withHistory emptyPrenatalMeasurements) )
                    , ( name ++ ", then a later blood sugar result", bloodSugarResultLaterAt ( 28, 0 ) 130 (withHistory emptyPrenatalMeasurements) )
                    ]
                )
                highRiskHistories

        firstVisitWith diagnosis =
            { startDate = Date.add Date.Weeks -8 visitDay
            , diagnoses = EverySet.singleton diagnosis
            , pastDiagnoses = EverySet.empty
            , measurements = emptyPrenatalMeasurements
            }

        recheckAfterRiskAtFirstVisit diagnosis =
            emptyPrenatalMeasurements
                |> withBloodPressures ( 145, 95 ) Nothing
                |> enterResultLaterAfter [ firstVisitWith diagnosis ] ( 28, 0 ) 0 (withBloodPressures ( 145, 95 ) (Just ( 145, 95 )))
    in
    describe "risk found at the visit from history"
        [ test "A risk found at the visit from history (preeclampsia in a previous pregnancy, twins, hypertension or diabetes before pregnancy) stays a visit diagnosis. A recheck or a later result on the same day does not offer Aspirin a second time." <|
            \_ ->
                scenarios
                    |> List.map (\( name, result ) -> ( name, aspirinAtVisit result, aspirinAfterRecheckOrResult result ))
                    |> Expect.equal (List.map (\( name, _ ) -> ( name, True, False )) scenarios)
        , test "the risk from history is still the visit's diagnosis after the recheck or the later result" <|
            \_ ->
                scenarios
                    |> List.map
                        (\( name, result ) ->
                            ( name
                            , EverySet.member DiagnosisHighRiskOfPreeclampsiaInitialPhase result.afterEntry
                            , EverySet.member DiagnosisHighRiskOfPreeclampsiaRecurrentPhase result.afterEntry
                            )
                        )
                    |> Expect.equal (List.map (\( name, _ ) -> ( name, True, False )) scenarios)
        , test "guard: high risk found at an earlier visit: this visit's recheck gives no risk again and no Aspirin" <|
            -- The earlier visit may hold either high risk diagnosis.
            \_ ->
                highRiskOfPreeclampsiaVariants
                    |> List.map
                        (recheckAfterRiskAtFirstVisit
                            >> (\result -> ( highRiskOfPreeclampsiaIn result.afterEntry, aspirinAfterRecheckOrResult result ))
                        )
                    |> Expect.equal [ ( False, False ), ( False, False ) ]
        ]


preeclampsiaFoundAfterVisitTest : Test
preeclampsiaFoundAfterVisitTest =
    let
        moderatePreeclampsiaVariants =
            [ DiagnosisModeratePreeclampsiaInitialPhase
            , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
            , DiagnosisModeratePreeclampsiaRecurrentPhase
            , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
            ]

        -- (moderate preeclampsia, any preeclampsia risk, Aspirin after the
        -- recheck or result)
        outcome result =
            ( List.any (\diagnosis -> EverySet.member diagnosis result.afterEntry) moderatePreeclampsiaVariants
            , preeclampsiaRiskIn result.afterEntry
            , aspirinAfterRecheckOrResult result
            )
    in
    describe "preeclampsia found at the recheck or from the later result"
        [ test "When preeclampsia is found at the recheck or from the later result, no preeclampsia risk is given, as today." <|
            -- At 28 weeks, 145/95 at the visit:
            --   * urine protein +1 read at the visit, the recheck confirms;
            --   * urine protein +1 entered in Lab Results first, the recheck
            --     confirms afterwards;
            --   * the recheck confirms first (its reassessment stored), then
            --     urine protein +1 is entered in Lab Results.
            \_ ->
                [ recheckConfirmsAt ( 28, 0 ) ( 145, 95 ) (withUrineProtein ProteinPlus1 emptyPrenatalMeasurements)
                , recheckConfirmsAt ( 28, 0 ) ( 145, 95 ) (enterProteinInLabResults ProteinPlus1 (withUrineDipstickSentToLab emptyPrenatalMeasurements))
                , emptyPrenatalMeasurements
                    |> withBloodPressures ( 145, 95 ) (Just ( 145, 95 ))
                    |> withUrineDipstickSentToLab
                    |> enterResultLater ( 28, 0 ) 3 (enterProteinInLabResults ProteinPlus1)
                ]
                    |> List.map outcome
                    |> Expect.equal (List.repeat 3 ( True, False, False ))
        ]


{-| The progress report lists the encounter's diagnoses found in
`obstetricalDiagnoses`, labelled with `PrenatalDiagnosisForProgressReport`.
Today's label is "High Risk of Preeclampsia" (Kinyarwanda "Afite ibyago
byinshi byo kugira Preklampusi"). The wording is matched, not the variant, so
the test reads the label whichever diagnosis carries it.
-}
preeclampsiaRiskProgressReportTest : Test
preeclampsiaRiskProgressReportTest =
    let
        label =
            ( "High Risk of Preeclampsia", "Afite ibyago byinshi byo kugira Preklampusi" )

        labelsShown result =
            EverySet.toList result.afterEntry
                |> List.filter (\diagnosis -> List.member diagnosis obstetricalDiagnoses)
                |> List.map progressReportWording
                |> List.filter ((==) label)
    in
    describe "progress report"
        [ test "The progress report shows \"High Risk of Preeclampsia\" with the same wording in all these cases." <|
            -- Recheck at 28 and at 16 weeks; blood sugar and urine glucose
            -- entered later; 160/110 and blood sugar read at the visit;
            -- history at the visit followed by the recheck.
            \_ ->
                [ recheckConfirms145Over95At ( 28, 0 )
                , recheckConfirms145Over95At ( 16, 0 )
                , bloodSugarResultLaterAt ( 28, 0 ) 130 emptyPrenatalMeasurements
                , urineGlucoseResultLaterAt ( 28, 0 ) withRandomBloodSugarNotDone GlucosePlus2
                , enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) highBloodPressureVisit
                , enterResultLater ( 28, 0 ) 3 (enterHemoglobin 12) (withImmediateRandomBloodSugar 130 (withPendingHemoglobin emptyPrenatalMeasurements))
                , recheckConfirmsAt ( 28, 0 ) ( 145, 95 ) (withTwins emptyPrenatalMeasurements)
                ]
                    |> List.map labelsShown
                    |> Expect.equal (List.repeat 7 [ label ])
        ]



-- ULTRASOUND FORM
--
-- The Estimated Delivery Date (EDD) is the ultrasound day plus what remains of
-- a 280-day pregnancy, given how far along the ultrasound found it (weeks and
-- days). That formula is the independent oracle used below.
--
-- The nurse can fill the date, the weeks and the days in any order, and can
-- set the date two ways: tap a day in the calendar and then Close, or open the
-- calendar and Close it without tapping a day, which fills in today (the
-- calendar's default day when no date is set yet).
--
-- Save is enabled when every task of the form is completed. What Save stores
-- is `toUltrasoundValueWithDefault`; when it stores something the activity is
-- left, and the stored EDD becomes the pregnancy's EDD.
--
-- With an ultrasound already saved, the form held by the page starts empty and
-- the screen shows it merged with the saved ultrasound. Each nurse action is
-- applied to the form held by the page, which is how the page's update applies
-- them; the screen and Save then merge it with the saved ultrasound.


{-| The form as it is before the nurse touches it.
-}
emptyUltrasoundFormForTest : UltrasoundForm
emptyUltrasoundFormForTest =
    emptyModel.ultrasoundData.form


ultrasoundFormTest : Test
ultrasoundFormTest =
    let
        today =
            Date.fromCalendarDate 2026 Time.Nov 3

        -- A day picked in the calendar, two weeks before today.
        earlierDay =
            Date.fromCalendarDate 2026 Time.Oct 20

        -- Another day picked later, to replace the first pick.
        otherDay =
            Date.fromCalendarDate 2026 Time.Oct 1

        expectedEdd executionDate weeks days =
            Date.add Date.Days (280 - (weeks * 7 + days)) executionDate

        -- The calendar as the date field opens it: the day it starts on is
        -- the date already set, or today when none is set.
        openCalendar startDay =
            setUltrasoundExecutionDateSelectorState
                (Just
                    { select = always NoOp
                    , close = NoOp
                    , dateFrom = Date.add Date.Months -9 today
                    , dateTo = today
                    , dateDefault = Just startDay
                    }
                )

        closeCalendar =
            setUltrasoundExecutionDateSelectorState Nothing

        -- Open the calendar, tap a day, then Close.
        pickDay startDay day =
            openCalendar startDay >> setUltrasoundExecutionDate day >> closeCalendar

        -- Open the calendar and Close it without tapping a day.
        closeWithoutPicking startDay =
            openCalendar startDay >> closeCalendar

        answerSigns form =
            { form
                | pregnancyNotViable = Just False
                , pregnancyEctopic = Just False
                , pregnancyMultipleFetuses = Just False
            }

        noSigns =
            EverySet.singleton NoPregnancySigns

        apply steps form =
            List.foldl (\step acc -> step acc) form steps

        -- Nurse actions on a form with nothing saved, after the three
        -- pregnancy-sign questions are answered.
        fill steps =
            apply steps (answerSigns emptyUltrasoundFormForTest)

        saveEnabled shown =
            List.all ((/=) Nothing) (ultrasoundTasks shown)

        completedTasks shown =
            ultrasoundTasks shown
                |> List.filterMap identity
                |> List.length

        stored =
            toUltrasoundValueWithDefault Nothing

        -- The ultrasound recorded earlier: done 1 September at 10 weeks.
        savedDate =
            Date.fromCalendarDate 2026 Time.Sep 1

        saved =
            { signs = noSigns
            , executionDate = savedDate
            , eddWeeks = 10
            , eddDays = 0
            , eddDate = expectedEdd savedDate 10 0
            }

        -- Nurse actions on the form when an ultrasound is already saved.
        editSaved steps =
            apply steps emptyUltrasoundFormForTest

        shownWithSaved form =
            ultrasoundFormWithDefault form (Just saved)

        storedWithSaved =
            toUltrasoundValueWithDefault (Just saved)

        weeks12 =
            setUltrasoundEDDWeeks "12"

        days3 =
            setUltrasoundEDDDays "3"

        -- The three ways of filling the date, weeks and days in each order,
        -- for each way of setting the date, with the day it sets.
        orderings =
            [ ( "a day picked", pickDay today earlierDay, earlierDay )
            , ( "the calendar closed without picking", closeWithoutPicking today, today )
            ]
                |> List.concatMap
                    (\( dateName, dateStep, date ) ->
                        [ ( [ dateName, "weeks", "days" ], [ dateStep, weeks12, days3 ] )
                        , ( [ dateName, "days", "weeks" ], [ dateStep, days3, weeks12 ] )
                        , ( [ "weeks", dateName, "days" ], [ weeks12, dateStep, days3 ] )
                        , ( [ "weeks", "days", dateName ], [ weeks12, days3, dateStep ] )
                        , ( [ "days", dateName, "weeks" ], [ days3, dateStep, weeks12 ] )
                        , ( [ "days", "weeks", dateName ], [ days3, weeks12, dateStep ] )
                        ]
                            |> List.map
                                (\( names, steps ) ->
                                    { name = String.join ", then " names
                                    , steps = steps
                                    , date = date
                                    }
                                )
                    )

        -- Every way to a complete form, with what the screen and Save are
        -- expected to give. `shown` and `store` are how the screen and Save
        -- see the form: on their own, or merged with a saved ultrasound.
        completePaths =
            List.map
                (\ordering ->
                    { name = "nothing saved, " ++ ordering.name
                    , form = fill ordering.steps
                    , shown = identity
                    , store = stored
                    , expected =
                        { signs = noSigns
                        , executionDate = ordering.date
                        , eddWeeks = 12
                        , eddDays = 3
                        , eddDate = expectedEdd ordering.date 12 3
                        }
                    }
                )
                orderings
                ++ [ { name = "nothing saved, the issue's steps: signs, weeks, days, then the calendar closed without picking"
                     , form = fill [ weeks12, days3, closeWithoutPicking today ]
                     , shown = identity
                     , store = stored
                     , expected =
                        { signs = noSigns
                        , executionDate = today
                        , eddWeeks = 12
                        , eddDays = 3
                        , eddDate = expectedEdd today 12 3
                        }
                     }
                   , { name = "nothing saved, weeks, days, the calendar closed without picking, then the signs answered"
                     , form =
                        apply [ weeks12, days3, closeWithoutPicking today, answerSigns ]
                            emptyUltrasoundFormForTest
                     , shown = identity
                     , store = stored
                     , expected =
                        { signs = noSigns
                        , executionDate = today
                        , eddWeeks = 12
                        , eddDays = 3
                        , eddDate = expectedEdd today 12 3
                        }
                     }
                   , { name = "nothing saved, the issue's steps, then the calendar opened and closed again"
                     , form = fill [ weeks12, days3, closeWithoutPicking today, closeWithoutPicking today ]
                     , shown = identity
                     , store = stored
                     , expected =
                        { signs = noSigns
                        , executionDate = today
                        , eddWeeks = 12
                        , eddDays = 3
                        , eddDate = expectedEdd today 12 3
                        }
                     }
                   , { name = "an ultrasound saved, nothing changed"
                     , form = editSaved []
                     , shown = shownWithSaved
                     , store = storedWithSaved
                     , expected = saved
                     }
                   , { name = "an ultrasound saved, the calendar opened and closed without picking"
                     , form = editSaved [ closeWithoutPicking savedDate ]
                     , shown = shownWithSaved
                     , store = storedWithSaved
                     , expected = saved
                     }
                   , { name = "an ultrasound saved, weeks changed to 20"
                     , form = editSaved [ setUltrasoundEDDWeeks "20" ]
                     , shown = shownWithSaved
                     , store = storedWithSaved
                     , expected = { saved | eddWeeks = 20, eddDate = expectedEdd savedDate 20 0 }
                     }
                   , { name = "an ultrasound saved, days changed to 4"
                     , form = editSaved [ setUltrasoundEDDDays "4" ]
                     , shown = shownWithSaved
                     , store = storedWithSaved
                     , expected = { saved | eddDays = 4, eddDate = expectedEdd savedDate 10 4 }
                     }
                   , { name = "an ultrasound saved, a different day picked"
                     , form = editSaved [ pickDay savedDate earlierDay ]
                     , shown = shownWithSaved
                     , store = storedWithSaved
                     , expected = { saved | executionDate = earlierDay, eddDate = expectedEdd earlierDay 10 0 }
                     }
                   ]
    in
    describe "Prenatal Ultrasound form"
        [ describe "Filling the Ultrasound form in any order shows the Estimated Delivery Date as soon as the ultrasound date, weeks and days are all filled in"
            (List.map
                (\ordering ->
                    test ordering.name <|
                        \_ ->
                            -- No EDD before the last of the three is filled,
                            -- the right one straight after it.
                            ( ultrasoundEDDDate (fill (List.take 2 ordering.steps))
                            , ultrasoundEDDDate (fill ordering.steps)
                            )
                                |> Expect.equal ( Nothing, Just (expectedEdd ordering.date 12 3) )
                )
                orderings
            )
        , describe "Opening the \"Date Ultrasound was performed\" calendar and closing it without picking a day fills in today's date. If weeks and days are already filled in, the Estimated Delivery Date appears straight away."
            [ test "closing the calendar without picking a day fills in today's date" <|
                \_ ->
                    fill [ closeWithoutPicking today, weeks12, days3 ]
                        |> stored
                        |> Maybe.map .executionDate
                        |> Expect.equal (Just today)
            , test "on a form with only the signs answered, closing the calendar completes the date task" <|
                \_ ->
                    ( completedTasks (fill []), completedTasks (fill [ closeWithoutPicking today ]) )
                        |> Expect.equal ( 3, 4 )
            , test "weeks and days already filled in: the Estimated Delivery Date appears straight away" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd today 12 3))
            , test "weeks and days already filled in: Save stores today's date" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today ]
                        |> stored
                        |> Maybe.map .executionDate
                        |> Expect.equal (Just today)
            , test "only weeks filled in: no Estimated Delivery Date until days are filled in too" <|
                \_ ->
                    ( ultrasoundEDDDate (fill [ weeks12, closeWithoutPicking today ])
                    , ultrasoundEDDDate (fill [ weeks12, closeWithoutPicking today, days3 ])
                    )
                        |> Expect.equal ( Nothing, Just (expectedEdd today 12 3) )
            , test "only days filled in: no Estimated Delivery Date until weeks are filled in too" <|
                \_ ->
                    ( ultrasoundEDDDate (fill [ days3, closeWithoutPicking today ])
                    , ultrasoundEDDDate (fill [ days3, closeWithoutPicking today, weeks12 ])
                    )
                        |> Expect.equal ( Nothing, Just (expectedEdd today 12 3) )
            ]
        , describe "The Estimated Delivery Date shown after closing the calendar is the same as the one shown when today is picked in the calendar"
            [ test "weeks and days filled in before the date" <|
                \_ ->
                    let
                        closed =
                            fill [ weeks12, days3, closeWithoutPicking today ]

                        picked =
                            fill [ weeks12, days3, pickDay today today ]
                    in
                    ( ultrasoundEDDDate closed, stored closed )
                        |> Expect.all
                            [ Expect.equal ( ultrasoundEDDDate picked, stored picked )
                            , Tuple.first >> Expect.notEqual Nothing
                            ]
            , test "the date filled in before weeks and days" <|
                \_ ->
                    let
                        closed =
                            fill [ closeWithoutPicking today, weeks12, days3 ]

                        picked =
                            fill [ pickDay today today, weeks12, days3 ]
                    in
                    ( ultrasoundEDDDate closed, stored closed )
                        |> Expect.all
                            [ Expect.equal ( ultrasoundEDDDate picked, stored picked )
                            , Tuple.first >> Expect.notEqual Nothing
                            ]
            ]
        , describe "Picking a different day in the calendar later recalculates the Estimated Delivery Date for that day"
            [ test "after the calendar was closed without picking, once weeks and days were filled in" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today, pickDay today earlierDay ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 12 3))
            , test "after a day was picked before weeks and days" <|
                \_ ->
                    fill [ pickDay today today, weeks12, days3, pickDay today earlierDay ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 12 3))
            , test "a second pick replaces the first" <|
                \_ ->
                    fill [ pickDay today earlierDay, weeks12, days3, pickDay earlierDay otherDay ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd otherDay 12 3))
            , test "Save stores the later day and its Estimated Delivery Date" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today, pickDay today earlierDay ]
                        |> stored
                        |> Maybe.map (\value -> ( value.executionDate, value.eddDate ))
                        |> Expect.equal (Just ( earlierDay, expectedEdd earlierDay 12 3 ))
            , test "with an ultrasound saved, the EDD is recalculated from the saved weeks and days" <|
                \_ ->
                    editSaved [ pickDay savedDate earlierDay ]
                        |> shownWithSaved
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 10 0))
            ]
        , describe "Opening and closing the calendar when a date is already set leaves the date and the Estimated Delivery Date unchanged"
            [ test "a day picked, then weeks and days, then the calendar opened and closed" <|
                \_ ->
                    fill [ pickDay today earlierDay, weeks12, days3, closeWithoutPicking earlierDay ]
                        |> stored
                        |> Maybe.map (\value -> ( value.executionDate, value.eddDate ))
                        |> Expect.equal (Just ( earlierDay, expectedEdd earlierDay 12 3 ))
            , test "weeks and days, then a day picked, then the calendar opened and closed" <|
                \_ ->
                    let
                        form =
                            fill [ weeks12, days3, pickDay today earlierDay, closeWithoutPicking earlierDay ]
                    in
                    ( ultrasoundEDDDate form, Maybe.map .executionDate (stored form) )
                        |> Expect.equal ( Just (expectedEdd earlierDay 12 3), Just earlierDay )
            , test "a day picked, the calendar opened and closed before weeks and days are filled in" <|
                \_ ->
                    fill [ pickDay today earlierDay, closeWithoutPicking earlierDay, weeks12, days3 ]
                        |> stored
                        |> Maybe.map (\value -> ( value.executionDate, value.eddDate ))
                        |> Expect.equal (Just ( earlierDay, expectedEdd earlierDay 12 3 ))
            , test "today filled in by closing the calendar, then opened and closed again" <|
                \_ ->
                    let
                        form =
                            fill [ weeks12, days3, closeWithoutPicking today, closeWithoutPicking today ]
                    in
                    ( ultrasoundEDDDate form, Maybe.map .executionDate (stored form) )
                        |> Expect.equal ( Just (expectedEdd today 12 3), Just today )
            , test "with an ultrasound saved, its date and EDD stay" <|
                \_ ->
                    let
                        form =
                            editSaved [ closeWithoutPicking savedDate ]
                    in
                    ( ultrasoundEDDDate (shownWithSaved form), storedWithSaved form )
                        |> Expect.equal ( Just saved.eddDate, Just saved )
            ]
        , describe "Changing weeks or days after the date is set still recalculates the Estimated Delivery Date"
            [ test "a day picked, then weeks changed" <|
                \_ ->
                    fill [ pickDay today earlierDay, weeks12, days3, setUltrasoundEDDWeeks "20" ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 20 3))
            , test "a day picked, then days changed" <|
                \_ ->
                    fill [ pickDay today earlierDay, weeks12, days3, setUltrasoundEDDDays "5" ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 12 5))
            , test "today filled in by closing the calendar after weeks and days, then weeks changed" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today, setUltrasoundEDDWeeks "20" ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd today 20 3))
            , test "today filled in by closing the calendar after weeks and days, then days changed" <|
                \_ ->
                    fill [ weeks12, days3, closeWithoutPicking today, setUltrasoundEDDDays "5" ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd today 12 5))
            , test "weeks cleared: the Estimated Delivery Date goes and Save is disabled" <|
                \_ ->
                    let
                        form =
                            fill [ pickDay today earlierDay, weeks12, days3, setUltrasoundEDDWeeks "" ]
                    in
                    ( ultrasoundEDDDate form, saveEnabled form )
                        |> Expect.equal ( Nothing, False )
            , test "weeks cleared and typed again: the Estimated Delivery Date comes back" <|
                \_ ->
                    fill [ pickDay today earlierDay, weeks12, days3, setUltrasoundEDDWeeks "", setUltrasoundEDDWeeks "20" ]
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd earlierDay 20 3))
            , test "with an ultrasound saved, weeks changed: recalculated from the saved date and days" <|
                \_ ->
                    editSaved [ setUltrasoundEDDWeeks "20" ]
                        |> shownWithSaved
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd savedDate 20 0))
            , test "with an ultrasound saved, days changed: recalculated from the saved date and weeks" <|
                \_ ->
                    editSaved [ setUltrasoundEDDDays "4" ]
                        |> shownWithSaved
                        |> ultrasoundEDDDate
                        |> Expect.equal (Just (expectedEdd savedDate 10 4))
            ]
        , describe "Whenever Save is enabled, tapping it saves the ultrasound and leaves the activity"
            (List.map
                (\path ->
                    test path.name <|
                        \_ ->
                            ( saveEnabled (path.shown path.form)
                            , path.store path.form /= Nothing
                            )
                                |> Expect.equal ( True, True )
                )
                completePaths
                ++ [ test "the signs not answered: Save is disabled" <|
                        \_ ->
                            apply [ weeks12, days3, closeWithoutPicking today ] emptyUltrasoundFormForTest
                                |> saveEnabled
                                |> Expect.equal False
                   , test "days not filled in: Save is disabled and stores nothing" <|
                        \_ ->
                            let
                                form =
                                    fill [ weeks12, closeWithoutPicking today ]
                            in
                            ( saveEnabled form, stored form )
                                |> Expect.equal ( False, Nothing )
                   , test "no date set: Save is disabled and stores nothing" <|
                        \_ ->
                            let
                                form =
                                    fill [ weeks12, days3 ]
                            in
                            ( saveEnabled form, stored form )
                                |> Expect.equal ( False, Nothing )
                   ]
            )
        , describe "The saved ultrasound carries the Estimated Delivery Date that was shown, and the pregnancy's EDD is updated to it, as it is today"
            (List.map
                (\path ->
                    test path.name <|
                        \_ ->
                            -- What the screen shows, and the whole ultrasound
                            -- Save stores, whose EDD the pregnancy takes.
                            ( ultrasoundEDDDate (path.shown path.form)
                            , path.store path.form
                            )
                                |> Expect.equal ( Just path.expected.eddDate, Just path.expected )
                )
                completePaths
            )
        ]



-- PREECLAMPSIA RISK NEXT TO PREECLAMPSIA AND ECLAMPSIA
--
-- Oracle (issue "Expected behaviour"): "high risk of preeclampsia" and
-- "moderate risk of preeclampsia" are not diagnosed beside preeclampsia found
-- at the same encounter, nor beside eclampsia. At 37 weeks or later the visit
-- grades preeclampsia as its emergency EGA 37+ variants; those, and eclampsia
-- at any week, must keep the risk diagnoses off just as the plain [20, 37)
-- preeclampsia diagnoses already do.
--
-- Each risk diagnosis is reached three ways, so a fix that covers one route
-- and not another shows up:
--   * high risk from high blood pressure (gestational hypertension at the
--     visit, or after the blood pressure recheck);
--   * high risk from history (preeclampsia at a previous pregnancy);
--   * moderate risk from history (more than 10 years since the last delivery),
--     which is only visible when nothing gives high risk, because high risk
--     replaces moderate risk.
--
-- "Found in Lab Results" is modelled the way the app runs it: the visit is
-- assessed on the visit day, the result (a urine protein entered after the
-- visit, or the repeated blood pressure) is then entered, and the encounter is
-- reassessed with the visit-day set stored on it (`enterResultLater`).
-- Visits are written as (weeks, days) of EGA on the visit day.


{-| Run the function under test on a first nurse visit at the given EGA.
-}
diagnoseVisitAt : ( Int, Int ) -> PrenatalMeasurements -> EverySet PrenatalDiagnosis
diagnoseVisitAt ega measurements =
    generatePrenatalDiagnosesForNurse (visitAssembled ega [] EverySet.empty measurements)


{-| Every preeclampsia diagnosis, in the order the tests list them.
-}
allPreeclampsiaDiagnoses : List PrenatalDiagnosis
allPreeclampsiaDiagnoses =
    [ DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaInitialPhase
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhase
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    ]


{-| The members of a diagnosis set out of the given list, in list order.
-}
presentOf : List PrenatalDiagnosis -> EverySet PrenatalDiagnosis -> List PrenatalDiagnosis
presentOf diagnoses set =
    List.filter (\diagnosis -> EverySet.member diagnosis set) diagnoses


{-| The preeclampsia diagnoses in a set, and its preeclampsia risk diagnoses.
-}
preeclampsiaAndRisks : EverySet PrenatalDiagnosis -> ( List PrenatalDiagnosis, List PrenatalDiagnosis )
preeclampsiaAndRisks set =
    ( presentOf allPreeclampsiaDiagnoses set, presentOf preeclampsiaRiskDiagnoses set )


{-| Whether a set holds any preeclampsia diagnosis, and its preeclampsia risk
diagnoses. Used where the preeclampsia variant is not what is under test.
-}
preeclampsiaFoundAndRisks : EverySet PrenatalDiagnosis -> ( Bool, List PrenatalDiagnosis )
preeclampsiaFoundAndRisks set =
    ( not <| List.isEmpty (presentOf allPreeclampsiaDiagnoses set), presentOf preeclampsiaRiskDiagnoses set )


{-| Preeclampsia at a previous pregnancy: high risk of preeclampsia.
-}
withHighRiskHistory : PrenatalMeasurements -> PrenatalMeasurements
withHighRiskHistory =
    withPreeclampsiaRiskHistory ObstetricHistoryPreeclampsiaPreviousPregnancy Neither


{-| More than 10 years since the last delivery: moderate risk of preeclampsia.
-}
withModerateRiskHistory : PrenatalMeasurements -> PrenatalMeasurements
withModerateRiskHistory =
    withPreeclampsiaRiskHistory NoObstetricHistoryStep2Sign MoreThan10Years


{-| Edema on the hands in the physical exam, with no severe preeclampsia sign.
-}
withHandsEdema : PrenatalMeasurements -> PrenatalMeasurements
withHandsEdema measurements =
    let
        exam =
            corePhysicalExamValueWith EverySet.empty EverySet.empty
    in
    { measurements | corePhysicalExam = wrapMeasurement { exam | hands = EverySet.singleton EdemaHands } }


{-| A marginal blood pressure at the visit (145/95): high enough to ask for the
recheck on the Lab Results page, not high enough for hypertension by itself.
-}
withMarginalVitals : PrenatalMeasurements -> PrenatalMeasurements
withMarginalVitals =
    withVitals 145 95


{-| The repeated blood pressure, entered on the Lab Results page after a
marginal visit reading.
-}
enterRecheck : Float -> Float -> PrenatalMeasurements -> PrenatalMeasurements
enterRecheck sysRepeated diaRepeated measurements =
    let
        marginal =
            TestFixtures.vitalsValueWith 145 95
    in
    { measurements
        | vitals =
            TestFixtures.wrapMeasurement visitDay
                { marginal | sysRepeated = Just sysRepeated, diaRepeated = Just diaRepeated }
    }


{-| The issue's example visit: blood pressure 165/115, right upper quadrant
tenderness, and an immediate urine dipstick with protein +1.
-}
severePreeclampsiaVisit : PrenatalMeasurements
severePreeclampsiaVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withSeverePreeclampsiaSigns
        |> withUrineProtein ProteinPlus1


{-| Blood pressure 165/115 and an immediate urine dipstick with protein +1.
-}
moderatePreeclampsiaByProteinVisit : PrenatalMeasurements
moderatePreeclampsiaByProteinVisit =
    emptyPrenatalMeasurements
        |> withVitals 165 115
        |> withUrineProtein ProteinPlus1


preeclampsiaRiskSevereAt37PlusTest : Test
preeclampsiaRiskSevereAt37PlusTest =
    describe "preeclampsia risk: severe preeclampsia at 37 weeks or later"
        [ test "At 37 weeks or later, a visit that finds severe preeclampsia does not also give high or moderate risk of preeclampsia" <|
            -- The issue's steps: 38 weeks, 165/115, right upper quadrant
            -- tenderness, urine protein +1 with an immediate result.
            \_ ->
                diagnoseVisitAt ( 38, 0 ) severePreeclampsiaVisit
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "severe preeclampsia at exactly 37 weeks 0 days: no risk of preeclampsia" <|
            \_ ->
                diagnoseVisitAt ( 37, 0 ) severePreeclampsiaVisit
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "severe preeclampsia at 38 weeks, with preeclampsia at a previous pregnancy: no high risk of preeclampsia" <|
            \_ ->
                severePreeclampsiaVisit
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ], [] )

        -- Headache with blurred vision alone is severe preeclampsia, with no
        -- blood pressure, so no hypertension: the risk can only come from
        -- history here.
        , test "headache with blurred vision at 38 weeks, with preeclampsia at a previous pregnancy: severe preeclampsia, no high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withDangerSigns (EverySet.singleton HeadacheBlurredVision)
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "headache with blurred vision at 38 weeks, more than 10 years since the last delivery: severe preeclampsia, no moderate risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withDangerSigns (EverySet.singleton HeadacheBlurredVision)
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ], [] )
        ]


preeclampsiaRiskModerateAt37PlusTest : Test
preeclampsiaRiskModerateAt37PlusTest =
    describe "preeclampsia risk: moderate preeclampsia at 37 weeks or later"
        [ test "At 37 weeks or later, a visit that finds moderate preeclampsia does not also give high or moderate risk of preeclampsia" <|
            \_ ->
                diagnoseVisitAt ( 38, 0 ) moderatePreeclampsiaByProteinVisit
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "moderate preeclampsia at exactly 37 weeks 0 days: no risk of preeclampsia" <|
            \_ ->
                diagnoseVisitAt ( 37, 0 ) moderatePreeclampsiaByProteinVisit
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "moderate preeclampsia by edema (165/115 and edema on the hands) at 38 weeks: no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withHandsEdema
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ], [] )
        , test "moderate preeclampsia at 38 weeks, with preeclampsia at a previous pregnancy: no high risk of preeclampsia" <|
            \_ ->
                moderatePreeclampsiaByProteinVisit
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ], [] )

        -- The 165/115 also gives hypertension, so today high risk shows and
        -- hides the moderate risk from history; neither may be left.
        , test "moderate preeclampsia at 38 weeks, more than 10 years since the last delivery: no moderate risk of preeclampsia" <|
            \_ ->
                moderatePreeclampsiaByProteinVisit
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ], [] )
        ]


preeclampsiaRiskEclampsiaTest : Test
preeclampsiaRiskEclampsiaTest =
    let
        eclampsiaAndRisks set =
            ( EverySet.member DiagnosisEclampsia set, presentOf preeclampsiaRiskDiagnoses set )

        convulsions =
            withDangerSigns (EverySet.singleton Convulsions)
    in
    describe "preeclampsia risk: eclampsia at any week"
        [ test "At any week, a visit that finds eclampsia does not also give high or moderate risk of preeclampsia" <|
            -- Convulsions and 165/115 at 28 weeks: the blood pressure alone
            -- would give gestational hypertension and high risk.
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withVitals 165 115
                    |> diagnoseVisitAt ( 28, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )
        , test "convulsions at 28 weeks, with preeclampsia at a previous pregnancy: eclampsia, no high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 28, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )
        , test "convulsions at 28 weeks, more than 10 years since the last delivery: eclampsia, no moderate risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 28, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )

        -- Before week 20 high blood pressure is chronic hypertension, which
        -- gives high risk just as gestational hypertension does.
        , test "convulsions and 165/115 at 14 weeks (risk diagnoses start at week 12): eclampsia, no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withVitals 165 115
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 14, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )
        , test "convulsions and 165/115 at 36 weeks 6 days: eclampsia, no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withVitals 165 115
                    |> diagnoseVisitAt ( 36, 6 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )
        , test "convulsions and 165/115 at 38 weeks, with preeclampsia at a previous pregnancy: eclampsia, no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> convulsions
                    |> withVitals 165 115
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )

        -- Eclampsia together with the measurements of severe preeclampsia:
        -- the blood pressure hierarchy keeps eclampsia alone, and the risk
        -- must not survive it either.
        , test "convulsions on the issue's severe preeclampsia visit at 38 weeks: eclampsia, no risk of preeclampsia" <|
            \_ ->
                severePreeclampsiaVisit
                    |> convulsions
                    |> diagnoseVisitAt ( 38, 0 )
                    |> eclampsiaAndRisks
                    |> Expect.equal ( True, [] )
        ]


preeclampsiaRiskLabResultsAt37PlusTest : Test
preeclampsiaRiskLabResultsAt37PlusTest =
    describe "preeclampsia risk: preeclampsia found in Lab Results at 37 weeks or later"
        [ test "At 37 weeks or later, when preeclampsia is found in Lab Results (a later urine result, or the blood pressure recheck), no high or moderate risk of preeclampsia stays beside it" <|
            -- 165/115 at 38 weeks with the urine sent to the lab: the visit
            -- day gives gestational hypertension and high risk. Protein +1
            -- entered later makes it moderate preeclampsia.
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 38, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )
        , test "later urine result at 37 weeks 0 days: preeclampsia, no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 37, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )
        , test "later urine result completing severe preeclampsia (165/115 and right upper quadrant tenderness at the visit) at 38 weeks: no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withSeverePreeclampsiaSigns
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 38, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )
        , test "later urine result at 38 weeks, with preeclampsia at a previous pregnancy: the visit-day high risk does not stay beside preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> withHighRiskHistory
                    |> enterResultLater ( 38, 0 ) 3 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )

        -- Marginal 145/95 at the visit with protein +1 read immediately; the
        -- recheck 150/95 on the Lab Results page gives hypertension after
        -- recheck, and with the protein, moderate preeclampsia.
        , test "blood pressure recheck at 38 weeks finding moderate preeclampsia: no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMarginalVitals
                    |> withUrineProtein ProteinPlus1
                    |> enterResultLater ( 38, 0 ) 0 (enterRecheck 150 95)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )
        , test "blood pressure recheck at 38 weeks finding severe preeclampsia (recheck 165/115, protein +1, right upper quadrant tenderness): no risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMarginalVitals
                    |> withUrineProtein ProteinPlus1
                    |> withSeverePreeclampsiaSigns
                    |> enterResultLater ( 38, 0 ) 0 (enterRecheck 165 115)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )

        -- The recheck also gives hypertension, so today high risk shows and
        -- hides the moderate risk from history. Taking high risk away must
        -- not let the moderate risk surface in its place.
        , test "blood pressure recheck at 38 weeks finding moderate preeclampsia, more than 10 years since the last delivery: neither high nor moderate risk stays beside preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMarginalVitals
                    |> withUrineProtein ProteinPlus1
                    |> withModerateRiskHistory
                    |> enterResultLater ( 38, 0 ) 0 (enterRecheck 150 95)
                    |> .afterEntry
                    |> preeclampsiaFoundAndRisks
                    |> Expect.equal ( True, [] )
        ]


{-| Today's behaviour before week 37, pinned: the [20, 37) preeclampsia
diagnoses already keep the risk diagnoses off, and hypertension or history
alone still give them.
-}
preeclampsiaRiskBefore37Test : Test
preeclampsiaRiskBefore37Test =
    describe "preeclampsia risk: before 37 weeks"
        [ test "Before 37 weeks, preeclampsia and the risk diagnoses behave as today" <|
            -- Moderate preeclampsia at 36 weeks 6 days, the last day before
            -- the EGA 37+ variants, with preeclampsia at a previous pregnancy.
            \_ ->
                moderatePreeclampsiaByProteinVisit
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 36, 6 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaInitialPhase ], [] )
        , test "guard: the issue's severe preeclampsia visit at 28 weeks gives severe preeclampsia and no risk" <|
            \_ ->
                diagnoseVisitAt ( 28, 0 ) severePreeclampsiaVisit
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhase ], [] )
        , test "guard: headache with blurred vision at 28 weeks, more than 10 years since the last delivery, gives severe preeclampsia and no moderate risk" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withDangerSigns (EverySet.singleton HeadacheBlurredVision)
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 28, 0 )
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisSeverePreeclampsiaInitialPhase ], [] )
        , test "guard: a later urine result completing moderate preeclampsia at 36 weeks 6 days leaves no risk beside it" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 36, 6 ) 0 (enterUrineProtein ProteinPlus1)
                    |> .afterEntry
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaRecurrentPhase ], [] )
        , test "guard: a blood pressure recheck finding moderate preeclampsia at 28 weeks leaves no risk beside it" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMarginalVitals
                    |> withUrineProtein ProteinPlus1
                    |> enterResultLater ( 28, 0 ) 0 (enterRecheck 150 95)
                    |> .afterEntry
                    |> preeclampsiaAndRisks
                    |> Expect.equal ( [ DiagnosisModeratePreeclampsiaRecurrentPhase ], [] )
        , test "guard: 165/115 alone at 36 weeks 6 days gives gestational hypertension and high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> diagnoseVisitAt ( 36, 6 )
                    |> membership [ DiagnosisGestationalHypertensionImmediate, DiagnosisHighRiskOfPreeclampsiaInitialPhase ]
                    |> Expect.equal [ ( DiagnosisGestationalHypertensionImmediate, True ), ( DiagnosisHighRiskOfPreeclampsiaInitialPhase, True ) ]
        , test "guard: more than 10 years since the last delivery at 28 weeks gives moderate risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 28, 0 )
                    |> presentOf preeclampsiaRiskDiagnoses
                    |> Expect.equal [ DiagnosisModerateRiskOfPreeclampsia ]
        ]


preeclampsiaRiskHypertensionAt37PlusTest : Test
preeclampsiaRiskHypertensionAt37PlusTest =
    let
        highRisk set =
            not <|
                List.isEmpty
                    (presentOf [ DiagnosisHighRiskOfPreeclampsiaInitialPhase, DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ] set)
    in
    describe "preeclampsia risk: high blood pressure without preeclampsia at 37 weeks or later"
        [ test "At 37 weeks or later, high blood pressure without preeclampsia or eclampsia still gives high risk of preeclampsia, as today" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> diagnoseVisitAt ( 38, 0 )
                    |> membership [ DiagnosisGestationalHypertensionImmediate, DiagnosisHighRiskOfPreeclampsiaInitialPhase ]
                    |> Expect.equal [ ( DiagnosisGestationalHypertensionImmediate, True ), ( DiagnosisHighRiskOfPreeclampsiaInitialPhase, True ) ]
        , test "165/115 at exactly 37 weeks 0 days: high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> diagnoseVisitAt ( 37, 0 )
                    |> highRisk
                    |> Expect.equal True

        -- Protein 0 is a urine result that rules preeclampsia out; it must
        -- not be read as preeclampsia found.
        , test "165/115 at 38 weeks with urine protein 0 (immediate result): high risk of preeclampsia, no preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withUrineProtein Protein0
                    |> diagnoseVisitAt ( 38, 0 )
                    |> (\set -> ( presentOf allPreeclampsiaDiagnoses set, highRisk set ))
                    |> Expect.equal ( [], True )

        -- The urine is sent to the lab and no result is in yet: nothing has
        -- found preeclampsia, so the high risk stands.
        , test "165/115 at 38 weeks with the urine result still pending: high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> diagnoseVisitAt ( 38, 0 )
                    |> highRisk
                    |> Expect.equal True
        , test "a later urine result of protein 0 at 38 weeks: the high risk from 165/115 stays" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 165 115
                    |> withPendingUrineDipstick
                    |> enterResultLater ( 38, 0 ) 3 (enterUrineProtein Protein0)
                    |> .afterEntry
                    |> highRisk
                    |> Expect.equal True
        , test "a blood pressure recheck of 150/95 at 38 weeks with no protein or edema: high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withMarginalVitals
                    |> enterResultLater ( 38, 0 ) 0 (enterRecheck 150 95)
                    |> .afterEntry
                    |> highRisk
                    |> Expect.equal True
        , test "guard: preeclampsia at a previous pregnancy at 38 weeks, normal blood pressure: high risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 120 80
                    |> withHighRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> highRisk
                    |> Expect.equal True
        , test "guard: more than 10 years since the last delivery at 38 weeks, normal blood pressure: moderate risk of preeclampsia" <|
            \_ ->
                emptyPrenatalMeasurements
                    |> withVitals 120 80
                    |> withModerateRiskHistory
                    |> diagnoseVisitAt ( 38, 0 )
                    |> presentOf preeclampsiaRiskDiagnoses
                    |> Expect.equal [ DiagnosisModerateRiskOfPreeclampsia ]
        ]


{-| The emergency warning lists the diagnoses a save added that require an
emergency referral: at the visit, `Pages.Prenatal.Encounter.Utils`' list; on
the Lab Results page, `Pages.Prenatal.RecurrentActivity.Utils`'. The hospital
referral reads the diagnoses stored on the encounter. Both are pinned to what
they show today.
-}
preeclampsiaRiskReferralUnchangedTest : Test
preeclampsiaRiskReferralUnchangedTest =
    let
        -- A first visit's save adds its whole diagnosis set.
        riskVisitWarning ega measurements =
            diagnoseVisitAt ega measurements
                |> EverySet.toList
                |> List.filter Pages.Prenatal.Encounter.Utils.diagnosisRequiresEmergencyReferal

        visitReferral ega measurements =
            let
                stored =
                    diagnoseVisitAt ega measurements
            in
            visitAssembled ega [] stored measurements
                |> diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseInitial
                |> EverySet.toList

        riskLabResultsWarning result =
            addedByLateResult result
                |> List.filter Pages.Prenatal.RecurrentActivity.Utils.diagnosisRequiresEmergencyReferal

        labResultsReferral ega measurementsAfterEntry result =
            visitAssembled ega [] result.afterEntry measurementsAfterEntry
                |> diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseRecurrent
                |> EverySet.toList

        laterUrineVisit =
            emptyPrenatalMeasurements
                |> withVitals 165 115
                |> withPendingUrineDipstick

        recheckVisit =
            emptyPrenatalMeasurements
                |> withMarginalVitals
                |> withUrineProtein ProteinPlus1
                |> withSeverePreeclampsiaSigns
    in
    describe "preeclampsia risk: emergency warning and hospital referral"
        [ test "The emergency warning and the hospital referral are the same as today" <|
            -- The issue's visit: the warning names severe preeclampsia only,
            -- and Next Steps refers to hospital for it.
            \_ ->
                ( riskVisitWarning ( 38, 0 ) severePreeclampsiaVisit, visitReferral ( 38, 0 ) severePreeclampsiaVisit )
                    |> Expect.equal
                        ( [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]
                        , [ DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ]
                        )
        , test "moderate preeclampsia at 38 weeks: warning and referral name moderate preeclampsia" <|
            \_ ->
                ( riskVisitWarning ( 38, 0 ) moderatePreeclampsiaByProteinVisit, visitReferral ( 38, 0 ) moderatePreeclampsiaByProteinVisit )
                    |> Expect.equal
                        ( [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                        , [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ]
                        )
        , test "convulsions and 165/115 at 28 weeks: warning and referral name eclampsia" <|
            \_ ->
                let
                    visit =
                        emptyPrenatalMeasurements
                            |> withDangerSigns (EverySet.singleton Convulsions)
                            |> withVitals 165 115
                in
                ( riskVisitWarning ( 28, 0 ) visit, visitReferral ( 28, 0 ) visit )
                    |> Expect.equal ( [ DiagnosisEclampsia ], [ DiagnosisEclampsia ] )
        , test "later urine result at 38 weeks: the Lab Results warning and referral name moderate preeclampsia" <|
            \_ ->
                let
                    result =
                        enterResultLater ( 38, 0 ) 3 (enterUrineProtein ProteinPlus1) laterUrineVisit
                in
                ( riskLabResultsWarning result
                , labResultsReferral ( 38, 0 ) (enterUrineProtein ProteinPlus1 laterUrineVisit) result
                )
                    |> Expect.equal
                        ( [ DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ]
                        , [ DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ]
                        )
        , test "blood pressure recheck at 38 weeks finding severe preeclampsia: the Lab Results warning and referral name severe preeclampsia" <|
            \_ ->
                let
                    result =
                        enterResultLater ( 38, 0 ) 0 (enterRecheck 165 115) recheckVisit
                in
                ( riskLabResultsWarning result
                , labResultsReferral ( 38, 0 ) (enterRecheck 165 115 recheckVisit) result
                )
                    |> Expect.equal
                        ( [ DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ]
                        , [ DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ]
                        )
        , test "guard: no preeclampsia risk diagnosis brings up the emergency warning, at the visit or on the Lab Results page" <|
            \_ ->
                List.filter
                    (\diagnosis ->
                        Pages.Prenatal.Encounter.Utils.diagnosisRequiresEmergencyReferal diagnosis
                            || Pages.Prenatal.RecurrentActivity.Utils.diagnosisRequiresEmergencyReferal diagnosis
                    )
                    preeclampsiaRiskDiagnoses
                    |> Expect.equal []
        ]


{-| The tetanus immunisation form on the day a dose falls due.

The scenarios drive the form through the page's own `update`, with the
messages a tap sends, and then read it through the same function the page
and the encounter's task count use. Save is enabled when the form's tasks
completed equal its tasks active. The nurse is asked "will receive today?".

  - Dose 1 was given four weeks ago, in an earlier encounter: dose 2 falls
    due today, four weeks later. Dose 3 is due six months after dose 2.
  - Doses 1 and 2 were given before this pregnancy (30 weeks ago, at EGA 20
    weeks): dose 3 falls due today, because a woman who had no dose during
    this pregnancy gets the next one right away. Dose 4 is due a year after
    dose 3.

"Recorded as given previously with today's date" is: "Administered
previously?" Yes, pick today in the date picker (the only day it offers for
a dose that falls due today), save the date.

-}
tetanusDoseFallingDueTodayTest : Test
tetanusDoseFallingDueTodayTest =
    let
        encounterId =
            toEntityUuid "dummy-encounter"

        -- The page model after the given taps, starting from a fresh page.
        afterTaps msgs =
            List.foldl
                (\msg model ->
                    let
                        ( updated, _, _ ) =
                            update currentDate SiteRwanda encounterId emptyModelIndexedDb msg model
                    in
                    updated
                )
                emptyModel
                msgs
                |> .immunisationData
                |> .tetanusForm

        assembledWith history measurements =
            let
                base =
                    testAssembled measurements
            in
            { base | vaccinationHistory = Dict.singleton VaccineTetanus history, vaccinationProgress = Dict.singleton VaccineTetanus history }

        formOf history form =
            Pages.Prenatal.Activity.Utils.vaccinationFormDynamicContentAndTasks English
                currentDate
                SiteRwanda
                (assembledWith history emptyPrenatalMeasurements)
                VaccineTetanus
                form

        inputsOf ( inputs, _, _ ) =
            inputs

        asQuery inputs =
            Query.fromHtml (div [] inputs)

        notAskedAbout inputs =
            asQuery inputs
                |> Expect.all
                    [ Query.hasNot [ text TestFixtures.vaccinationQuestionPrevious ]
                    , Query.hasNot [ text TestFixtures.vaccinationQuestionToday ]
                    ]

        askedAbout question inputs =
            asQuery inputs
                |> Query.has [ text question ]

        -- What the page saves for the form, as Save does on a new measurement.
        savedValue form =
            toVaccinationValueWithDefault Nothing form

        recordedPreviouslyOn date dose =
            afterTaps
                [ SetUpdatePreviousVaccines VaccineTetanus dose True
                , SetVaccinationUpdateDate VaccineTetanus date
                , SaveVaccinationUpdateDate VaccineTetanus dose
                ]

        -- Dose 1 four weeks ago: dose 2 due today.
        firstDoseDate =
            Date.add Date.Weeks -4 currentDate

        dose2DueToday =
            Dict.singleton VaccineDoseFirst firstDoseDate

        dose2RecordedPreviouslyToday =
            recordedPreviouslyOn currentDate VaccineDoseSecond

        dose2GivenToday =
            afterTaps
                [ SetUpdatePreviousVaccines VaccineTetanus VaccineDoseSecond False
                , SetWillReceiveVaccineToday VaccineTetanus VaccineDoseSecond True
                ]

        -- Doses 1 and 2 before this pregnancy: dose 3 due today.
        dose3DueToday =
            Dict.fromList
                [ ( VaccineDoseFirst, Date.add Date.Weeks -34 currentDate )
                , ( VaccineDoseSecond, Date.add Date.Weeks -30 currentDate )
                ]
    in
    describe "Tetanus immunisation form, a dose that falls due today"
        [ describe "A dose that falls due today, recorded as given previously with today's date, shows in the dose history and is not asked about again"
            [ test "on the form, once the date is saved" <|
                \_ ->
                    formOf dose2DueToday dose2RecordedPreviouslyToday
                        |> inputsOf
                        |> Expect.all
                            [ TestFixtures.expectDoseInHistory 1 firstDoseDate
                            , TestFixtures.expectDoseInHistory 2 currentDate
                            , notAskedAbout
                            ]
            , test "once the measurement is saved and the form is opened again" <|
                \_ ->
                    vaccinationFormWithDefault emptyVaccinationForm (savedValue dose2RecordedPreviouslyToday)
                        |> formOf dose2DueToday
                        |> inputsOf
                        |> Expect.all
                            [ TestFixtures.expectDoseInHistory 2 currentDate
                            , notAskedAbout
                            ]
            , test "once the measurement is saved and the form is revisited in the same session" <|
                \_ ->
                    vaccinationFormWithDefault dose2RecordedPreviouslyToday (savedValue dose2RecordedPreviouslyToday)
                        |> formOf dose2DueToday
                        |> Expect.all
                            [ inputsOf >> TestFixtures.expectDoseInHistory 2 currentDate
                            , inputsOf >> notAskedAbout
                            , TestFixtures.expectSaveEnabled
                            ]
            , test "for a dose due today because the last one was before this pregnancy" <|
                \_ ->
                    recordedPreviouslyOn currentDate VaccineDoseThird
                        |> formOf dose3DueToday
                        |> inputsOf
                        |> Expect.all
                            [ TestFixtures.expectDoseInHistory 3 currentDate
                            , notAskedAbout
                            ]
            , test "the encounter counts the saved immunisation task as completed" <|
                \_ ->
                    let
                        measurements =
                            { emptyPrenatalMeasurements
                                | tetanusImmunisation =
                                    savedValue dose2RecordedPreviouslyToday
                                        |> Maybe.andThen wrapMeasurement
                            }

                        ( completed, active ) =
                            immunisationTasksCompletedFromTotal English
                                currentDate
                                SiteRwanda
                                (assembledWith dose2DueToday measurements)
                                emptyModel.immunisationData
                                TaskTetanus
                    in
                    Expect.equal active completed
            ]
        , describe "After saving that date, the next dose is not asked about (it is not due yet), and Save is enabled"
            [ test "dose 3 is due six months after dose 2" <|
                \_ ->
                    formOf dose2DueToday dose2RecordedPreviouslyToday
                        |> Expect.all
                            [ inputsOf >> notAskedAbout
                            , TestFixtures.expectSaveEnabled
                            ]
            , test "dose 4 is due a year after dose 3, recorded on a day it fell due because of the pregnancy" <|
                \_ ->
                    recordedPreviouslyOn currentDate VaccineDoseThird
                        |> formOf dose3DueToday
                        |> Expect.all
                            [ inputsOf >> notAskedAbout
                            , TestFixtures.expectSaveEnabled
                            ]
            , test "once the measurement is saved and the form is opened again" <|
                \_ ->
                    vaccinationFormWithDefault emptyVaccinationForm (savedValue dose2RecordedPreviouslyToday)
                        |> formOf dose2DueToday
                        |> Expect.all
                            [ inputsOf >> notAskedAbout
                            , TestFixtures.expectSaveEnabled
                            ]
            ]
        , describe "A dose given today (\"will receive today: Yes\") keeps its questions on the form, answered, and Save is enabled, as today"
            [ test "on the form, the question is about the dose given today" <|
                \_ ->
                    formOf dose2DueToday dose2GivenToday
                        |> Expect.all
                            [ inputsOf >> TestFixtures.expectDoseInHistory 2 currentDate
                            , inputsOf >> askedAbout TestFixtures.vaccinationQuestionToday
                            , inputsOf >> TestFixtures.expectYesAnswerSends 0 (SetWillReceiveVaccineToday VaccineTetanus VaccineDoseSecond True)
                            , TestFixtures.expectSaveEnabled
                            , \( _, _, active ) -> Expect.atLeast 1 active
                            ]
            , test "once the measurement is saved and the form is opened again" <|
                \_ ->
                    vaccinationFormWithDefault emptyVaccinationForm (savedValue dose2GivenToday)
                        |> formOf dose2DueToday
                        |> Expect.all
                            [ inputsOf >> TestFixtures.expectDoseInHistory 2 currentDate
                            , inputsOf >> askedAbout TestFixtures.vaccinationQuestionToday
                            , TestFixtures.expectSaveEnabled
                            , \( _, _, active ) -> Expect.atLeast 1 active
                            ]
            ]
        , describe "A dose that falls due today and has not been recorded is still asked about"
            [ test "\"administered previously?\", about that dose, with Save disabled until it is answered" <|
                \_ ->
                    formOf dose2DueToday emptyVaccinationForm
                        |> Expect.all
                            [ inputsOf >> askedAbout TestFixtures.vaccinationQuestionPrevious
                            , inputsOf >> TestFixtures.expectYesAnswerSends 0 (SetUpdatePreviousVaccines VaccineTetanus VaccineDoseSecond True)
                            , \( _, completed, active ) -> Expect.notEqual active completed
                            ]
            , test "for a nurse, \"will receive today?\" once \"administered previously?\" is answered No" <|
                \_ ->
                    afterTaps [ SetUpdatePreviousVaccines VaccineTetanus VaccineDoseSecond False ]
                        |> formOf dose2DueToday
                        |> inputsOf
                        |> Expect.all
                            [ askedAbout TestFixtures.vaccinationQuestionToday
                            , TestFixtures.expectYesAnswerSends 1 (SetWillReceiveVaccineToday VaccineTetanus VaccineDoseSecond True)
                            ]
            , test "\"administered previously?\", for a dose due today because the last one was before this pregnancy" <|
                \_ ->
                    formOf dose3DueToday emptyVaccinationForm
                        |> inputsOf
                        |> Expect.all
                            [ askedAbout TestFixtures.vaccinationQuestionPrevious
                            , TestFixtures.expectYesAnswerSends 0 (SetUpdatePreviousVaccines VaccineTetanus VaccineDoseThird True)
                            ]
            ]
        , describe "A dose given previously on an earlier day behaves as today"
            [ test "dose 2 recorded five weeks ago: in the history, deletable, the next dose not asked about, Save enabled" <|
                \_ ->
                    let
                        history =
                            Dict.singleton VaccineDoseFirst (Date.add Date.Weeks -10 currentDate)

                        secondDoseDate =
                            Date.add Date.Weeks -5 currentDate
                    in
                    recordedPreviouslyOn secondDoseDate VaccineDoseSecond
                        |> formOf history
                        |> Expect.all
                            [ inputsOf >> TestFixtures.expectDoseInHistory 2 secondDoseDate
                            , inputsOf >> asQuery >> Query.find [ class "history" ] >> Query.has [ class "delete" ]
                            , inputsOf >> notAskedAbout
                            , TestFixtures.expectSaveEnabled
                            ]
            ]
        ]
