module Measurement.Test exposing (all)

import AssocList as Dict
import Backend.ChildScoreboardEncounter.Model exposing (emptyChildScoreboardEncounter)
import Backend.Entities exposing (IndividualEncounterParticipantId, PersonId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), emptyIndividualEncounterParticipant)
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , AdverseEvent(..)
        , BloodSmearResult(..)
        , ChildNutritionSign(..)
        , ColorAlertIndication(..)
        , CreatinineTestValue
        , HealthEducationSign(..)
        , HealthEducationValue
        , HeightInCm(..)
        , LiverFunctionTestValue
        , MalariaTestValue
        , MuacInCm(..)
        , NCDASign(..)
        , NutritionAssessment(..)
        , PartnerHIVTestValue
        , ReasonForNonReferral(..)
        , ReasonForNotProvidingHealthEducation(..)
        , ReasonForNotTaking(..)
        , ReferralFacility(..)
        , SendToHCSign(..)
        , SendToHCValue
        , SkippedForm(..)
        , StuntingLevel(..)
        , TestExecutionNote(..)
        , TestPrerequisite(..)
        , TestResult(..)
        , TreatmentOngoingSign(..)
        , TreatmentOngoingValue
        , VaccineDose(..)
        , WeightInGrm(..)
        , WeightInKg(..)
        , WellChildVaccineType(..)
        )
import Backend.Model exposing (ModelIndexedDb, emptyModelIndexedDb)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), emptyPrenatalEncounter)
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Date, Unit(..))
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html, div)
import Html.Attributes
import Measurement.Model exposing (MsgChild(..), NCDAStep(..), OutMsgChild(..), RangedMeasurement(..), emptyCreatinineResultForm, emptyHIVTestForm, emptyHIVTestUniversalForm, emptyHealthEducationForm, emptyHeightForm, emptyHepatitisBTestForm, emptyLiverFunctionResultForm, emptyMalariaResultForm, emptyMalariaTestForm, emptyModelChild, emptyNCDAData, emptyNCDAForm, emptyOngoingTreatmentReviewForm, emptyPartnerHIVResultForm, emptyPartnerHIVTestForm, emptyPregnancyTestForm, emptyRandomBloodSugarTestUniversalForm, emptySendToHCForm)
import Measurement.Update exposing (updateChild)
import Measurement.Utils
    exposing
        ( ancVisitDateForMonth
        , birthWeightOutsideConstraints
        , bloodGlucoseOutOfRange
        , bornUnderweightByBirthWeight
        , creatinineResultFormWithDefault
        , generateIndividualNutritionAssessmentEntries
        , generateVaccinationProgressDictByChildScoreboard
        , getAllDosesForVaccine
        , getInputConstraintsHeight
        , getInputConstraintsMuac
        , getInputConstraintsWeight
        , getIntervalForVaccine
        , healthEducationFormWithDefault
        , heightFormWithDefault
        , initialVaccinationDateByBirthDate
        , knownAsPositiveUpdate
        , knownAsPositiveUpdateHIVTest
        , knownAsPositiveUpdatePartnerHIVTest
        , liverFunctionResultFormWithDefault
        , malariaResultFormWithDefault
        , malariaTestFormWithDefault
        , mergeVaccinationProgressDicts
        , ncdaFormWithDefault
        , ncdaMeasurementsOutOfRange
        , nextVaccinationDataForVaccine
        , ongoingTreatmentReviewFormWithDefault
        , outOfRangeAsEntered
        , partnerHIVResultFormWithDefault
        , rdtKnownAsPositiveUpdate
        , rdtTestPerformedUpdate
        , sendToHCFormWithDefault
        , setNCDAStep
        , showNCDAMeasurementOutOfRange
        , toHIVTestValueUniversalWithDefault
        , toHIVTestValueWithDefault
        , toHealthEducationValueWithDefault
        , toHepatitisBTestValueWithDefault
        , toMalariaResultValueWithDefault
        , toMalariaTestValueWithDefault
        , toNCDAValueWithDefault
        , toOngoingTreatmentReviewValueWithDefault
        , toPartnerHIVTestValueWithDefault
        , toPregnancyTestValueWithDefault
        , toRandomBloodSugarTestValueUniversalWithDefault
        , toSendToHCValueWithDefault
        , treatmentReviewInputsAndTasks
        )
import Measurement.View exposing (healthEducationFormInputsAndTasks, sendToFacilityInputsAndTasks, viewColorAlertIndication, viewNCDAContent)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, classes, containing, selected, tag, text)
import TestFixtures exposing (testChild, testParticipant)
import Time exposing (Month(..))
import Translate.Model exposing (Language(..))


viewChildFormsTest : Test
viewChildFormsTest =
    test "Re-implement viewChildFormsTest" <|
        always Expect.pass


viewMotherFormsTest : Test
viewMotherFormsTest =
    test "Re-implement viewMotherFormsTest" <|
        always Expect.pass


viewColorAlertIndicationTest : Test
viewColorAlertIndicationTest =
    describe "viewColorAlertIndication"
        [ test "red" <|
            \_ ->
                viewColorAlertIndication English ColorAlertRed
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-red" ]
                        , text "RED"
                        ]
        , test "yellow" <|
            \_ ->
                viewColorAlertIndication English ColorAlertYellow
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-yellow" ]
                        , text "YELLOW"
                        ]
        , test "green" <|
            \_ ->
                viewColorAlertIndication English ColorAlertGreen
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-green" ]
                        , text "GREEN"
                        ]
        ]


{-| Vaccine scheduling tests.

ORACLE: the "Standard Pediatric Visit" tab of the clinical sheet (Rwanda
schedule). Each [sheet]-marked expectation is taken from that sheet, not from
the code. [CODE]-marked cases are not covered by the sheet; they pin current
code behavior (findings, not oracle matches).

-}
getIntervalForVaccineTest : Test
getIntervalForVaccineTest =
    describe "getIntervalForVaccine"
        [ test "Rwanda OPV - sheet: no sooner than 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineOPV
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda DTP - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineDTP
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda PCV13 - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccinePCV13
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda Rotarix - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineRotarix
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda HPV - sheet: 6 months between doses" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineHPV
                    |> Expect.equal ( 6, Months )
        , test "Rwanda MR - sheet: doses 9mo -> 15mo (= 6 months apart)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineMR
                    |> Expect.equal ( 6, Months )
        , test "Rwanda BCG - sheet: single dose (interval 0)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineBCG
                    |> Expect.equal ( 0, Days )
        , test "[CODE] Burundi MR - sheet is Rwanda; Burundi interval differs (9 months)" <|
            \_ ->
                getIntervalForVaccine SiteBurundi VaccineMR
                    |> Expect.equal ( 9, Months )
        , test "[CODE] Rwanda IPV - placeholder 0; real 2nd-dose interval is special-cased per issue #1426" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineIPV
                    |> Expect.equal ( 0, Days )
        ]


getAllDosesForVaccineTest : Test
getAllDosesForVaccineTest =
    describe "getAllDosesForVaccine (Rwanda)"
        [ test "OPV, initialOpvAdministered=True - sheet: 4 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineOPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        , VaccineDoseFourth
                        ]
        , test "[CODE] OPV, initialOpvAdministered=False - without the birth dose, 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda False VaccineOPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "DTP - sheet: 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineDTP
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "PCV13 - sheet: 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccinePCV13
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "Rotarix - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineRotarix
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "MR - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineMR
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "HPV - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineHPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "BCG - sheet: 1 dose" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineBCG
                    |> Expect.equal
                        [ VaccineDoseFirst ]
        , test "[CODE] IPV - sheet shows only 1 IPV dose at 14wk; code adds a 2nd dose for Rwanda per issue #1426" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineIPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        ]


nextVaccinationDataForVaccineTest : Test
nextVaccinationDataForVaccineTest =
    let
        birthDate =
            Date.fromCalendarDate 2026 Time.Jan 1

        lastDoseDate =
            Date.fromCalendarDate 2026 Time.Jun 1

        -- Rwanda gives three doses of DTP, four weeks apart.
        nextDTPDose dose =
            nextVaccinationDataForVaccine SiteRwanda (Just birthDate) VaccineDTP False lastDoseDate dose
    in
    describe "nextVaccinationDataForVaccine (Rwanda, DTP)"
        [ test "a dose is due while the doses given are short of the three expected" <|
            \_ ->
                nextDTPDose VaccineDoseSecond
                    |> Expect.equal
                        (Just ( VaccineDoseThird, Date.fromCalendarDate 2026 Time.Jun 29 ))
        , test "no dose is due once the third has been given" <|
            \_ ->
                nextDTPDose VaccineDoseThird
                    |> Expect.equal Nothing
        , test "no dose is due when more doses are recorded than the three expected" <|
            \_ ->
                -- A record holding a fourth dose has no dose left to receive.
                -- Read as still owing one, the child is behind from the day the
                -- dose that does not exist falls due, and nothing clears it.
                nextDTPDose VaccineDoseFourth
                    |> Expect.equal Nothing
        ]


mergeVaccinationProgressDictsTest : Test
mergeVaccinationProgressDictsTest =
    let
        date day month year =
            Date.fromCalendarDate year month day

        wellChildProgress doses =
            Dict.fromList [ ( VaccineOPV, Dict.fromList doses ), ( VaccineBCG, Dict.empty ) ]

        -- The OPV doses of a child followed by both Well Child and Child Scoreboard.
        mergedOPVDoses wellChildDoses childScoreboardDoses =
            mergeVaccinationProgressDicts
                (wellChildProgress wellChildDoses)
                (Dict.fromList [ ( VaccineOPV, Dict.fromList childScoreboardDoses ), ( VaccineBCG, Dict.empty ) ])
                |> Dict.get VaccineOPV
                |> Maybe.map Dict.toList

        firstTwoDoses =
            [ ( VaccineDoseFirst, date 1 Oct 2025 ), ( VaccineDoseSecond, date 15 Aug 2026 ) ]
    in
    describe "mergeVaccinationProgressDicts"
        [ test "keeps the doses in dose order for a child followed by one program" <|
            \_ ->
                mergeVaccinationProgressDicts (wellChildProgress firstTwoDoses) Dict.empty
                    |> Dict.get VaccineOPV
                    |> Maybe.map Dict.toList
                    |> Expect.equal (Just firstTwoDoses)
        , test "keeps the doses in dose order when the other program has none" <|
            \_ ->
                mergedOPVDoses firstTwoDoses []
                    |> Expect.equal (Just firstTwoDoses)
        , test "puts doses recorded by both programs in dose order" <|
            \_ ->
                mergedOPVDoses
                    [ ( VaccineDoseFirst, date 1 Oct 2025 ), ( VaccineDoseThird, date 20 Sep 2026 ) ]
                    [ ( VaccineDoseSecond, date 15 Aug 2026 ) ]
                    |> Expect.equal
                        (Just
                            [ ( VaccineDoseFirst, date 1 Oct 2025 )
                            , ( VaccineDoseSecond, date 15 Aug 2026 )
                            , ( VaccineDoseThird, date 20 Sep 2026 )
                            ]
                        )
        , test "keeps the later date of a dose both programs recorded" <|
            \_ ->
                mergedOPVDoses
                    [ ( VaccineDoseFirst, date 1 Oct 2025 ) ]
                    [ ( VaccineDoseFirst, date 3 Oct 2025 ) ]
                    |> Expect.equal (Just [ ( VaccineDoseFirst, date 3 Oct 2025 ) ])
        ]


initialVaccinationDateByBirthDateTest : Test
initialVaccinationDateByBirthDateTest =
    let
        birthDate =
            Date.fromCalendarDate 2020 Time.Jan 1

        -- For dose-1 cases dosesInterval is 0, so the empty progress dict is
        -- never consulted. VaccinationProgressDict is an AssocList.Dict.
        emptyProgress =
            Dict.empty

        firstDoseDate vaccineType =
            initialVaccinationDateByBirthDate SiteRwanda birthDate True emptyProgress ( vaccineType, VaccineDoseFirst )
    in
    describe "initialVaccinationDateByBirthDate (Rwanda, dose 1 start age)"
        [ test "BCG - sheet: birth" <|
            \_ ->
                firstDoseDate VaccineBCG
                    |> Expect.equal birthDate
        , test "OPV - sheet: birth" <|
            \_ ->
                firstDoseDate VaccineOPV
                    |> Expect.equal birthDate
        , test "DTP - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccineDTP
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "PCV13 - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccinePCV13
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "Rotarix - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccineRotarix
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "IPV - sheet: 14 weeks" <|
            \_ ->
                firstDoseDate VaccineIPV
                    |> Expect.equal (Date.add Weeks 14 birthDate)
        , test "MR - sheet: 36 weeks / 9 months" <|
            \_ ->
                firstDoseDate VaccineMR
                    |> Expect.equal (Date.add Weeks 36 birthDate)
        , test "HPV - sheet: 12 years" <|
            \_ ->
                firstDoseDate VaccineHPV
                    |> Expect.equal (Date.add Years 12 birthDate)
        , test "[CODE] MR dose 2 - 36wk + 6mo (~14.3mo); sheet says 15 months (~3-week precision gap)" <|
            \_ ->
                initialVaccinationDateByBirthDate SiteRwanda birthDate True emptyProgress ( VaccineMR, VaccineDoseSecond )
                    |> Expect.equal (Date.add Months 6 (Date.add Weeks 36 birthDate))
        ]


updateChildSetMuacTest : Test
updateChildSetMuacTest =
    -- The NCDA MUAC field stores cm. At Burundi the nurse enters mm, so the
    -- group-session input handler must divide by 10 (like every other MUAC
    -- field) rather than store the typed value verbatim.
    describe "updateChild SetMuac (group NCDA MUAC input is site-aware)"
        [ test "Burundi: entering 125 (mm) stores 12.5 cm" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteBurundi (SetMuac "125") emptyModelChild
                in
                model.ncdaData.form.muac
                    |> Expect.equal (Just (MuacInCm 12.5))
        , test "Rwanda: entering 12.5 (cm) stores 12.5 cm unchanged" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteRwanda (SetMuac "12.5") emptyModelChild
                in
                model.ncdaData.form.muac
                    |> Expect.equal (Just (MuacInCm 12.5))
        ]


{-| The treatment question is asked only while the MUAC is off. An answer the
form kept from before the MUAC changed would be saved behind a hidden question,
for a child the form no longer calls malnourished.
-}
ncdaTreatmentAnswerSavedTest : Test
ncdaTreatmentAnswerSavedTest =
    let
        treatedSaved saved form =
            toNCDAValueWithDefault saved form
                |> Maybe.map (.signs >> EverySet.member TreatedForAcuteMalnutrition)

        answeredYes =
            { emptyNCDAForm | treatedForAcuteMalnutrition = Just True }

        savedWithOffMuac =
            Just
                { signs = EverySet.singleton TreatedForAcuteMalnutrition
                , birthWeight = Nothing
                , ancVisitsDates = EverySet.empty
                , receivesVitaminA = Nothing
                , stuntingLevel = Nothing
                , weight = Nothing
                , muac = Just (MuacInCm 11)
                }
    in
    describe "the treatment answer and the MUAC it was asked for"
        [ test "an answer is saved while the MUAC is off" <|
            \_ ->
                treatedSaved Nothing { answeredYes | muac = Just (MuacInCm 11) }
                    |> Expect.equal (Just True)
        , test "an answer kept from before the MUAC was corrected to green is not saved" <|
            \_ ->
                treatedSaved Nothing { answeredYes | muac = Just (MuacInCm 14) }
                    |> Expect.equal (Just False)
        , test "an answer kept from before the saved MUAC was marked not taken is not saved" <|
            \_ ->
                treatedSaved savedWithOffMuac { answeredYes | muacNotTaken = Just True }
                    |> Expect.equal (Just False)
        , test "a saved answer is not saved again once the MUAC on the form is green" <|
            \_ ->
                treatedSaved savedWithOffMuac { emptyNCDAForm | muac = Just (MuacInCm 14) }
                    |> Expect.equal (Just False)
        ]


ncdaMeasurementsOutOfRangeTest : Test
ncdaMeasurementsOutOfRangeTest =
    -- The form may only be stopped over a measurement the nurse can actually
    -- reach. Each of these questions is asked only in some circumstances, and a
    -- value saved earlier stays on the form even once its question is gone, so
    -- a form stopped over one it does not show could not be left at all. Every
    -- condition is checked on its own here for that reason.
    let
        allSteps =
            [ NCDAStepAntenatalCare
            , NCDAStepNutritionAssessment
            , NCDAStepUniversalInterventions
            ]

        newbornExamWithoutBirthWeight =
            Nothing

        newbornExamWithBirthWeight =
            Just
                { expectedDateConcluded = Date.fromCalendarDate 2020 Jan 1
                , deliveryComplications = EverySet.empty
                , signs = EverySet.empty
                , apgarOneMin = Nothing
                , apgarFiveMin = Nothing
                , birthWeight = Just (WeightInGrm 3000)
                , birthLength = Nothing
                , birthDefects = EverySet.empty
                }
    in
    describe "ncdaMeasurementsOutOfRange"
        [ describe "the birth weight, asked on the Antenatal Care step"
            [ test "a weight in kilograms is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal [ ( MeasurementBirthWeight, NCDAStepAntenatalCare ) ]
            , test "a weight in grams is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3000) }
                        |> Expect.equal []
            , test "without that step it is not named, though the form still holds it" <|
                -- The step is dropped once an NCDA was filled before, and the
                -- weight saved then is still on the form. There is no field to
                -- correct it, so stopping here would leave the form unsavable.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda [ NCDAStepNutritionAssessment ] newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal []
            , test "when the newborn exam already recorded one, it is not asked and not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal []
            ]
        , describe "the weight, asked on the Nutrition Assessment step"
            [ test "a weight in grams is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8500) }
                        |> Expect.equal [ ( MeasurementWeight, NCDAStepNutritionAssessment ) ]
            , test "a weight in kilograms is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8.5) }
                        |> Expect.equal []
            , test "at a health centre that step is not asked, so it is not named" <|
                -- The step is only part of the form when the NCDA is filled by
                -- a CHW, and a weight saved earlier is still on the form.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda [ NCDAStepAntenatalCare ] newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8500) }
                        |> Expect.equal []
            , test "a form holding no weight is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True emptyNCDAForm
                        |> Expect.equal []
            ]
        , describe "the MUAC, asked from six months of age"
            [ test "a MUAC in millimetres is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 125) }
                        |> Expect.equal [ ( MeasurementMuac, NCDAStepNutritionAssessment ) ]
            , test "a MUAC in centimetres is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 12.5) }
                        |> Expect.equal []
            , test "under six months it is not asked, so it is not named" <|
                -- The exact regression this had three times before: the MUAC
                -- input is only drawn from six months, and gating on a value
                -- the form never shows deadened the button with nothing on
                -- screen to correct.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight False { emptyNCDAForm | muac = Just (MuacInCm 125) }
                        |> Expect.equal []
            , test "Burundi: a form holding 12.5 cm is 125 mm there, so it is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteBurundi allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 12.5) }
                        |> Expect.equal []
            , test "Burundi: a form holding 1.25 cm is 12.5 mm there, below the range" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteBurundi allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 1.25) }
                        |> Expect.equal [ ( MeasurementMuac, NCDAStepNutritionAssessment ) ]
            ]
        , describe "saying a measurement could not be taken"
            [ test "empties it, so there is nothing left to be out of range" <|
                -- Ticking the box clears the value, and hydrating a saved form
                -- keeps it cleared. Were either to leave the old value behind,
                -- the form would be stopped by a measurement whose input is not
                -- even drawn.
                \_ ->
                    ncdaFormWithDefault
                        { emptyNCDAForm | weightNotTaken = Just True }
                        (Just
                            { signs = EverySet.empty
                            , birthWeight = Nothing
                            , ancVisitsDates = EverySet.empty
                            , receivesVitaminA = Nothing
                            , stuntingLevel = Nothing
                            , weight = Just (WeightInKg 8500)
                            , muac = Nothing
                            }
                        )
                        |> ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True
                        |> Expect.equal []
            ]
        , describe "several at once"
            [ test "all are named, in the order the form asks them" <|
                -- The nurse is told about every one, rather than correcting one
                -- and being stopped again by the next.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda
                        allSteps
                        newbornExamWithoutBirthWeight
                        True
                        { emptyNCDAForm
                            | birthWeight = Just (WeightInGrm 3)
                            , weight = Just (WeightInKg 8500)
                            , muac = Just (MuacInCm 125)
                        }
                        |> Expect.equal
                            [ ( MeasurementBirthWeight, NCDAStepAntenatalCare )
                            , ( MeasurementWeight, NCDAStepNutritionAssessment )
                            , ( MeasurementMuac, NCDAStepNutritionAssessment )
                            ]
            ]
        ]


{-| The warning has to leave the nurse looking at the input it names.

Four pages hold this form and each moves it the same way, which is why the move
is said once and asked for here: dropping it on any one of them would name a
measurement asked on a step she is not on, and nothing else would notice.

-}
showNCDAMeasurementOutOfRangeTest : Test
showNCDAMeasurementOutOfRangeTest =
    let
        onStep step data =
            { data | form = (\form -> { form | step = Just step }) data.form }

        shownAndStep data =
            ( data.showMeasurementOutOfRangePopup, data.form.step )
    in
    describe "showNCDAMeasurementOutOfRange"
        [ test "showing it opens the form on the step that asks for the measurement" <|
            \_ ->
                emptyNCDAData
                    |> onStep NCDAStepInfrastructureEnvironment
                    |> showNCDAMeasurementOutOfRange (Just NCDAStepNutritionAssessment)
                    |> shownAndStep
                    |> Expect.equal ( True, Just NCDAStepNutritionAssessment )
        , test "hiding it leaves the form where it is" <|
            \_ ->
                emptyNCDAData
                    |> onStep NCDAStepNutritionAssessment
                    |> showNCDAMeasurementOutOfRange Nothing
                    |> shownAndStep
                    |> Expect.equal ( False, Just NCDAStepNutritionAssessment )
        , test "hiding it on a form that was never moved leaves it unmoved" <|
            \_ ->
                emptyNCDAData
                    |> showNCDAMeasurementOutOfRange Nothing
                    |> shownAndStep
                    |> Expect.equal ( False, Nothing )
        , test "setNCDAStep moves the form and touches nothing else" <|
            \_ ->
                setNCDAStep NCDAStepTargetedInterventions emptyNCDAData
                    |> shownAndStep
                    |> Expect.equal ( False, Just NCDAStepTargetedInterventions )
        ]


bornUnderweightByBirthWeightTest : Test
bornUnderweightByBirthWeightTest =
    -- Below 2500g the child was born underweight - the threshold the red
    -- callout, the scorecard and the aggregated scorecard all ask for. A weight
    -- that cannot be read answers that we do not know, rather than that the
    -- child was not underweight.
    describe "bornUnderweightByBirthWeight"
        [ test "an ordinary birth weight is not underweight" <|
            \_ ->
                bornUnderweightByBirthWeight (Just (WeightInGrm 3000))
                    |> Expect.equal (Just False)
        , test "a weight below the threshold is underweight" <|
            \_ ->
                bornUnderweightByBirthWeight (Just (WeightInGrm 2300))
                    |> Expect.equal (Just True)
        , test "the threshold itself is not underweight" <|
            \_ ->
                ( bornUnderweightByBirthWeight (Just (WeightInGrm 2499))
                , bornUnderweightByBirthWeight (Just (WeightInGrm 2500))
                )
                    |> Expect.equal ( Just True, Just False )
        , test "the smallest weight that can be read is answered" <|
            \_ ->
                bornUnderweightByBirthWeight (Just (WeightInGrm 300))
                    |> Expect.equal (Just True)
        , test "a weight too small to read is not an answer" <|
            -- Kilograms typed into a grams field, and values that cannot be
            -- interpreted at all. Answering False here would count them as
            -- children known not to be underweight.
            \_ ->
                ( bornUnderweightByBirthWeight (Just (WeightInGrm 299))
                , bornUnderweightByBirthWeight (Just (WeightInGrm 3.2))
                )
                    |> Expect.equal ( Nothing, Nothing )
        , test "a weight that was never taken is not an answer" <|
            \_ ->
                bornUnderweightByBirthWeight Nothing
                    |> Expect.equal Nothing
        ]


birthWeightOutsideConstraintsTest : Test
birthWeightOutsideConstraintsTest =
    -- Birth weight is stored in grams, but is often typed in kilograms. The
    -- kilogram value lands inside the range a weight in grams cannot be, which
    -- is what this reports so that the form can say so and refuse to save.
    describe "birthWeightOutsideConstraints"
        [ test "an ordinary birth weight in grams is inside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 3000))
                    |> Expect.equal False
        , test "a genuinely low birth weight is still inside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 1200))
                    |> Expect.equal False
        , test "the same weight typed in kilograms is outside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 3))
                    |> Expect.equal True
        , test "a weight far above what a newborn can be is outside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 350022))
                    |> Expect.equal True
        , test "the extremes that do occur are accepted" <|
            -- Babies have survived under 500g, and a very large baby can be
            -- over 6000g. Both have to be recordable as they are.
            \_ ->
                ( birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 300))
                , birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 7000))
                )
                    |> Expect.equal ( False, False )
        , test "just outside either end is refused" <|
            \_ ->
                ( birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 299))
                , birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 7001))
                )
                    |> Expect.equal ( True, True )
        , test "a weight that has not been entered is not reported" <|
            -- Whether the measurement still has to be taken is answered by the
            -- task count, not here.
            \_ ->
                birthWeightOutsideConstraints SiteRwanda Nothing
                    |> Expect.equal False
        ]


{-| A saved creatinine test with both results filled in, so the tests can check
what happens when the nurse clears one and re-opens the recurrent encounter.
-}
savedCreatinineTest : CreatinineTestValue
savedCreatinineTest =
    { executionNote = TestNoteRunToday
    , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
    , creatinineResult = Just 1.2
    , bunResult = Just 15
    }


creatinineResultFormWithDefaultTest : Test
creatinineResultFormWithDefaultTest =
    let
        resolve form =
            creatinineResultFormWithDefault form (Just savedCreatinineTest)
    in
    describe "creatinineResultFormWithDefault"
        [ test "a cleared creatinine result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyCreatinineResultForm | creatinineResultDirty = True }
                    |> .creatinineResult
                    |> Expect.equal Nothing
        , test "an untouched creatinine result loads from the saved value" <|
            \_ ->
                resolve emptyCreatinineResultForm
                    |> .creatinineResult
                    |> Expect.equal (Just 1.2)
        , test "an edited creatinine result wins over the saved value" <|
            \_ ->
                resolve { emptyCreatinineResultForm | creatinineResultDirty = True, creatinineResult = Just 2.5 }
                    |> .creatinineResult
                    |> Expect.equal (Just 2.5)
        , test "a cleared BUN result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyCreatinineResultForm | bunResultDirty = True }
                    |> .bunResult
                    |> Expect.equal Nothing
        ]


{-| A saved liver function test with both results filled in, for the same
clear-then-reopen check.
-}
savedLiverFunctionTest : LiverFunctionTestValue
savedLiverFunctionTest =
    { executionNote = TestNoteRunToday
    , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
    , altResult = Just 30
    , astResult = Just 25
    }


{-| The lab technician reopens a partner HIV test they declined because the
partner was not present.
-}
partnerHIVResultFormNotPresentTest : Test
partnerHIVResultFormNotPresentTest =
    let
        saved : PartnerHIVTestValue
        saved =
            { executionNote = TestNoteNotPresent
            , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
            , testPrerequisites = Nothing
            , testResult = Nothing
            , hivSigns = Nothing
            }
    in
    describe "partnerHIVResultFormWithDefault, on a partner who was not present"
        [ test "shows the test will not be performed today" <|
            \_ ->
                partnerHIVResultFormWithDefault emptyPartnerHIVResultForm (Just saved)
                    |> .runConfirmedByLabTech
                    |> Expect.equal (Just False)
        ]


liverFunctionResultFormWithDefaultTest : Test
liverFunctionResultFormWithDefaultTest =
    let
        resolve form =
            liverFunctionResultFormWithDefault form (Just savedLiverFunctionTest)
    in
    describe "liverFunctionResultFormWithDefault"
        [ test "a cleared ALT result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyLiverFunctionResultForm | altResultDirty = True }
                    |> .altResult
                    |> Expect.equal Nothing
        , test "an untouched ALT result loads from the saved value" <|
            \_ ->
                resolve emptyLiverFunctionResultForm
                    |> .altResult
                    |> Expect.equal (Just 30)
        , test "a cleared AST result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyLiverFunctionResultForm | astResultDirty = True }
                    |> .astResult
                    |> Expect.equal Nothing
        ]


{-| A malaria test the nurse did not run as an RDT, ordering a blood smear at
the lab instead. The execution note says why the RDT was not run; the pending
marker says the smear is still owed a result.
-}
orderedBloodSmear : MalariaTestValue
orderedBloodSmear =
    { executionNote = TestNoteLackOfReagents
    , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
    , testPrerequisites = Just EverySet.empty
    , testResult = Nothing
    , bloodSmearResult = BloodSmearPendingInput
    , bloodSmearOrdered = True
    }


{-| A malaria test the nurse ran as a rapid test and sent to the lab to be
read. No smear was taken, so the lab is owed a rapid test result.
-}
rapidTestAtLab : MalariaTestValue
rapidTestAtLab =
    { orderedBloodSmear
        | executionNote = TestNoteRunToday
        , bloodSmearResult = BloodSmearNotTaken
        , bloodSmearOrdered = False
    }


malariaResultFormWithDefaultTest : Test
malariaResultFormWithDefaultTest =
    let
        resolve value =
            malariaResultFormWithDefault emptyMalariaResultForm (Just value)
    in
    describe "malariaResultFormWithDefault"
        [ test "an ordered blood smear leaves the lab technician's confirmation unanswered" <|
            \_ ->
                resolve orderedBloodSmear
                    |> .runConfirmedByLabTech
                    |> Expect.equal Nothing
        , test "an ordered blood smear is a smear, so the smear result is asked for" <|
            \_ ->
                resolve orderedBloodSmear
                    |> .bloodSmearTaken
                    |> Expect.equal True
        , test "an ordered blood smear carries no result yet" <|
            \_ ->
                resolve orderedBloodSmear
                    |> .bloodSmearResult
                    |> Expect.equal Nothing
        , test "a smear the lab technician cancelled keeps their No" <|
            \_ ->
                resolve { orderedBloodSmear | bloodSmearResult = BloodSmearNotTaken }
                    |> .runConfirmedByLabTech
                    |> Expect.equal (Just False)
        , test "a smear the lab technician ran keeps their Yes" <|
            \_ ->
                resolve
                    { orderedBloodSmear
                        | executionNote = TestNoteRunConfirmedByLabTech
                        , bloodSmearResult = BloodSmearNegative
                    }
                    |> .runConfirmedByLabTech
                    |> Expect.equal (Just True)
        ]


{-| Whether the form asks about the blood smear or about the rapid test.

The lab declining a test leaves no result and overwrites the execution note
with the lab technician's own reason, so after a decline a smear and a rapid
test look the same in both. What tells them apart is whether a smear was
ordered, which is recorded once by the nurse and kept.

-}
malariaResultFormBloodSmearTakenTest : Test
malariaResultFormBloodSmearTakenTest =
    let
        resolve value =
            malariaResultFormWithDefault emptyMalariaResultForm (Just value)
                |> .bloodSmearTaken
    in
    describe "malariaResultFormWithDefault, asking about the right test"
        [ test "a smear the lab did not run is still a smear" <|
            \_ ->
                resolve
                    { orderedBloodSmear
                        | executionNote = TestNoteBrokenEquipment
                        , bloodSmearResult = BloodSmearNotTaken
                    }
                    |> Expect.equal True
        , test "a smear the lab read is a smear" <|
            \_ ->
                resolve
                    { orderedBloodSmear
                        | executionNote = TestNoteRunConfirmedByLabTech
                        , bloodSmearResult = BloodSmearNegative
                    }
                    |> Expect.equal True
        , test "a rapid test sent to the lab is not a smear" <|
            \_ ->
                resolve rapidTestAtLab
                    |> Expect.equal False
        , test "a rapid test the lab did not run is still not a smear" <|
            \_ ->
                resolve { rapidTestAtLab | executionNote = TestNoteBrokenEquipment }
                    |> Expect.equal False
        , test "a smear ordered before the record said so is still a smear" <|
            \_ ->
                resolve { orderedBloodSmear | bloodSmearOrdered = False }
                    |> Expect.equal True
        , -- The nurse's own Laboratory form reads the same record, and has to
          -- reach the same answer, or a save from it writes the order away.
          test "the nurse's form agrees about a smear ordered before the record said so" <|
            \_ ->
                malariaTestFormWithDefault emptyMalariaTestForm
                    (Just { orderedBloodSmear | bloodSmearOrdered = False })
                    |> .bloodSmearTaken
                    |> Expect.equal (Just True)
        , test "the nurse's form agrees a rapid test sent to the lab is not a smear" <|
            \_ ->
                malariaTestFormWithDefault emptyMalariaTestForm (Just rapidTestAtLab)
                    |> .bloodSmearTaken
                    |> Expect.equal (Just False)
        ]


{-| What the result select starts on. Neither the pending marker nor "not
taken" is a result the smear scale offers, so carrying either into the form
would show its first option, "Negative", as though it had been chosen.
-}
malariaResultFormSmearResultTest : Test
malariaResultFormSmearResultTest =
    let
        resolve value =
            malariaResultFormWithDefault emptyMalariaResultForm (Just value)
                |> .bloodSmearResult
    in
    describe "malariaResultFormWithDefault, the smear result it starts on"
        [ test "a smear still awaited by the lab is unanswered" <|
            \_ ->
                resolve orderedBloodSmear
                    |> Expect.equal Nothing
        , test "a smear the lab did not run is unanswered" <|
            \_ ->
                resolve
                    { orderedBloodSmear
                        | executionNote = TestNoteBrokenEquipment
                        , bloodSmearResult = BloodSmearNotTaken
                    }
                    |> Expect.equal Nothing
        , test "a smear the lab read shows what they read" <|
            \_ ->
                resolve
                    { orderedBloodSmear
                        | executionNote = TestNoteRunConfirmedByLabTech
                        , bloodSmearResult = BloodSmearPlus
                    }
                    |> Expect.equal (Just BloodSmearPlus)
        ]


{-| A rapid test result on a record that ordered a smear is one nobody entered
for it, and it is read back as a rapid test that was run. Saving the smear
clears it.
-}
toMalariaResultValueTestResultTest : Test
toMalariaResultValueTestResultTest =
    let
        saveWithResult saved =
            toMalariaResultValueWithDefault (Just saved)
                { emptyMalariaResultForm
                    | runConfirmedByLabTech = Just True
                    , executionNote = Just TestNoteRunConfirmedByLabTech
                    , executionNoteDirty = True
                    , testResult = Just TestPositive
                    , testResultDirty = True
                }
                |> Maybe.map .testResult
    in
    describe "toMalariaResultValue, the rapid test result it keeps"
        [ test "saving a smear keeps no rapid test result" <|
            \_ ->
                saveWithResult orderedBloodSmear
                    |> Expect.equal (Just Nothing)
        , test "saving a rapid test sent to the lab keeps its result" <|
            \_ ->
                saveWithResult rapidTestAtLab
                    |> Expect.equal (Just (Just TestPositive))
        ]


{-| The record keeps saying what was ordered through everything the lab does
with it - a decline included - so that reopening it asks about the same test.
-}
bloodSmearOrderedTest : Test
bloodSmearOrderedTest =
    let
        declined saved =
            toMalariaResultValueWithDefault (Just saved)
                { emptyMalariaResultForm
                    | runConfirmedByLabTech = Just False
                    , executionNote = Just TestNoteBrokenEquipment
                    , executionNoteDirty = True
                }
                |> Maybe.map .bloodSmearOrdered

        ordered bloodSmearTaken =
            toMalariaTestValueWithDefault Nothing
                { emptyMalariaTestForm
                    | testPerformed = Just False
                    , executionNote = Just TestNoteLackOfReagents
                    , bloodSmearTaken = Just bloodSmearTaken
                    , immediateResult = Just False
                }
                |> Maybe.map .bloodSmearOrdered
    in
    describe "bloodSmearOrdered"
        [ test "the nurse ordering a smear records it" <|
            \_ ->
                ordered True
                    |> Expect.equal (Just True)
        , test "the nurse declining the rapid test without a smear records none" <|
            \_ ->
                ordered False
                    |> Expect.equal (Just False)
        , test "the lab declining an ordered smear keeps it ordered" <|
            \_ ->
                declined orderedBloodSmear
                    |> Expect.equal (Just True)
        , -- The lab confirming the run makes the nurse's form read the rapid
          -- test as performed again; a save from it must not drop the order.
          test "a nurse re-save after the lab read the smear keeps it ordered" <|
            \_ ->
                toMalariaTestValueWithDefault
                    (Just
                        { orderedBloodSmear
                            | executionNote = TestNoteRunConfirmedByLabTech
                            , bloodSmearResult = BloodSmearNegative
                        }
                    )
                    emptyMalariaTestForm
                    |> Maybe.map .bloodSmearOrdered
                    |> Expect.equal (Just True)
        , test "the lab declining a rapid test does not invent an order" <|
            \_ ->
                declined
                    rapidTestAtLab
                    |> Expect.equal (Just False)
        ]


generateIndividualNutritionAssessmentEntriesTest : Test
generateIndividualNutritionAssessmentEntriesTest =
    let
        date =
            Date.fromCalendarDate 2026 Oct 1

        entriesForSignsTapped signs =
            generateIndividualNutritionAssessmentEntries
                [ { nutrition =
                        Just
                            ( ()
                            , { dateMeasured = date
                              , value =
                                    { signs = EverySet.fromList signs
                                    , assesment = EverySet.singleton (AssesmentMalnutritionSigns signs)
                                    }
                              }
                            )
                  , followUp =
                        Just
                            ( ()
                            , { dateMeasured = date
                              , value =
                                    { options = EverySet.empty
                                    , resolutionDate = Nothing
                                    , assesment = EverySet.singleton (AssesmentMalnutritionSigns [ Apathy, Edema ])
                                    }
                              }
                            )
                  }
                ]

        expected =
            [ ( date, [ AssesmentMalnutritionSigns [ Apathy, Edema ] ] ) ]
    in
    describe "generateIndividualNutritionAssessmentEntries"
        [ test "signs tapped in sorted order give one malnutrition signs entry" <|
            \_ ->
                entriesForSignsTapped [ Apathy, Edema ]
                    |> Expect.equal expected
        , test "signs tapped in reverse order give one malnutrition signs entry" <|
            \_ ->
                entriesForSignsTapped [ Edema, Apathy ]
                    |> Expect.equal expected
        ]


{-| A child should have one Child Scoreboard participant, but some have two.
A dose recorded under either one must count, or it is offered again.
-}
vaccinationProgressAcrossDuplicateParticipantsTest : Test
vaccinationProgressAcrossDuplicateParticipantsTest =
    let
        childId : PersonId
        childId =
            toEntityUuid "child"

        -- The first OPV dose was given under one participant, the second
        -- under the other.
        doses : List ( IndividualEncounterParticipantId, VaccineDose, Date )
        doses =
            [ ( toEntityUuid "participant-a", VaccineDoseFirst, Date.fromCalendarDate 2020 Jan 10 )
            , ( toEntityUuid "participant-b", VaccineDoseSecond, Date.fromCalendarDate 2020 Feb 10 )
            ]

        encounterId participantId =
            toEntityUuid ("encounter-" ++ fromEntityUuid participantId)

        measurementsWithOPV participantId dose date =
            { ncda = Nothing
            , bcgImmunisation = Nothing
            , dtpImmunisation = Nothing
            , dtpStandaloneImmunisation = Nothing
            , ipvImmunisation = Nothing
            , mrImmunisation = Nothing
            , opvImmunisation =
                Just
                    ( toEntityUuid ("opv-" ++ fromEntityUuid participantId)
                    , { dateMeasured = date
                      , nurse = Nothing
                      , healthCenter = Nothing
                      , participantId = childId
                      , deleted = False
                      , encounterId = Just (encounterId participantId)
                      , value =
                            { administeredDoses = EverySet.singleton dose
                            , administrationDates = EverySet.singleton date
                            , administrationNote = AdministeredToday
                            }
                      }
                    )
            , pcv13Immunisation = Nothing
            , rotarixImmunisation = Nothing
            }

        db : ModelIndexedDb
        db =
            { emptyModelIndexedDb
                | individualParticipantsByPerson =
                    Dict.singleton childId
                        (Success <|
                            Dict.fromList <|
                                List.map
                                    (\( participantId, _, date ) ->
                                        ( participantId
                                        , emptyIndividualEncounterParticipant date childId ChildScoreboardEncounter (toEntityUuid "health-center")
                                        )
                                    )
                                    doses
                        )
                , childScoreboardEncountersByParticipant =
                    Dict.fromList <|
                        List.map
                            (\( participantId, _, date ) ->
                                ( participantId
                                , Success <| Dict.singleton (encounterId participantId) (emptyChildScoreboardEncounter participantId date Nothing)
                                )
                            )
                            doses
                , childScoreboardMeasurements =
                    Dict.fromList <|
                        List.map
                            (\( participantId, dose, date ) ->
                                ( encounterId participantId, Success <| measurementsWithOPV participantId dose date )
                            )
                            doses
            }
    in
    describe "Child Scoreboard doses of a child with two participants"
        [ test "both OPV doses count, whichever participant holds them" <|
            \_ ->
                generateVaccinationProgressDictByChildScoreboard SiteRwanda db childId
                    |> Dict.get VaccineOPV
                    |> Maybe.map Dict.toList
                    |> Expect.equal
                        (Just
                            [ ( VaccineDoseFirst, Date.fromCalendarDate 2020 Jan 10 )
                            , ( VaccineDoseSecond, Date.fromCalendarDate 2020 Feb 10 )
                            ]
                        )
        , test "a child with no Child Scoreboard participant has no doses" <|
            \_ ->
                generateVaccinationProgressDictByChildScoreboard SiteRwanda emptyModelIndexedDb childId
                    |> Expect.equal Dict.empty
        ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ nextVaccinationDataForVaccineTest
        , generateIndividualNutritionAssessmentEntriesTest
        , viewChildFormsTest
        , viewMotherFormsTest
        , viewColorAlertIndicationTest
        , getIntervalForVaccineTest
        , getAllDosesForVaccineTest
        , initialVaccinationDateByBirthDateTest
        , mergeVaccinationProgressDictsTest
        , vaccinationProgressAcrossDuplicateParticipantsTest
        , updateChildSetMuacTest
        , ncdaFormWithDefaultNotTakenTest
        , bloodGlucoseOutOfRangeTest
        , outOfRangeAsEnteredTest
        , updateChildOutOfRangePopupTest
        , ncdaMeasurementsOutOfRangeTest
        , ncdaTreatmentAnswerSavedTest
        , showNCDAMeasurementOutOfRangeTest
        , birthWeightOutsideConstraintsTest
        , bornUnderweightByBirthWeightTest
        , heightFormWithDefaultSkippedTest
        , creatinineResultFormWithDefaultTest
        , liverFunctionResultFormWithDefaultTest
        , partnerHIVResultFormNotPresentTest
        , malariaResultFormWithDefaultTest
        , malariaResultFormBloodSmearTakenTest
        , malariaResultFormSmearResultTest
        , toMalariaResultValueTestResultTest
        , bloodSmearOrderedTest
        , knownAsPositiveSavesNoPrerequisitesTest
        , knownAsPositiveUpdateTest
        , withdrawnTestResultTest
        , ancVisitDateForMonthTest
        , ncdaAncMonthTapsTest
        , ncdaAncMonthsViewTest
        , referralWhyNotTest
        , healthEducationWhyNotTest
        , ongoingTreatmentReviewFollowUpTest
        ]


{-| The range check asks the form the nurse is looking at, which is why it does
not have to ask separately whether the measurement could be taken: a form that
was skipped holds no height to be out of range.
-}
heightFormWithDefaultSkippedTest : Test
heightFormWithDefaultSkippedTest =
    describe "heightFormWithDefault, on a measurement that could not be taken"
        [ test "holds no height when the encounter says the form was skipped" <|
            \_ ->
                heightFormWithDefault (EverySet.singleton SkippedHeight)
                    emptyHeightForm
                    (Just (HeightInCm 1050))
                    |> Expect.equal
                        { height = Nothing
                        , heightDirty = False
                        , measurementNotTaken = Just True
                        }
        , test "holds no height when the nurse said so on the form" <|
            \_ ->
                heightFormWithDefault EverySet.empty
                    { emptyHeightForm | height = Just 1050, measurementNotTaken = Just True }
                    Nothing
                    |> Expect.equal
                        { height = Nothing
                        , heightDirty = False
                        , measurementNotTaken = Just True
                        }
        ]


{-| Blood glucose is entered in milligrams per decilitre. A reading typed in
millimoles per litre is about eighteen times smaller - 12.5 mmol/L is 225
mg/dL - which is what the bottom of the range is there to catch. A reading the
form is not showing is left alone, because it cannot be corrected.
-}
bloodGlucoseOutOfRangeTest : Test
bloodGlucoseOutOfRangeTest =
    describe "bloodGlucoseOutOfRange"
        [ test "a reading typed in millimoles per litre is outside the range" <|
            \_ ->
                bloodGlucoseOutOfRange True (Just 12.5)
                    |> Expect.equal [ MeasurementBloodGlucose ]
        , test "the same reading in milligrams per decilitre is within it" <|
            \_ ->
                bloodGlucoseOutOfRange True (Just 225)
                    |> Expect.equal []
        , test "a laboratory result above what a meter can show is within it" <|
            \_ ->
                bloodGlucoseOutOfRange True (Just 900)
                    |> Expect.equal []
        , test "a reading above what a laboratory reports is outside it" <|
            \_ ->
                bloodGlucoseOutOfRange True (Just 1201)
                    |> Expect.equal [ MeasurementBloodGlucose ]
        , test "both ends of the range are within it" <|
            \_ ->
                ( bloodGlucoseOutOfRange True (Just 20)
                , bloodGlucoseOutOfRange True (Just 1200)
                )
                    |> Expect.equal ( [], [] )
        , test "a reading the form is not showing is not asked about" <|
            \_ ->
                bloodGlucoseOutOfRange False (Just 12.5)
                    |> Expect.equal []
        , test "no reading is not out of range" <|
            \_ ->
                bloodGlucoseOutOfRange True Nothing
                    |> Expect.equal []
        ]


{-| The group session forms hold what the nurse typed, in the unit they show:
millimetres for MUAC at Burundi, centimetres elsewhere. The range they are
compared against is the one shown above the input, so the value is compared as
it is - converting here as well would report every MUAC at Burundi.
-}
outOfRangeAsEnteredTest : Test
outOfRangeAsEnteredTest =
    describe "outOfRangeAsEntered"
        [ test "Burundi: 125 mm is within the range shown there" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteBurundi) MeasurementMuac 125
                    |> Expect.equal []
        , test "Rwanda: 125 is outside the range shown there, being millimetres" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteRwanda) MeasurementMuac 125
                    |> Expect.equal [ MeasurementMuac ]
        , test "Rwanda: 12.5 cm is within the range shown there" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteRwanda) MeasurementMuac 12.5
                    |> Expect.equal []
        , test "Burundi: 12.5 is outside the range shown there, being centimetres" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteBurundi) MeasurementMuac 12.5
                    |> Expect.equal [ MeasurementMuac ]
        , test "a height of 1050 is outside its range, which is the same everywhere" <|
            \_ ->
                outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 1050
                    |> Expect.equal [ MeasurementHeight ]
        , test "a weight of 850 is outside its range" <|
            \_ ->
                outOfRangeAsEntered getInputConstraintsWeight MeasurementWeight 850
                    |> Expect.equal [ MeasurementWeight ]
        , test "both ends of a range are within it" <|
            \_ ->
                ( outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 25
                , outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 250
                )
                    |> Expect.equal ( [], [] )
        ]


{-| What the group session form asks for is named on a warning until the nurse
closes it.
-}
updateChildOutOfRangePopupTest : Test
updateChildOutOfRangePopupTest =
    describe "updateChild SetMeasurementOutOfRangePopupState"
        [ test "names the measurement the form asked for" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteRwanda
                            (SetMeasurementOutOfRangePopupState [ MeasurementMuac ])
                            emptyModelChild
                in
                model.measurementOutOfRangePopupState
                    |> Expect.equal [ MeasurementMuac ]
        , test "closing it names nothing, and saves nothing on the way" <|
            \_ ->
                let
                    ( model, _, outMsg ) =
                        updateChild SiteRwanda
                            (SetMeasurementOutOfRangePopupState [])
                            { emptyModelChild | measurementOutOfRangePopupState = [ MeasurementWeight ] }
                in
                ( model.measurementOutOfRangePopupState, outMsg )
                    |> Expect.equal ( [], Nothing )
        ]


{-| The Child Scorecard is filled over several steps and saved only at the end,
so what the form holds between them is what gets saved. Ticking "measurement
not taken" empties the input and hides it; the measurement saved at an earlier
encounter must not come back in its place, or it is live behind a box saying it
was not taken, and saved again at the end.
-}
ncdaFormWithDefaultNotTakenTest : Test
ncdaFormWithDefaultNotTakenTest =
    let
        saved =
            { signs = EverySet.empty
            , birthWeight = Nothing
            , ancVisitsDates = EverySet.empty
            , receivesVitaminA = Nothing
            , stuntingLevel = Just LevelYellow
            , weight = Just (WeightInKg 12)
            , muac = Just (MuacInCm 14)
            }

        emptyForm =
            emptyNCDAData.form

        hydrate form =
            ncdaFormWithDefault form (Just saved)
    in
    describe "ncdaFormWithDefault, on a measurement said not to have been taken"
        [ test "the weight saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | weightNotTaken = Just True }
                    |> .weight
                    |> Expect.equal Nothing
        , test "the MUAC saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | muacNotTaken = Just True }
                    |> .muac
                    |> Expect.equal Nothing
        , test "the stunting level saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | stuntingLevelNotTaken = Just True }
                    |> .stuntingLevel
                    |> Expect.equal Nothing
        , test "saying one was not taken leaves the others alone" <|
            \_ ->
                let
                    form =
                        hydrate { emptyForm | weightNotTaken = Just True }
                in
                ( form.weight, form.muac, form.stuntingLevel )
                    |> Expect.equal ( Nothing, Just (MuacInCm 14), Just LevelYellow )
        , test "a form nobody has touched still inherits what was saved" <|
            \_ ->
                let
                    form =
                        hydrate emptyForm
                in
                ( form.weight, form.muac, form.stuntingLevel )
                    |> Expect.equal ( Just (WeightInKg 12), Just (MuacInCm 14), Just LevelYellow )
        , test "what the nurse has typed is kept over what was saved" <|
            \_ ->
                hydrate { emptyForm | weight = Just (WeightInKg 13) }
                    |> .weight
                    |> Expect.equal (Just (WeightInKg 13))
        ]


{-| A test sent to the lab, then corrected to known as positive or to not
performed. The form reads the old "immediate result" answer back from the saved
run, but the saved record must not claim a run.
-}
knownAsPositiveSavesNoPrerequisitesTest : Test
knownAsPositiveSavesNoPrerequisitesTest =
    let
        sentToLab =
            Just (EverySet.singleton NoTestPrerequisites)

        savedHIVTest =
            { executionNote = TestNoteRunToday
            , executionDate = Nothing
            , testPrerequisites = sentToLab
            , testResult = Nothing
            , hivSigns = Nothing
            }
    in
    describe "a test corrected to one not run saves no prerequisites"
        [ test "HIV test" <|
            \_ ->
                knownAsPositiveUpdateHIVTest True emptyHIVTestUniversalForm
                    |> toHIVTestValueUniversalWithDefault (Just savedHIVTest)
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        , test "partner HIV test" <|
            \_ ->
                knownAsPositiveUpdatePartnerHIVTest True emptyPartnerHIVTestForm
                    |> toPartnerHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        , test "Hepatitis B test" <|
            \_ ->
                knownAsPositiveUpdate True emptyHepatitisBTestForm
                    |> toHepatitisBTestValueWithDefault
                        (Just
                            { executionNote = TestNoteRunToday
                            , executionDate = Nothing
                            , testPrerequisites = sentToLab
                            , testResult = Nothing
                            , originatingEncounter = Nothing
                            }
                        )
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        , test "NCD HIV test" <|
            \_ ->
                rdtKnownAsPositiveUpdate True emptyHIVTestForm
                    |> toHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        , test "a run sent to the lab, then not performed with a reason, saves no prerequisites" <|
            \_ ->
                { emptyPartnerHIVTestForm
                    | testPerformed = Just False
                    , testPerformedDirty = True
                    , executionNote = Just TestNoteLackOfReagents
                    , executionNoteDirty = True
                }
                    |> toPartnerHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        , test "a run sent to the lab still saves its prerequisites" <|
            \_ ->
                emptyPartnerHIVTestForm
                    |> toPartnerHIVTestValueWithDefault (Just savedHIVTest)
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just sentToLab)
        , test "Prenatal random blood sugar test, not performed with a reason" <|
            \_ ->
                { emptyRandomBloodSugarTestUniversalForm
                    | testPerformed = Just False
                    , testPerformedDirty = True
                    , executionNote = Just TestNoteNotIndicated
                    , executionNoteDirty = True
                }
                    |> toRandomBloodSugarTestValueUniversalWithDefault
                        (Just
                            { executionNote = TestNoteRunToday
                            , executionDate = Nothing
                            , testPrerequisites = sentToLab
                            , sugarCount = Nothing
                            , originatingEncounter = Nothing
                            }
                        )
                    |> Maybe.map .testPrerequisites
                    |> Expect.equal (Just Nothing)
        ]


{-| The questions that follow a test result are only asked while the answer to
"known as positive" is No: the patient's own result and the questions under it
on the HIV test, and the result and ARV questions on the partner's test. Saying
Yes withdraws them, so what they hold is cleared, and cleared as dirty, rather
than read back from the saved value as a result the patient never had.

An answer that repeats what the form already shows is not a correction, and a
bool input fires on every tap, so it has to leave the entry alone.

-}
knownAsPositiveUpdateTest : Test
knownAsPositiveUpdateTest =
    let
        answeredHIVTest =
            { emptyHIVTestUniversalForm
                | knownAsPositive = Just False
                , testPerformed = Just True
                , immediateResult = Just True
                , executionNote = Just TestNoteRunToday
                , testResult = Just TestNegative
                , hivProgramHC = Just False
                , partnerHIVPositive = Just True
                , partnerTakingARV = Just False
                , partnerSurpressedViralLoad = Just False
            }

        knownPositiveHIVTest =
            { emptyHIVTestUniversalForm
                | knownAsPositive = Just True
                , executionNote = Just TestNoteKnownAsPositive
            }

        answeredPartnerHIVTest =
            { emptyPartnerHIVTestForm
                | knownAsPositive = Just False
                , testPerformed = Just True
                , executionNote = Just TestNoteRunToday
                , testResult = Just TestPositive
                , partnerTakingARV = Just True
                , partnerSurpressedViralLoad = Just True
            }
    in
    describe "known as positive withdraws the answers that depend on the test"
        [ test "HIV test: the result and every partner question are cleared" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdateHIVTest True answeredHIVTest
                in
                ( updated.testResult
                , [ updated.hivProgramHC
                  , updated.partnerHIVPositive
                  , updated.partnerTakingARV
                  , updated.partnerSurpressedViralLoad
                  ]
                )
                    |> Expect.equal ( Nothing, [ Nothing, Nothing, Nothing, Nothing ] )
        , test "HIV test: every cleared field is marked dirty, so the saved value is not read back over it" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdateHIVTest True answeredHIVTest
                in
                [ updated.testResultDirty
                , updated.hivProgramHCDirty
                , updated.partnerHIVPositiveDirty
                , updated.partnerTakingARVDirty
                , updated.partnerSurpressedViralLoadDirty
                ]
                    |> Expect.equal [ True, True, True, True, True ]
        , test "HIV test: the execution note records that the patient is known as positive" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdateHIVTest True answeredHIVTest
                in
                ( updated.knownAsPositive, updated.executionNote, updated.testPerformed )
                    |> Expect.equal ( Just True, Just TestNoteKnownAsPositive, Nothing )
        , test "HIV test: correcting back to No leaves nothing claiming a test was run" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdateHIVTest False knownPositiveHIVTest
                in
                ( updated.knownAsPositive, updated.executionNote, updated.testPerformed )
                    |> Expect.equal ( Just False, Nothing, Nothing )
        , test "HIV test: re-tapping the No already chosen leaves the entry untouched" <|
            \_ ->
                knownAsPositiveUpdateHIVTest False answeredHIVTest
                    |> Expect.equal answeredHIVTest
        , test "HIV test: re-tapping the Yes already chosen leaves the entry untouched" <|
            \_ ->
                knownAsPositiveUpdateHIVTest True knownPositiveHIVTest
                    |> Expect.equal knownPositiveHIVTest
        , test "partner HIV test: the result and the ARV questions are cleared" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdatePartnerHIVTest True answeredPartnerHIVTest
                in
                ( updated.testResult, updated.partnerTakingARV, updated.partnerSurpressedViralLoad )
                    |> Expect.equal ( Nothing, Nothing, Nothing )
        , test "partner HIV test: every cleared field is marked dirty" <|
            \_ ->
                let
                    updated =
                        knownAsPositiveUpdatePartnerHIVTest True answeredPartnerHIVTest
                in
                [ updated.testResultDirty
                , updated.partnerTakingARVDirty
                , updated.partnerSurpressedViralLoadDirty
                ]
                    |> Expect.equal [ True, True, True ]
        , test "partner HIV test: re-tapping the No already chosen leaves the entry untouched" <|
            \_ ->
                knownAsPositiveUpdatePartnerHIVTest False answeredPartnerHIVTest
                    |> Expect.equal answeredPartnerHIVTest
        ]


{-| The NCD pregnancy and HIV tests keep the result out of sight once the
nurse answers that the test was not performed, or that the patient is known as
positive. A withdrawn run clears its result as dirty, so the saved result is
not read back and saved again unseen; an untouched form still reads it, and a
result the nurse enters wins over it.
-}
withdrawnTestResultTest : Test
withdrawnTestResultTest =
    let
        savedNegative =
            { executionNote = TestNoteRunToday
            , executionDate = Nothing
            , testResult = Just TestNegative
            }

        savedNegativeHIV =
            { executionNote = TestNoteRunToday
            , executionDate = Nothing
            , testPrerequisites = Nothing
            , testResult = Just TestNegative
            , hivSigns = Nothing
            }

        -- The form as the nurse leaves it after changing "performed" to No
        -- on a test she had run, and giving the reason.
        notPerformed form =
            let
                withdrawn =
                    rdtTestPerformedUpdate False (performed Nothing form)
            in
            { withdrawn | executionNote = Just TestNoteLackOfReagents, executionNoteDirty = True }

        -- The form once the test was run and read.
        performed result form =
            { form
                | knownAsPositive = Just False
                , testPerformed = Just True
                , testPerformedDirty = True
                , testPerformedToday = Just True
                , executionNote = Just TestNoteRunToday
                , testResult = result
            }
    in
    describe "a withdrawn test does not save the result it used to have"
        [ test "performed changed to No withdraws the result, as dirty" <|
            \_ ->
                let
                    updated =
                        rdtTestPerformedUpdate False (performed (Just TestPositive) emptyPregnancyTestForm)
                in
                ( updated.testPerformed, updated.testResult, updated.testResultDirty )
                    |> Expect.equal ( Just False, Nothing, True )
        , test "repeating Yes on performed leaves the entered result alone" <|
            \_ ->
                let
                    answered =
                        performed (Just TestPositive) emptyPregnancyTestForm
                in
                rdtTestPerformedUpdate True answered
                    |> Expect.equal answered
        , test "known as positive withdraws the result, as dirty, with the run" <|
            \_ ->
                let
                    updated =
                        rdtKnownAsPositiveUpdate True (performed (Just TestNegative) emptyHIVTestForm)
                in
                ( ( updated.testResult, updated.testResultDirty )
                , ( updated.executionNote, updated.testPerformed, updated.testPerformedToday )
                )
                    |> Expect.equal ( ( Nothing, True ), ( Just TestNoteKnownAsPositive, Nothing, Nothing ) )
        , test "repeating No on known as positive leaves the entered result alone" <|
            \_ ->
                let
                    answered =
                        performed (Just TestNegative) emptyHIVTestForm
                in
                rdtKnownAsPositiveUpdate False answered
                    |> Expect.equal answered
        , test "pregnancy test: the saved result is not read back into the value" <|
            \_ ->
                toPregnancyTestValueWithDefault (Just savedNegative) (notPerformed emptyPregnancyTestForm)
                    |> Maybe.map .testResult
                    |> Expect.equal (Just Nothing)
        , test "pregnancy test: an untouched form still reads the saved result" <|
            \_ ->
                toPregnancyTestValueWithDefault (Just savedNegative) emptyPregnancyTestForm
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestNegative))
        , test "pregnancy test: a result the nurse enters wins over the saved one" <|
            \_ ->
                toPregnancyTestValueWithDefault (Just savedNegative)
                    { emptyPregnancyTestForm | testResult = Just TestPositive, testResultDirty = True }
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestPositive))
        , test "HIV test: the saved result is not read back into the value" <|
            \_ ->
                toHIVTestValueWithDefault (Just savedNegativeHIV) (notPerformed emptyHIVTestForm)
                    |> Maybe.map .testResult
                    |> Expect.equal (Just Nothing)
        , test "HIV test: the follow-up answers given under the result are not saved once the run is withdrawn" <|
            \_ ->
                let
                    answered =
                        performed (Just TestPositive) emptyHIVTestForm
                in
                toHIVTestValueWithDefault (Just savedNegativeHIV)
                    (notPerformed { answered | hivProgramHC = Just True, partnerHIVPositive = Just True })
                    |> Maybe.map .hivSigns
                    |> Expect.equal (Just Nothing)
        , test "HIV test: an untouched form still reads the saved result" <|
            \_ ->
                toHIVTestValueWithDefault (Just savedNegativeHIV) emptyHIVTestForm
                    |> Maybe.map .testResult
                    |> Expect.equal (Just (Just TestNegative))
        ]


{-| NCDA Antenatal Care step: the nine pregnancy months, where the months that
already hold a recorded antenatal visit are shown checked and greyed out.

The pregnancy in these tests starts on 10 January 2025. Its recorded visits
sit in month 2 (in the middle of it), month 4 (on its very first day, 10 April)
and month 6 (on its very last day, 9 July), so a month computed one off on
either side of a recorded visit shows up as a wrong answer. A month is the
number of whole months from the pregnancy start to the date, plus 1.

A picked month N is stored as pregnancy start + (N - 1) months + 14 days.

-}
ancPregnancyStart : NominalDate
ancPregnancyStart =
    Date.fromCalendarDate 2025 Jan 10


{-| What a pick of month N stores: 14 days into that pregnancy month.
-}
ancPickedDate : Int -> NominalDate
ancPickedDate monthNumber =
    Date.add Months (monthNumber - 1) ancPregnancyStart
        |> Date.add Days 14


ancRecordedMonth2 : NominalDate
ancRecordedMonth2 =
    Date.fromCalendarDate 2025 Feb 15


ancRecordedMonth4FirstDay : NominalDate
ancRecordedMonth4FirstDay =
    Date.fromCalendarDate 2025 Apr 10


ancRecordedMonth6LastDay : NominalDate
ancRecordedMonth6LastDay =
    Date.fromCalendarDate 2025 Jul 9


ancRecordedVisits : EverySet NominalDate
ancRecordedVisits =
    EverySet.fromList [ ancRecordedMonth2, ancRecordedMonth4FirstDay, ancRecordedMonth6LastDay ]


ancVisitDateForMonthTest : Test
ancVisitDateForMonthTest =
    let
        tap =
            ancVisitDateForMonth ancPregnancyStart ancRecordedVisits

        monthOf date =
            Date.diff Months ancPregnancyStart date + 1
    in
    describe "ancVisitDateForMonth (what a tap on an NCDA ANC pregnancy month adds)"
        [ describe "A month that already shows a recorded antenatal visit does not change when tapped, and tapping it adds no visit to the questionnaire"
            [ test "a visit recorded in the middle of month 2: tapping month 2 adds no visit" <|
                \_ ->
                    tap 2 |> Expect.equal Nothing
            , test "a visit recorded on the first day of month 4: tapping month 4 adds no visit" <|
                \_ ->
                    tap 4 |> Expect.equal Nothing
            , test "a visit recorded on the last day of month 6: tapping month 6 adds no visit" <|
                \_ ->
                    tap 6 |> Expect.equal Nothing
            , test "two visits recorded in the same month: tapping that month adds no visit" <|
                \_ ->
                    ancVisitDateForMonth ancPregnancyStart
                        (EverySet.fromList [ Date.fromCalendarDate 2025 Mar 12, Date.fromCalendarDate 2025 Apr 2 ])
                        3
                        |> Expect.equal Nothing
            ]
        , describe "A month with no recorded visit still adds its date when tapped"
            [ -- Months 3, 5 and 7 border the recorded visits on the first
              -- day of month 4 and the last day of month 6. A month computed
              -- one off would block one of them.
              test "months 1, 3, 5, 7, 8 and 9, beside the recorded months, each add their date" <|
                \_ ->
                    [ 1, 3, 5, 7, 8, 9 ]
                        |> List.map tap
                        |> Expect.equal (List.map (ancPickedDate >> Just) [ 1, 3, 5, 7, 8, 9 ])
            , test "with no recorded visits at all, every month 1 to 9 adds its date" <|
                \_ ->
                    List.range 1 9
                        |> List.map (ancVisitDateForMonth ancPregnancyStart EverySet.empty)
                        |> Expect.equal (List.map (ancPickedDate >> Just) (List.range 1 9))
            , -- A visit recorded after the ninth month (a late, post-term
              -- encounter) falls in no month on the grid, so it blocks none.
              test "a visit recorded in month 10 blocks none of months 1 to 9" <|
                \_ ->
                    List.range 1 9
                        |> List.map
                            (ancVisitDateForMonth ancPregnancyStart
                                (EverySet.singleton (Date.fromCalendarDate 2025 Oct 13))
                            )
                        |> Expect.equal (List.map (ancPickedDate >> Just) (List.range 1 9))
            ]
        , describe "A picked month N is stored as the date 14 days into that pregnancy month (pregnancy start + N-1 months + 14 days)"
            [ test "month 1 is stored as 24 January 2025" <|
                \_ ->
                    tap 1 |> Expect.equal (Just (Date.fromCalendarDate 2025 Jan 24))
            , test "month 9 is stored as 24 September 2025" <|
                \_ ->
                    tap 9 |> Expect.equal (Just (Date.fromCalendarDate 2025 Sep 24))
            , -- The months are added before the 14 days, so a pregnancy that
              -- starts on 31 January has month 2 start on 28 February, and
              -- month 2 is stored as 14 March.
              test "a pregnancy starting on 31 January stores month 2 as 14 March" <|
                \_ ->
                    ancVisitDateForMonth (Date.fromCalendarDate 2025 Jan 31) EverySet.empty 2
                        |> Expect.equal (Just (Date.fromCalendarDate 2025 Mar 14))
            , -- The stored date must fall in the month that was tapped, or the
              -- grid would show a different month checked than the one picked.
              test "the date stored for each month 1 to 9 falls in that month" <|
                \_ ->
                    List.range 1 9
                        |> List.map (ancVisitDateForMonth ancPregnancyStart EverySet.empty >> Maybe.map monthOf)
                        |> Expect.equal (List.map Just (List.range 1 9))
            ]
        ]


{-| A tap on a month, as the questionnaire handles it: the view turns the tap
into the date `ancVisitDateForMonth` gives, and a date toggles it on the form.
A tap that gives no date sends nothing.

The group session handles the toggle in `updateChild`. Nutrition, Well Child
and Child Scoreboard have their own toggle handlers, but every one of them
receives its date from the same shared NCDA view and its call to
`ancVisitDateForMonth`.

The questionnaire starts after the nurse answered "Yes" to unrecorded ANC
visits, which leaves the form with an empty set of picked months.

-}
ncdaAncMonthTapsTest : Test
ncdaAncMonthTapsTest =
    let
        answeredYes recorded =
            ( recorded
            , updateChild SiteRwanda (SetUpdateANCVisits True) emptyModelChild
                |> (\( model, _, _ ) -> model)
            )

        tapMonth monthNumber ( recorded, model ) =
            ( recorded
            , ancVisitDateForMonth ancPregnancyStart recorded monthNumber
                |> Maybe.map
                    (\date ->
                        updateChild SiteRwanda (ToggleANCVisitDate date) model
                            |> (\( updated, _, _ ) -> updated)
                    )
                |> Maybe.withDefault model
            )

        pickedOnForm ( _, model ) =
            Maybe.withDefault EverySet.empty model.ncdaData.form.ancVisitsDates

        -- The NCDA activity saved before, with every other answer given, so
        -- the save produces a value and the ANC dates come from the form.
        savedBefore =
            { signs = EverySet.empty
            , birthWeight = Nothing
            , ancVisitsDates = EverySet.empty
            , receivesVitaminA = Nothing
            , stuntingLevel = Nothing
            , weight = Nothing
            , muac = Nothing
            }

        savedDates ( _, model ) =
            toNCDAValueWithDefault (Just savedBefore) model.ncdaData.form
                |> Maybe.map .ancVisitsDates

        -- The rule the shared view applies to show the counseling message:
        -- recorded visits plus picked months fewer than 4.
        counselingShown (( recorded, _ ) as state) =
            EverySet.size recorded + EverySet.size (pickedOnForm state) < 4
    in
    describe "NCDA ANC month taps on the questionnaire"
        [ describe "After tapping such months and saving, the questionnaire holds only the months the user picked that had no recorded visit"
            [ test "tapping recorded months 2, 4 and 6 between picks of months 3 and 8 saves only months 3 and 8" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 2
                        |> tapMonth 3
                        |> tapMonth 4
                        |> tapMonth 8
                        |> tapMonth 6
                        |> savedDates
                        |> Expect.equal (Just (EverySet.fromList [ ancPickedDate 3, ancPickedDate 8 ]))
            , test "tapping only recorded months saves no visit dates" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 2
                        |> tapMonth 4
                        |> tapMonth 6
                        |> savedDates
                        |> Expect.equal (Just EverySet.empty)
            ]
        , describe "A month with no recorded visit is picked by a tap and un-picked by a second tap, as today"
            [ test "one tap on month 3 picks it" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 3
                        |> pickedOnForm
                        |> Expect.equal (EverySet.singleton (ancPickedDate 3))
            , test "a second tap on month 3 un-picks it" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 3
                        |> tapMonth 3
                        |> pickedOnForm
                        |> Expect.equal EverySet.empty
            , test "a tap on recorded month 4 between the two taps on month 3 leaves nothing picked" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 3
                        |> tapMonth 4
                        |> tapMonth 3
                        |> pickedOnForm
                        |> Expect.equal EverySet.empty
            , test "with no recorded visits, a tap on month 2 picks it and a second tap un-picks it" <|
                \_ ->
                    let
                        picked =
                            answeredYes EverySet.empty |> tapMonth 2
                    in
                    ( pickedOnForm picked, pickedOnForm (tapMonth 2 picked) )
                        |> Expect.equal ( EverySet.singleton (ancPickedDate 2), EverySet.empty )
            ]
        , describe "The \"fewer than 4 ANC visits\" counseling message shows when recorded visits plus picked months are fewer than 4; tapping a recorded month does not change it"
            [ test "3 recorded visits, nothing picked: the message shows" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> counselingShown
                        |> Expect.equal True
            , test "3 recorded visits, recorded month 4 tapped: the message still shows" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 4
                        |> counselingShown
                        |> Expect.equal True
            , test "3 recorded visits, month 3 picked: the message is gone" <|
                \_ ->
                    answeredYes ancRecordedVisits
                        |> tapMonth 3
                        |> counselingShown
                        |> Expect.equal False
            , -- Visits are counted, not months: two visits recorded in month 3
              -- and one in month 5 are 3 visits, and tapping month 3 adds none.
              test "2 visits recorded in month 3 and 1 in month 5, month 3 tapped: the message still shows" <|
                \_ ->
                    answeredYes
                        (EverySet.fromList
                            [ Date.fromCalendarDate 2025 Mar 12
                            , Date.fromCalendarDate 2025 Apr 2
                            , Date.fromCalendarDate 2025 May 20
                            ]
                        )
                        |> tapMonth 3
                        |> counselingShown
                        |> Expect.equal True
            ]
        ]


{-| The NCDA Antenatal Care step as the shared view renders it. The group
session, Nutrition, Well Child and Child Scoreboard all render this one view,
each passing its own toggle message; the group session's is used here.

The child's mother has a pregnancy whose EDD puts its start on 10 January
2025, with nurse prenatal encounters on the three recorded visit dates. A
postpartum encounter is not an antenatal visit and blocks no month.

-}
ncdaAncMonthsViewTest : Test
ncdaAncMonthsViewTest =
    let
        childId =
            toEntityUuid "ncda-child"

        participantId =
            toEntityUuid "ncda-pregnancy"

        birthDate =
            Date.fromCalendarDate 2025 Oct 17

        pregnancy =
            let
                participant =
                    testParticipant ancPregnancyStart AntenatalEncounter
            in
            { participant | eddDate = Just (lmpToEDDDate ancPregnancyStart) }

        encounters =
            [ ( "enc-1", emptyPrenatalEncounter participantId ancRecordedMonth2 NurseEncounter Nothing )
            , ( "enc-2", emptyPrenatalEncounter participantId ancRecordedMonth4FirstDay NurseEncounter Nothing )
            , ( "enc-3", emptyPrenatalEncounter participantId ancRecordedMonth6LastDay NurseEncounter Nothing )
            , ( "enc-4", emptyPrenatalEncounter participantId birthDate NursePostpartumEncounter Nothing )
            ]
                |> List.map (\( id, encounter ) -> ( toEntityUuid id, encounter ))
                |> Dict.fromList

        db =
            { emptyModelIndexedDb
                | pregnancyByNewborn = Dict.singleton childId (Success (Just ( participantId, pregnancy )))
                , prenatalEncountersByParticipant = Dict.singleton participantId (Success encounters)
            }

        config =
            { atHealthCenter = True
            , showTasksTray = False
            , pregnancySummary = Nothing
            , ncdaNeverFilled = True
            , ncdaNotFilledAfterAgeOfSixMonths = True
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStuntingLevelMsg = SetStuntingLevel
            , setWeightMsg = SetWeight
            , setMuacMsg = SetMuac
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , setMeasurementOutOfRangePopupMsg = SetMeasurementOutOfRangePopup
            , saveMsg = SendOutMsgChild NoOp
            }

        form =
            { emptyNCDAForm
                | step = Just NCDAStepAntenatalCare
                , updateANCVisits = Just True
                , ancVisitsDates = Just EverySet.empty
            }

        rendered =
            viewNCDAContent English
                (Date.fromCalendarDate 2026 Jan 15)
                SiteRwanda
                childId
                (testChild birthDate)
                config
                Nothing
                False
                form
                db
                |> div []
                |> Query.fromHtml

        monthRadio monthNumber =
            rendered
                |> Query.findAll [ class "month-radio" ]
                |> Query.index (monthNumber - 1)

        tapMonth monthNumber =
            monthRadio monthNumber
                |> Query.find [ tag "label" ]
                |> Event.simulate Event.click
    in
    describe "NCDA ANC months, as the shared view renders them"
        [ describe "A month that already shows a recorded antenatal visit does not change when tapped, and tapping it adds no visit to the questionnaire"
            [ test "recorded months 2, 4 and 6 are shown checked and greyed out" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\monthNumber _ ->
                                monthRadio monthNumber
                                    |> Query.find [ tag "input" ]
                                    |> Query.has [ classes [ "checked", "disabled" ] ]
                            )
                            [ 2, 4, 6 ]
                        )
                        ()
            , test "tapping recorded month 2 sends no message" <|
                \_ ->
                    tapMonth 2 |> Event.toResult |> Expect.err
            , test "tapping recorded month 4 sends no message" <|
                \_ ->
                    tapMonth 4 |> Event.toResult |> Expect.err
            , test "tapping recorded month 6 sends no message" <|
                \_ ->
                    tapMonth 6 |> Event.toResult |> Expect.err
            ]
        , describe "A month with no recorded visit is picked by a tap, with the date 14 days into that month"
            [ test "tapping month 3 toggles 24 March 2025" <|
                \_ ->
                    tapMonth 3 |> Event.expect (ToggleANCVisitDate (ancPickedDate 3))
            , test "tapping month 7 toggles 24 July 2025" <|
                \_ ->
                    tapMonth 7 |> Event.expect (ToggleANCVisitDate (ancPickedDate 7))
            , test "month 3 is shown neither checked nor greyed out" <|
                \_ ->
                    monthRadio 3
                        |> Query.find [ tag "input" ]
                        |> Expect.all
                            [ Query.hasNot [ class "checked" ]
                            , Query.hasNot [ class "disabled" ]
                            ]
            ]
        , test "3 recorded visits and nothing picked: the \"fewer than 4 ANC visits\" counseling message shows" <|
            \_ ->
                rendered
                    |> Query.has [ classes [ "label", "counseling" ] ]
        ]



-- YES/NO QUESTIONS WITH A FOLLOW-UP


{-| A task list is complete when every entry holds an answer.
-}
followUpTasksComplete : List (Maybe Bool) -> Bool
followUpTasksComplete =
    List.all ((/=) Nothing)


{-| What the nurse sees of a "Why not?" question in the rendered inputs: whether
its options are on screen at all, how many of them are ticked, and whether the
task list counts as complete.

A stored "no reason" placeholder is not among the options, so it never shows
ticked; what gives the defect away is the task list reading complete while
nothing is ticked.

-}
expectWhyNot : { asked : Bool, ticked : Int, complete : Bool } -> ( List (Html msg), List (Maybe Bool) ) -> Expect.Expectation
expectWhyNot expected ( inputs, tasks ) =
    let
        rendered =
            Query.fromHtml (div [] inputs)

        options =
            Query.findAll [ classes [ "ui", "checkbox", "activity" ] ] rendered
    in
    Expect.all
        [ \_ ->
            if expected.asked then
                Query.count (Expect.atLeast 1) options

            else
                Query.count (Expect.equal 0) options
        , \_ ->
            Query.findAll
                [ classes [ "ui", "checkbox", "activity" ]
                , containing [ tag "input", class "checked" ]
                ]
                rendered
                |> Query.count (Expect.equal expected.ticked)
        , \_ ->
            followUpTasksComplete tasks
                |> Expect.equal expected.complete
        ]
        ()


{-| Referral to a facility asks "Why not?" after a "No". The saved record is
passed as `saved`; the form is the page's own form after the answer handler
ran, which sets the Yes/No answer and clears the reason. A stored "no reason"
is `NoReasonForNonReferral`.
-}
referralWhyNotTest : Test
referralWhyNotTest =
    let
        savedReferred =
            { signs = EverySet.fromList [ HandReferrerForm, ReferToHealthCenter ]
            , reasonForNotSendingToHC = NoReasonForNonReferral
            }

        savedNotReferred : ReasonForNonReferral -> SendToHCValue
        savedNotReferred reason =
            { signs = EverySet.singleton NoSendToHCSigns
            , reasonForNotSendingToHC = reason
            }

        -- The page's form right after "Yes" or "No" is tapped on a reopened record.
        answered value =
            { emptySendToHCForm | referToHealthCenter = Just value, reasonForNotSendingToHC = Nothing }

        reasonChosen reason form =
            { form | reasonForNotSendingToHC = Just reason }

        shown facility saved form =
            sendToHCFormWithDefault form saved
                |> sendToFacilityInputsAndTasks English facility (always ()) (always ()) (always ()) Nothing

        -- The record as comparable data: a set of signs compares unequal when built in another order.
        comparable =
            Maybe.map
                (\value ->
                    ( List.filter (\sign -> EverySet.member sign value.signs)
                        [ EnrollToNutritionProgram, HandReferrerForm, NoSendToHCSigns, PrenatalAccompanyToHC, ReferToHealthCenter, ReferToNutritionProgram ]
                    , value.reasonForNotSendingToHC
                    )
                )

        stored saved form =
            toSendToHCValueWithDefault saved form

        expectRecord expected =
            comparable >> Expect.equal (comparable (Just expected))
    in
    describe "Referral: the \"Why not?\" follow-up when a saved answer is changed"
        [ describe "A referral saved with \"Yes\", reopened and changed to \"No\": \"Why not?\" is unanswered and the task is not complete until a reason is chosen"
            [ test "health centre referral: \"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown FacilityHealthCenter (Just savedReferred) (answered False)
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "hospital referral: \"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown FacilityHospital (Just savedReferred) (answered False)
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "once a reason is chosen it is ticked and the task is complete" <|
                \_ ->
                    shown FacilityHealthCenter (Just savedReferred) (answered False |> reasonChosen ClientRefused)
                        |> expectWhyNot { asked = True, ticked = 1, complete = True }
            , test "the chosen reason is what gets saved, with no referral" <|
                \_ ->
                    stored (Just savedReferred) (answered False |> reasonChosen NoAmbulance)
                        |> Maybe.map
                            (\value ->
                                ( EverySet.member ReferToHealthCenter value.signs
                                , value.reasonForNotSendingToHC
                                )
                            )
                        |> Expect.equal (Just ( False, NoAmbulance ))
            , test "changed to \"No\" and back to \"Yes\": the task is complete and no reason is saved" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown FacilityHealthCenter (Just savedReferred) form
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                        , \form ->
                            stored (Just savedReferred) form
                                |> Maybe.map .reasonForNotSendingToHC
                                |> Expect.equal (Just NoReasonForNonReferral)
                        ]
                        (answered True)
            ]
        , describe "A referral saved with \"No\" and a reason, changed to \"Yes\" and saved: the record holds no reason"
            (List.map
                (\( label, reason ) ->
                    test ("saved reason: " ++ label) <|
                        \_ ->
                            stored (Just (savedNotReferred reason)) (answered True)
                                |> Maybe.map
                                    (\value ->
                                        ( EverySet.member ReferToHealthCenter value.signs
                                        , value.reasonForNotSendingToHC
                                        )
                                    )
                                |> Expect.equal (Just ( True, NoReasonForNonReferral ))
                )
                [ ( "client already in care", ClientAlreadyInCare )
                , ( "client refused", ClientRefused )
                , ( "client unable to afford fees", ClientUnableToAffordFees )
                , ( "no ambulance", NoAmbulance )
                , ( "not indicated", ReasonForNonReferralNotIndicated )
                , ( "other", ReasonForNonReferralOther )
                ]
                ++ [ test "the \"Why not?\" question is no longer asked and the task is complete" <|
                        \_ ->
                            shown FacilityHealthCenter (Just (savedNotReferred ClientRefused)) (answered True)
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                   ]
            )
        , describe "A referral saved with \"No\" and a reason, reopened without changes: the reason is shown selected and the task is complete"
            [ test "the saved reason is ticked and the task is complete" <|
                \_ ->
                    shown FacilityHealthCenter (Just (savedNotReferred ClientRefused)) emptySendToHCForm
                        |> expectWhyNot { asked = True, ticked = 1, complete = True }
            , test "saving it again stores the same record" <|
                \_ ->
                    stored (Just (savedNotReferred ClientRefused)) emptySendToHCForm
                        |> expectRecord (savedNotReferred ClientRefused)
            , test "a different reason chosen replaces the saved one" <|
                \_ ->
                    stored (Just (savedNotReferred ClientRefused)) (reasonChosen NoAmbulance emptySendToHCForm)
                        |> expectRecord (savedNotReferred NoAmbulance)
            ]
        , describe "An older referral saved with \"No\" and no reason, reopened: \"Why not?\" is unanswered and the task is not complete"
            [ test "\"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown FacilityHealthCenter (Just (savedNotReferred NoReasonForNonReferral)) emptySendToHCForm
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "once a reason is chosen the task is complete and the reason is saved" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown FacilityHealthCenter (Just (savedNotReferred NoReasonForNonReferral)) form
                                |> expectWhyNot { asked = True, ticked = 1, complete = True }
                        , \form ->
                            stored (Just (savedNotReferred NoReasonForNonReferral)) form
                                |> expectRecord (savedNotReferred ClientUnableToAffordFees)
                        ]
                        (reasonChosen ClientUnableToAffordFees emptySendToHCForm)
            ]
        , describe "Records the defect does not touch stay as they are"
            [ test "a referral saved with \"Yes\", reopened without changes, is complete and saves the same record" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown FacilityHealthCenter (Just savedReferred) form
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                        , \form ->
                            stored (Just savedReferred) form
                                |> expectRecord savedReferred
                        ]
                        emptySendToHCForm
            , test "a first-time \"No\", with no saved record, leaves \"Why not?\" unanswered and the task not complete" <|
                \_ ->
                    let
                        form =
                            answered False
                    in
                    shown FacilityHealthCenter Nothing { form | handReferralForm = Just False }
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            ]
        ]


{-| Health education asks "Why not?" after a "No". The saved record is passed
as `saved`; the form is the page's own form after the answer handler ran, which
sets the Yes/No answer and clears the reason. A stored "no reason" is
`NoReasonForNotProvidingHealthEducation`.
-}
healthEducationWhyNotTest : Test
healthEducationWhyNotTest =
    let
        savedProvided =
            { signs = EverySet.singleton MalariaPrevention
            , reasonForNotProvidingHealthEducation = NoReasonForNotProvidingHealthEducation
            }

        savedNotProvided : ReasonForNotProvidingHealthEducation -> HealthEducationValue
        savedNotProvided reason =
            { signs = EverySet.singleton NoHealthEducationSigns
            , reasonForNotProvidingHealthEducation = reason
            }

        -- The page's form right after "Yes" or "No" is tapped on a reopened record.
        answered value =
            { emptyHealthEducationForm | educationForDiagnosis = Just value, reasonForNotProvidingHealthEducation = Nothing }

        reasonChosen reason form =
            { form | reasonForNotProvidingHealthEducation = Just reason }

        shown saved form =
            healthEducationFormWithDefault form saved
                |> healthEducationFormInputsAndTasks English (always ()) (always ())

        stored saved form =
            toHealthEducationValueWithDefault saved form
    in
    describe "Health education: the \"Why not?\" follow-up when a saved answer is changed"
        [ describe "Health education saved with \"Yes\", reopened and changed to \"No\": \"Why not?\" is unanswered and the task is not complete until a reason is chosen"
            [ test "\"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown (Just savedProvided) (answered False)
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "once a reason is chosen it is ticked and the task is complete" <|
                \_ ->
                    shown (Just savedProvided) (answered False |> reasonChosen PatientRefused)
                        |> expectWhyNot { asked = True, ticked = 1, complete = True }
            , test "the chosen reason is what gets saved, with no education provided" <|
                \_ ->
                    stored (Just savedProvided) (answered False |> reasonChosen PatientTooIll)
                        |> Expect.equal (Just (savedNotProvided PatientTooIll))
            , test "changed to \"No\" and back to \"Yes\": the task is complete and no reason is saved" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown (Just savedProvided) form
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                        , \form ->
                            stored (Just savedProvided) form
                                |> Expect.equal (Just savedProvided)
                        ]
                        (answered True)
            ]
        , describe "Health education saved with \"No\" and a reason, changed to \"Yes\" and saved: the record holds no reason"
            (List.map
                (\( label, reason ) ->
                    test ("saved reason: " ++ label) <|
                        \_ ->
                            stored (Just (savedNotProvided reason)) (answered True)
                                |> Expect.equal (Just savedProvided)
                )
                [ ( "lack of appropriate education user guide", LackOfAppropriateEducationUserGuide )
                , ( "patient needs emergency referral", PatientNeedsEmergencyReferral )
                , ( "patient refused", PatientRefused )
                , ( "patient too ill", PatientTooIll )
                , ( "received emergency case", ReceivedEmergencyCase )
                ]
                ++ [ test "the \"Why not?\" question is no longer asked and the task is complete" <|
                        \_ ->
                            shown (Just (savedNotProvided PatientRefused)) (answered True)
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                   ]
            )
        , describe "Health education saved with \"No\" and a reason, reopened without changes: the reason is shown selected and the task is complete"
            [ test "the saved reason is ticked and the task is complete" <|
                \_ ->
                    shown (Just (savedNotProvided PatientRefused)) emptyHealthEducationForm
                        |> expectWhyNot { asked = True, ticked = 1, complete = True }
            , test "saving it again stores the same record" <|
                \_ ->
                    stored (Just (savedNotProvided PatientRefused)) emptyHealthEducationForm
                        |> Expect.equal (Just (savedNotProvided PatientRefused))
            , test "a different reason chosen replaces the saved one" <|
                \_ ->
                    stored (Just (savedNotProvided PatientRefused)) (reasonChosen ReceivedEmergencyCase emptyHealthEducationForm)
                        |> Expect.equal (Just (savedNotProvided ReceivedEmergencyCase))
            ]
        , describe "Older health education saved with \"No\" and no reason, reopened: \"Why not?\" is unanswered and the task is not complete"
            [ test "\"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown (Just (savedNotProvided NoReasonForNotProvidingHealthEducation)) emptyHealthEducationForm
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "once a reason is chosen the task is complete and the reason is saved" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown (Just (savedNotProvided NoReasonForNotProvidingHealthEducation)) form
                                |> expectWhyNot { asked = True, ticked = 1, complete = True }
                        , \form ->
                            stored (Just (savedNotProvided NoReasonForNotProvidingHealthEducation)) form
                                |> Expect.equal (Just (savedNotProvided PatientRefused))
                        ]
                        (reasonChosen PatientRefused emptyHealthEducationForm)
            ]
        , describe "Records the defect does not touch stay as they are"
            [ test "health education saved with \"Yes\", reopened without changes, is complete and saves the same record" <|
                \_ ->
                    Expect.all
                        [ \form ->
                            shown (Just savedProvided) form
                                |> expectWhyNot { asked = False, ticked = 0, complete = True }
                        , \form ->
                            stored (Just savedProvided) form
                                |> Expect.equal (Just savedProvided)
                        ]
                        emptyHealthEducationForm
            , test "a first-time \"No\", with no saved record, leaves \"Why not?\" unanswered and the task not complete" <|
                \_ ->
                    shown Nothing (answered False)
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            ]
        ]


{-| The treatment review (Acute Illness subsequent visit, HIV, Tuberculosis)
asks "Why not?" after "taken as prescribed: No" and "How many doses?" after
"missed doses: Yes".

Its Yes/No handlers live inside `treatmentReviewInputsAndTasks`, so each test
reads the real handler off the rendered Yes/No label instead of copying it. The
pages run that handler on the form already merged with the saved record, keep
the result as the page's form, and merge it with the saved record again to
draw and to save; `tap` and `choose*` below follow the same steps.

A stored "no reason" is `NoReasonForNotTakingSign`; stored "no number" is 0
missed doses.

-}
ongoingTreatmentReviewFollowUpTest : Test
ongoingTreatmentReviewFollowUpTest =
    let
        savedWith : List TreatmentOngoingSign -> ReasonForNotTaking -> Int -> TreatmentOngoingValue
        savedWith signs reason doses =
            { signs = EverySet.fromList signs
            , reasonForNotTaking = reason
            , missedDoses = doses
            , adverseEvents = EverySet.singleton NoAdverseEvent
            }

        -- Taken as prescribed, feeling better, no missed doses, no side effects.
        savedAllGood =
            savedWith [ FeelingBetter, TakenAsPrescribed ] NoReasonForNotTakingSign 0

        savedNotTaken reason =
            savedWith [ FeelingBetter ] reason 0

        savedMissed doses =
            savedWith [ FeelingBetter, MissedDoses, TakenAsPrescribed ] NoReasonForNotTakingSign doses

        merged saved pageForm =
            ongoingTreatmentReviewFormWithDefault pageForm saved

        -- The page's form after the label `answer` of the Yes/No input `inputClass` is tapped.
        tap inputClass answer saved pageForm =
            let
                form =
                    merged saved pageForm
            in
            treatmentReviewInputsAndTasks English
                (\updateFunc value -> updateFunc value)
                (always identity)
                (always identity)
                (always identity)
                form
                |> Tuple.first
                |> div []
                |> Query.fromHtml
                |> Query.find [ class inputClass ]
                |> Query.find [ tag "label", containing [ text answer ] ]
                |> Event.simulate Event.click
                |> Event.toResult
                |> Result.map (\updateFunc -> updateFunc form)

        -- The page's form after a reason for not taking is picked.
        chooseReason reason saved pageForm =
            let
                form =
                    merged saved pageForm
            in
            { form | reasonForNotTaking = Just reason, reasonForNotTakingDirty = True }

        -- The page's form after a number of missed doses is picked.
        chooseDoses doses saved pageForm =
            let
                form =
                    merged saved pageForm
            in
            { form | totalMissedDoses = Just doses, totalMissedDosesDirty = True }

        shown saved pageForm =
            merged saved pageForm
                |> treatmentReviewInputsAndTasks English (\_ _ -> ()) (always ()) (always ()) (always ())

        -- The record as comparable data: a set of signs compares unequal when built in another order.
        comparable =
            Maybe.map
                (\value ->
                    { signs =
                        List.filter (\sign -> EverySet.member sign value.signs)
                            [ FeelingBetter, MissedDoses, NoTreatmentOngoingSign, SideEffects, TakenAsPrescribed ]
                    , reasonForNotTaking = value.reasonForNotTaking
                    , missedDoses = value.missedDoses
                    , adverseEvents = EverySet.toList value.adverseEvents
                    }
                )

        stored saved pageForm =
            toOngoingTreatmentReviewValueWithDefault saved pageForm
                |> comparable

        expectRecord expected =
            Expect.equal (comparable (Just expected))

        onTapped result expectation =
            case result of
                Ok pageForm ->
                    expectation pageForm

                Err error ->
                    Expect.fail error

        -- "How many doses?" as drawn: asked or not, the number it shows (Nothing for empty), and the task list.
        expectHowManyDoses expected ( inputs, tasks ) =
            let
                selects =
                    Query.fromHtml (div [] inputs)
                        |> Query.findAll [ tag "select" ]

                numberShown =
                    case expected.number of
                        Just number ->
                            Query.first selects
                                |> Query.find [ tag "option", selected True ]
                                |> Query.has [ attribute (Html.Attributes.value (String.fromInt number)) ]

                        Nothing ->
                            -- Empty: any option marked selected is the blank one.
                            Query.first selects
                                |> Query.findAll [ tag "option", selected True ]
                                |> Query.each (Query.has [ attribute (Html.Attributes.value "") ])
            in
            Expect.all
                [ \_ ->
                    if expected.asked then
                        Expect.all [ \_ -> Query.count (Expect.equal 1) selects, \_ -> numberShown ] ()

                    else
                        Query.count (Expect.equal 0) selects
                , \_ ->
                    followUpTasksComplete tasks
                        |> Expect.equal expected.complete
                ]
                ()
    in
    describe "Treatment review: \"Why not?\" and \"How many doses?\" when a saved answer is changed"
        [ describe "A treatment review saved with \"taken as prescribed: Yes\", changed to \"No\": \"Why not?\" is unanswered and the task is not complete until a reason is chosen"
            [ test "\"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    onTapped (tap "taken-as-prescribed" "No" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (shown (Just savedAllGood)
                            >> expectWhyNot { asked = True, ticked = 0, complete = False }
                        )
            , test "once a reason is chosen it is ticked and the task is complete" <|
                \_ ->
                    onTapped (tap "taken-as-prescribed" "No" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (chooseReason NotTakingNoMoney (Just savedAllGood)
                            >> shown (Just savedAllGood)
                            >> expectWhyNot { asked = True, ticked = 1, complete = True }
                        )
            , test "the chosen reason is what gets saved" <|
                \_ ->
                    onTapped (tap "taken-as-prescribed" "No" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (chooseReason NotTakingMemoryProblems (Just savedAllGood)
                            >> stored (Just savedAllGood)
                            >> expectRecord (savedNotTaken NotTakingMemoryProblems)
                        )
            , test "changed to \"No\" and back to \"Yes\": the task is complete and no reason is saved" <|
                \_ ->
                    onTapped
                        (tap "taken-as-prescribed" "No" (Just savedAllGood) emptyOngoingTreatmentReviewForm
                            |> Result.andThen (tap "taken-as-prescribed" "Yes" (Just savedAllGood))
                        )
                        (Expect.all
                            [ shown (Just savedAllGood) >> expectWhyNot { asked = False, ticked = 0, complete = True }
                            , stored (Just savedAllGood) >> expectRecord savedAllGood
                            ]
                        )
            ]
        , describe "A treatment review saved with \"missed doses: No\", changed to \"Yes\": \"How many doses?\" is empty and the task is not complete until a number is chosen"
            [ test "\"How many doses?\" is asked, shows no number, the task is not complete" <|
                \_ ->
                    onTapped (tap "missed-doses" "Yes" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (shown (Just savedAllGood)
                            >> expectHowManyDoses { asked = True, number = Nothing, complete = False }
                        )
            , test "once a number is chosen it is shown and the task is complete" <|
                \_ ->
                    onTapped (tap "missed-doses" "Yes" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (chooseDoses 3 (Just savedAllGood)
                            >> shown (Just savedAllGood)
                            >> expectHowManyDoses { asked = True, number = Just 3, complete = True }
                        )
            , test "the chosen number is what gets saved" <|
                \_ ->
                    onTapped (tap "missed-doses" "Yes" (Just savedAllGood) emptyOngoingTreatmentReviewForm)
                        (chooseDoses 21 (Just savedAllGood)
                            >> stored (Just savedAllGood)
                            >> expectRecord (savedMissed 21)
                        )
            , test "changed to \"Yes\" and back to \"No\": the task is complete and zero doses are saved" <|
                \_ ->
                    onTapped
                        (tap "missed-doses" "Yes" (Just savedAllGood) emptyOngoingTreatmentReviewForm
                            |> Result.andThen (tap "missed-doses" "No" (Just savedAllGood))
                        )
                        (Expect.all
                            [ shown (Just savedAllGood) >> expectHowManyDoses { asked = False, number = Nothing, complete = True }
                            , stored (Just savedAllGood) >> expectRecord savedAllGood
                            ]
                        )
            ]
        , describe "A treatment review whose follow-up is no longer asked saves no earlier answer"
            [ test "saved \"taken as prescribed: No\" with a reason, changed to \"Yes\" and saved: the record holds no reason" <|
                \_ ->
                    onTapped (tap "taken-as-prescribed" "Yes" (Just (savedNotTaken NotTakingNoMoney)) emptyOngoingTreatmentReviewForm)
                        (stored (Just (savedNotTaken NotTakingNoMoney))
                            >> expectRecord savedAllGood
                        )
            , test "saved \"missed doses: Yes\" with 5 doses, changed to \"No\" and saved: the record holds zero doses" <|
                \_ ->
                    onTapped (tap "missed-doses" "No" (Just (savedMissed 5)) emptyOngoingTreatmentReviewForm)
                        (stored (Just (savedMissed 5))
                            >> expectRecord savedAllGood
                        )
            ]
        , describe "A treatment review saved with \"No\" and a reason, reopened without changes: the reason is shown selected and the task is complete"
            [ test "the saved reason is ticked and the task is complete" <|
                \_ ->
                    shown (Just (savedNotTaken NotTakingAdverseEvent)) emptyOngoingTreatmentReviewForm
                        |> expectWhyNot { asked = True, ticked = 1, complete = True }
            , test "saving it again stores the same record" <|
                \_ ->
                    stored (Just (savedNotTaken NotTakingAdverseEvent)) emptyOngoingTreatmentReviewForm
                        |> expectRecord (savedNotTaken NotTakingAdverseEvent)
            , test "the saved reason stays ticked when another question's answer is changed" <|
                \_ ->
                    onTapped (tap "feeling-better" "No" (Just (savedNotTaken NotTakingAdverseEvent)) emptyOngoingTreatmentReviewForm)
                        (shown (Just (savedNotTaken NotTakingAdverseEvent))
                            >> expectWhyNot { asked = True, ticked = 1, complete = True }
                        )
            , test "saved \"missed doses: Yes\" with 5 doses, reopened: 5 is shown and the task is complete" <|
                \_ ->
                    shown (Just (savedMissed 5)) emptyOngoingTreatmentReviewForm
                        |> expectHowManyDoses { asked = True, number = Just 5, complete = True }
            , test "saved \"missed doses: Yes\" with 5 doses, saved again: the same record" <|
                \_ ->
                    stored (Just (savedMissed 5)) emptyOngoingTreatmentReviewForm
                        |> expectRecord (savedMissed 5)
            ]
        , describe "An older treatment review saved with \"No\" and no reason, reopened: \"Why not?\" is unanswered and the task is not complete"
            [ test "\"Why not?\" is asked, nothing is ticked, the task is not complete" <|
                \_ ->
                    shown (Just (savedNotTaken NoReasonForNotTakingSign)) emptyOngoingTreatmentReviewForm
                        |> expectWhyNot { asked = True, ticked = 0, complete = False }
            , test "once a reason is chosen the task is complete and the reason is saved" <|
                \_ ->
                    Expect.all
                        [ shown (Just (savedNotTaken NoReasonForNotTakingSign)) >> expectWhyNot { asked = True, ticked = 1, complete = True }
                        , stored (Just (savedNotTaken NoReasonForNotTakingSign)) >> expectRecord (savedNotTaken NotTakingOther)
                        ]
                        (chooseReason NotTakingOther (Just (savedNotTaken NoReasonForNotTakingSign)) emptyOngoingTreatmentReviewForm)
            , test "saved \"missed doses: Yes\" with zero doses, reopened: \"How many doses?\" is empty and the task is not complete" <|
                \_ ->
                    shown (Just (savedMissed 0)) emptyOngoingTreatmentReviewForm
                        |> expectHowManyDoses { asked = True, number = Nothing, complete = False }
            ]
        , describe "Records the defect does not touch stay as they are"
            [ test "a review with no follow-up asked, reopened without changes, is complete and saves the same record" <|
                \_ ->
                    Expect.all
                        [ shown (Just savedAllGood) >> expectWhyNot { asked = False, ticked = 0, complete = True }
                        , stored (Just savedAllGood) >> expectRecord savedAllGood
                        ]
                        emptyOngoingTreatmentReviewForm
            , test "a first-time \"taken as prescribed: No\", with no saved record, leaves \"Why not?\" unanswered and the task not complete" <|
                \_ ->
                    onTapped
                        (tap "taken-as-prescribed"
                            "No"
                            Nothing
                            { emptyOngoingTreatmentReviewForm
                                | feelingBetter = Just True
                                , missedDoses = Just False
                                , sideEffects = Just False
                            }
                        )
                        (shown Nothing
                            >> expectWhyNot { asked = True, ticked = 0, complete = False }
                        )
            , test "a first-time \"missed doses: Yes\", with no saved record, leaves \"How many doses?\" empty and the task not complete" <|
                \_ ->
                    onTapped
                        (tap "missed-doses"
                            "Yes"
                            Nothing
                            { emptyOngoingTreatmentReviewForm
                                | feelingBetter = Just True
                                , sideEffects = Just False
                                , takenAsPrescribed = Just True
                            }
                        )
                        (shown Nothing
                            >> expectHowManyDoses { asked = True, number = Nothing, complete = False }
                        )
            ]
        ]
