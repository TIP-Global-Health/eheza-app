module Pages.Prenatal.ProgressReport.Test exposing (all)

import AssocList
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , DangerSign(..)
        , MedicationDistributionSign(..)
        , MedicationNonAdministrationSign(..)
        , MedicationSign(..)
        , OutsideCareMedication(..)
        , PostpartumChildDangerSign(..)
        , PostpartumMotherDangerSign(..)
        , PrenatalMeasurements
        , ReinforceTreatmentSign(..)
        , emptyPrenatalMeasurements
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), emptyPrenatalEncounter)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Html
import Measurement.Utils exposing (reinforceAdherenceQuestion)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..))
import Pages.Prenatal.ProgressReport.Utils exposing (resolveMedicationHistoryDate)
import Pages.Prenatal.ProgressReport.View exposing (matchCHWActivityAtEncounter, viewTreatmentForDiagnosis, viewTreatmentForOutsideCareDiagnosis)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import TestFixtures
import Time exposing (Month(..))
import Translate exposing (translate)
import Translate.Model exposing (Language(..))



-- The outside-care diagnosis line on the progress report. Each diagnosis
-- lists only the medicines of its own question, as the form stores it.
-- Medicines of another illness are recorded alongside to catch a wrong list.


render : PrenatalDiagnosis -> List OutsideCareMedication -> Query.Single msg
render diagnosis =
    EverySet.fromList >> Just >> renderStored diagnosis


renderStored : PrenatalDiagnosis -> Maybe (EverySet OutsideCareMedication) -> Query.Single msg
renderStored diagnosis medications =
    viewTreatmentForOutsideCareDiagnosis English
        (Date.fromCalendarDate 2026 Oct 1)
        medications
        diagnosis
        |> Html.div []
        |> Query.fromHtml


label : OutsideCareMedication -> String
label =
    Translate.OutsideCareMedicationLabel >> translate English


treatedWith : OutsideCareMedication -> String
treatedWith medication =
    ", treated with " ++ label medication ++ ", added"


noTreatment : String
noTreatment =
    ", no treatment administered, added"


withoutTreatmentPhrase : String
withoutTreatmentPhrase =
    " - "
        ++ String.toLower (translate English Translate.DiagnosedByOutsideCare)
        ++ ", "
        ++ String.toLower (translate English Translate.AddedToPatientRecordOn)


all : Test
all =
    describe "Pages.Prenatal.ProgressReport"
        [ outsideCareDiagnosisTest
        , moderateAnemiaTreatmentTest
        , chwDangerSignsActionTest
        , medicationHistoryTest
        ]


chwDangerSignsActionTest : Test
chwDangerSignsActionTest =
    let
        dangerSignsPresent encounterType signs mother child =
            { emptyPrenatalMeasurements
                | dangerSigns =
                    TestFixtures.wrapMeasurement (Date.fromCalendarDate 2026 Oct 1)
                        { signs = EverySet.fromList signs
                        , postpartumMother = EverySet.fromList mother
                        , postpartumChild = EverySet.fromList child
                        }
            }
                |> (\measurements -> matchCHWActivityAtEncounter encounterType measurements ActionDangerSignsPresent)
    in
    describe "CHW Activity pane: Danger Signs Present"
        [ test "an antenatal danger sign" <|
            \_ ->
                dangerSignsPresent ChwFirstEncounter [ Fever ] [] []
                    |> Expect.equal True
        , test "no antenatal danger sign" <|
            \_ ->
                dangerSignsPresent ChwFirstEncounter [ NoDangerSign ] [] []
                    |> Expect.equal False
        , test "a mother's danger sign at a postpartum visit" <|
            \_ ->
                dangerSignsPresent ChwPostpartumEncounter [ NoDangerSign ] [ PostpartumMotherFever ] [ NoPostpartumChildDangerSigns ]
                    |> Expect.equal True
        , test "a child's danger sign at a postpartum visit" <|
            \_ ->
                dangerSignsPresent ChwPostpartumEncounter [ NoDangerSign ] [ NoPostpartumMotherDangerSigns ] [ PostpartumChildAbnormalTemperature ]
                    |> Expect.equal True
        , test "no danger sign at a postpartum visit" <|
            \_ ->
                dangerSignsPresent ChwPostpartumEncounter [ NoDangerSign ] [ NoPostpartumMotherDangerSigns ] [ NoPostpartumChildDangerSigns ]
                    |> Expect.equal False
        ]


outsideCareDiagnosisTest : Test
outsideCareDiagnosisTest =
    describe "viewTreatmentForOutsideCareDiagnosis"
        [ test "syphilis lists its own medicine only" <|
            \_ ->
                render DiagnosisSyphilisRecurrentPhase [ OutsideCareMedicationPenecilin1, OutsideCareMedicationIron1 ]
                    |> Expect.all
                        [ Query.has [ text (treatedWith OutsideCareMedicationPenecilin1) ]
                        , Query.hasNot [ text (label OutsideCareMedicationIron1) ]
                        ]
        , test "syphilis with None of these reads no treatment" <|
            \_ ->
                render DiagnosisSyphilisRecurrentPhase [ NoOutsideCareMedicationForSyphilis, OutsideCareMedicationIron1 ]
                    |> Query.has [ text noTreatment ]
        , test "moderate anemia lists its own medicine only" <|
            \_ ->
                render DiagnosisModerateAnemiaRecurrentPhase [ OutsideCareMedicationIron1, OutsideCareMedicationPenecilin1 ]
                    |> Expect.all
                        [ Query.has [ text (treatedWith OutsideCareMedicationIron1) ]
                        , Query.hasNot [ text (label OutsideCareMedicationPenecilin1) ]
                        ]
        , test "moderate anemia with None of these reads no treatment" <|
            \_ ->
                render DiagnosisModerateAnemiaRecurrentPhase [ NoOutsideCareMedicationForAnemia, OutsideCareMedicationPenecilin1 ]
                    |> Query.has [ text noTreatment ]
        , test "HIV with None of these reads no treatment" <|
            \_ ->
                render DiagnosisHIVInitialPhase [ NoOutsideCareMedicationForHIV ]
                    |> Expect.all
                        [ Query.has [ text noTreatment ]
                        , Query.hasNot [ text "treated with" ]
                        ]
        , test "HIV lists its medicine" <|
            \_ ->
                render DiagnosisHIVInitialPhase [ OutsideCareMedicationTDF3TC ]
                    |> Query.has [ text (treatedWith OutsideCareMedicationTDF3TC) ]
        , test "hypertension lists its medicine" <|
            \_ ->
                render DiagnosisChronicHypertensionImmediate [ OutsideCareMedicationMethyldopa2, OutsideCareMedicationIron1 ]
                    |> Query.has [ text (treatedWith OutsideCareMedicationMethyldopa2) ]
        , test "gestational hypertension and moderate preeclampsia list the hypertension medicine" <|
            \_ ->
                Expect.all
                    (List.map
                        (\diagnosis ->
                            \_ ->
                                render diagnosis [ OutsideCareMedicationMethyldopa2 ]
                                    |> Query.has [ text (treatedWith OutsideCareMedicationMethyldopa2) ]
                        )
                        [ DiagnosisGestationalHypertensionImmediate, DiagnosisModeratePreeclampsiaInitialPhase ]
                    )
                    ()
        , test "malaria lists its own medicine only" <|
            \_ ->
                render DiagnosisMalariaInitialPhase [ OutsideCareMedicationCoartem, OutsideCareMedicationIron1 ]
                    |> Expect.all
                        [ Query.has [ text (treatedWith OutsideCareMedicationCoartem) ]
                        , Query.hasNot [ text (label OutsideCareMedicationIron1) ]
                        ]
        , test "a diagnosis with no medicine question has no treatment phrase" <|
            \_ ->
                render DiagnosisTuberculosis [ OutsideCareMedicationCoartem ]
                    |> Expect.all
                        [ Query.has [ text withoutTreatmentPhrase ]
                        , Query.hasNot [ text (label OutsideCareMedicationCoartem) ]
                        ]
        , test "no medicine answer stored reads no treatment" <|
            \_ ->
                renderStored DiagnosisSyphilisRecurrentPhase Nothing
                    |> Query.has [ text noTreatment ]
        , test "a diagnosis outside care cannot record shows nothing" <|
            \_ ->
                viewTreatmentForOutsideCareDiagnosis English
                    (Date.fromCalendarDate 2026 Oct 1)
                    (Just (EverySet.fromList [ OutsideCareMedicationPenecilin1 ]))
                    DiagnosisSyphilisInitialPhase
                    |> List.length
                    |> Expect.equal 0
        ]



-- The moderate-anemia line of the Medical Diagnosis pane. It lists the
-- reinforce adherence answers when the record holds them, and Iron and Folic
-- acid otherwise.


renderAnemia : PrenatalDiagnosis -> List MedicationDistributionSign -> Maybe (List ReinforceTreatmentSign) -> Query.Single msg
renderAnemia diagnosis distributionSigns reinforceSigns =
    let
        measurements =
            { emptyPrenatalMeasurements
                | medicationDistribution =
                    TestFixtures.wrapMeasurement (Date.fromCalendarDate 2026 Oct 1)
                        { distributionSigns = EverySet.fromList distributionSigns
                        , nonAdministrationSigns = EverySet.empty
                        , recommendedTreatmentSigns = Nothing
                        , avoidingGuidanceReason = Nothing
                        , reinforceTreatmentSigns = Maybe.map EverySet.fromList reinforceSigns
                        }
            }
    in
    viewTreatmentForDiagnosis English
        (Date.fromCalendarDate 2026 Oct 1)
        measurements
        (EverySet.singleton diagnosis)
        diagnosis
        |> Html.div []
        |> Query.fromHtml


adherenceAnswer : MedicationDistributionSign -> String -> String
adherenceAnswer medication answer =
    reinforceAdherenceQuestion English (Translate.MedicationDistributionSign medication) ++ ": " ++ answer


repeatTestAnswer : String -> String
repeatTestAnswer answer =
    translate English Translate.RepeatHemoglobinTestQuestion ++ ": " ++ answer


noTreatmentRecorded : String
noTreatmentRecorded =
    String.toLower (translate English Translate.NoTreatmentRecorded)


moderateAnemiaTreatmentTest : Test
moderateAnemiaTreatmentTest =
    describe "moderate anemia treatment"
        [ test "all reinforce answers Yes are listed" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaInitialPhase
                    []
                    (Just [ ReinforceSignFefol, ReinforceSignMMS, ReinforceSignRepeatHemoglobinTest ])
                    |> Expect.all
                        [ Query.has [ text (adherenceAnswer Fefol "Yes") ]
                        , Query.has [ text (adherenceAnswer MMS "Yes") ]
                        , Query.has [ text (repeatTestAnswer "Yes") ]
                        , Query.hasNot [ text noTreatmentRecorded ]
                        ]
        , test "a question left out of the set reads No" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaInitialPhase [] (Just [ ReinforceSignMMS ])
                    |> Expect.all
                        [ Query.has [ text (adherenceAnswer Fefol "No") ]
                        , Query.has [ text (adherenceAnswer MMS "Yes") ]
                        , Query.has [ text (repeatTestAnswer "No") ]
                        ]
        , test "all answers No are listed, not no treatment recorded" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaInitialPhase [] (Just [ NoReinforceTreatmentSigns ])
                    |> Expect.all
                        [ Query.has [ text (adherenceAnswer Fefol "No") ]
                        , Query.has [ text (adherenceAnswer MMS "No") ]
                        , Query.has [ text (repeatTestAnswer "No") ]
                        , Query.hasNot [ text noTreatmentRecorded ]
                        ]
        , test "the recurrent phase lists the same answers" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaRecurrentPhase [] (Just [ ReinforceSignFefol, ReinforceSignRepeatHemoglobinTest ])
                    |> Expect.all
                        [ Query.has [ text (adherenceAnswer Fefol "Yes") ]
                        , Query.has [ text (adherenceAnswer MMS "No") ]
                        , Query.has [ text (repeatTestAnswer "Yes") ]
                        ]
        , test "an older record lists Iron and Folic acid" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaInitialPhase [ Iron, FolicAcid ] Nothing
                    |> Expect.all
                        [ Query.has [ text (translate English Translate.TreatedWith ++ " " ++ translate English (Translate.MedicationDistributionSign Iron)) ]
                        , Query.has [ text (translate English Translate.TreatedWith ++ " " ++ translate English (Translate.MedicationDistributionSign FolicAcid)) ]
                        , Query.hasNot [ text (translate English Translate.ReinforceAdherenceQuestion) ]
                        ]
        ]



-- The Medication History pane: the date each medicine was received in this
-- pregnancy, "--" (Nothing) when there is none. The report is opened at
-- today's nurse visit on `reportDate`, unless a test says it is a CHW visit.
-- Mebendazole can be handed over in two places, the Medication activity or
-- Next Steps medication distribution, and either counts as given. The woman
-- saying in Treatment Review that she received it elsewhere gives no date.
-- Earlier visits are a number of weeks before `reportDate`, and everything
-- recorded at a visit is recorded on the day it started unless a test says
-- otherwise.


reportDate : NominalDate
reportDate =
    Date.fromCalendarDate 2026 Oct 1


weeksBefore : Int -> NominalDate
weeksBefore weeks =
    Date.add Date.Weeks -weeks reportDate


{-| Something recorded on the given day at a visit.
-}
type alias Record =
    NominalDate -> PrenatalMeasurements -> PrenatalMeasurements


recordAll : NominalDate -> List Record -> PrenatalMeasurements
recordAll date =
    List.foldl (\record -> record date) emptyPrenatalMeasurements


{-| Today's visit, at 30 weeks of pregnancy, after the given earlier nurse
visits, with the given records made today.
-}
todaysVisit : List PreviousEncounterData -> List Record -> AssembledData
todaysVisit earlierVisits records =
    let
        lmpDate =
            Date.add Date.Weeks -30 reportDate
    in
    { id = toEntityUuid "visit"
    , encounter = emptyPrenatalEncounter (toEntityUuid "pregnancy") reportDate NurseEncounter Nothing
    , participant = TestFixtures.testParticipant lmpDate AntenatalEncounter
    , person = TestFixtures.testPerson
    , measurements = recordAll reportDate records
    , nursePreviousEncountersData = earlierVisits
    , chwPreviousMeasurementsWithDates = []
    , globalLmpDate = Just lmpDate
    , globalObstetricHistory = Nothing
    , vaccinationHistory = AssocList.empty
    , vaccinationProgress = AssocList.empty
    }


{-| An earlier nurse visit, started `weeksAgo` weeks before `reportDate`,
with the given records made on that day.
-}
earlierVisit : Int -> List Record -> PreviousEncounterData
earlierVisit weeksAgo records =
    let
        date =
            weeksBefore weeksAgo
    in
    { startDate = date
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , measurements = recordAll date records
    }


{-| The Medication activity task of the given row, answered with `note`:
`AdministeredToday` when it was handed over, a reason when it was not.
-}
inMedicationActivity : MedicationDistributionSign -> AdministrationNote -> Record
inMedicationActivity row note date measurements =
    let
        measurement =
            TestFixtures.wrapMeasurement date note
    in
    case row of
        Calcium ->
            { measurements | calcium = measurement }

        Fefol ->
            { measurements | fefol = measurement }

        FolicAcid ->
            { measurements | folate = measurement }

        Iron ->
            { measurements | iron = measurement }

        Mebendezole ->
            { measurements | mebendazole = measurement }

        MMS ->
            { measurements | mms = measurement }

        _ ->
            measurements


{-| Next Steps medication distribution, with the given medicines and the
reasons recorded for those not handed over.
-}
nextStepsDistribution : List MedicationDistributionSign -> List MedicationNonAdministrationSign -> Record
nextStepsDistribution distributed notDistributed date measurements =
    { measurements
        | medicationDistribution =
            TestFixtures.wrapMeasurement date
                { distributionSigns = EverySet.fromList distributed
                , nonAdministrationSigns = EverySet.fromList notDistributed
                , recommendedTreatmentSigns = Nothing
                , avoidingGuidanceReason = Nothing
                , reinforceTreatmentSigns = Nothing
                }
    }


{-| Next Steps medication distribution handed over the given medicines.
-}
handedOverInNextSteps : List MedicationDistributionSign -> Record
handedOverInNextSteps distributed =
    nextStepsDistribution distributed []


{-| Treatment Review: what the woman says she received before this visit.
-}
reportedInTreatmentReview : List MedicationSign -> Record
reportedInTreatmentReview signs date measurements =
    { measurements
        | medication =
            TestFixtures.wrapMeasurement date
                { signs = Just (EverySet.fromList signs)
                , hivTreatment = Nothing
                , hypertensionTreatment = Nothing
                , malariaTreatment = Nothing
                , anemiaTreatment = Nothing
                , syphilisTreatment = Nothing
                }
    }


{-| The date the Mebendazole row of a nurse's report shows.
-}
mebendazoleDate : AssembledData -> Maybe NominalDate
mebendazoleDate assembled =
    resolveMedicationHistoryDate False assembled Mebendezole


otherRows : List ( String, MedicationDistributionSign )
otherRows =
    [ ( "Calcium", Calcium )
    , ( "Fefol", Fefol )
    , ( "Folic acid", FolicAcid )
    , ( "Iron", Iron )
    , ( "MMS", MMS )
    ]


medicationHistoryTest : Test
medicationHistoryTest =
    describe "Medication History"
        [ describe "Mebendazole given in the Medication activity of a nurse visit shows that visit's date, as today"
            [ test "given at an earlier visit" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ inMedicationActivity Mebendezole AdministeredToday ] ] []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            , test "given at today's visit" <|
                \_ ->
                    todaysVisit [] [ inMedicationActivity Mebendezole AdministeredToday ]
                        |> mebendazoleDate
                        |> Expect.equal (Just reportDate)
            ]
        , describe "Mebendazole handed over in Next Steps medication distribution shows the date it was handed over"
            [ -- The defect as reported: given from Next Steps at the visit
              -- the report is opened from.
              test "handed over at today's visit" <|
                \_ ->
                    todaysVisit [] [ handedOverInNextSteps [ Mebendezole ] ]
                        |> mebendazoleDate
                        |> Expect.equal (Just reportDate)
            , test "handed over at an earlier visit" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ] ] []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            , test "handed over together with other medicines" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Iron, FolicAcid, Mebendezole ] ] ] []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            , -- The report opened at a CHW visit looks at the earlier nurse
              -- visits only; a Next Steps hand-over at one of them counts.
              test "handed over at an earlier nurse visit, report opened at a CHW visit" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ] ] []
                        |> (\assembled -> resolveMedicationHistoryDate True assembled Mebendezole)
                        |> Expect.equal (Just (weeksBefore 4))
            , -- The hand-over was recorded on the day after the visit
              -- started. The criterion names the hand-over date.
              test "the hand-over date when it differs from the day the visit started" <|
                \_ ->
                    let
                        visit =
                            earlierVisit 4 []

                        handedOverOn =
                            Date.add Date.Days 1 visit.startDate
                    in
                    todaysVisit
                        [ { visit | measurements = handedOverInNextSteps [ Mebendezole ] handedOverOn visit.measurements } ]
                        []
                        |> mebendazoleDate
                        |> Expect.equal (Just handedOverOn)
            ]
        , describe "When Mebendazole was given at more than one visit, by either route, the most recent date is shown"
            [ test "Medication activity first, Next Steps later" <|
                \_ ->
                    todaysVisit
                        [ earlierVisit 8 [ inMedicationActivity Mebendezole AdministeredToday ]
                        , earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ]
                        ]
                        []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            , test "Next Steps first, Medication activity later" <|
                \_ ->
                    todaysVisit
                        [ earlierVisit 8 [ handedOverInNextSteps [ Mebendezole ] ]
                        , earlierVisit 4 [ inMedicationActivity Mebendezole AdministeredToday ]
                        ]
                        []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            , test "Next Steps at an earlier visit and again today" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ] ]
                        [ handedOverInNextSteps [ Mebendezole ] ]
                        |> mebendazoleDate
                        |> Expect.equal (Just reportDate)
            , test "Medication activity at an earlier visit, Next Steps today" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ inMedicationActivity Mebendezole AdministeredToday ] ]
                        [ handedOverInNextSteps [ Mebendezole ] ]
                        |> mebendazoleDate
                        |> Expect.equal (Just reportDate)
            , -- The earlier visits are not listed in date order.
              test "earlier visits listed newest first" <|
                \_ ->
                    todaysVisit
                        [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ]
                        , earlierVisit 8 [ inMedicationActivity Mebendezole AdministeredToday ]
                        ]
                        []
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            ]
        , describe "Mebendazole the woman only reported in Treatment Review, as received elsewhere, shows --"
            [ test "reported at today's visit" <|
                \_ ->
                    todaysVisit [] [ reportedInTreatmentReview [ Mebendazole ] ]
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , test "reported at an earlier visit" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ reportedInTreatmentReview [ Mebendazole ] ] ] []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , -- The other Treatment Review answer that stops Mebendazole
              -- being offered.
              test "a deworming pill reported" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ reportedInTreatmentReview [ DewormingPill ] ] ] []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , -- A later report does not replace the date of an earlier
              -- hand-over.
              test "reported today after a Next Steps hand-over at an earlier visit" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ] ]
                        [ reportedInTreatmentReview [ Mebendazole ] ]
                        |> mebendazoleDate
                        |> Expect.equal (Just (weeksBefore 4))
            ]
        , describe "When Mebendazole was never given, the row shows --"
            [ test "nothing recorded in the pregnancy" <|
                \_ ->
                    todaysVisit [] []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , test "the Medication activity recorded it was not given" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ inMedicationActivity Mebendezole NonAdministrationPatientDeclined ] ] []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , test "Next Steps recorded it was not handed over, out of stock" <|
                \_ ->
                    todaysVisit
                        [ earlierVisit 4
                            [ nextStepsDistribution [ NoMedicationDistributionSigns ]
                                [ MedicationMebendezole NonAdministrationLackOfStock ]
                            ]
                        ]
                        []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            , test "Next Steps handed over other medicines only" <|
                \_ ->
                    todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Iron, FolicAcid ] ] ] []
                        |> mebendazoleDate
                        |> Expect.equal Nothing
            ]
        , describe "The other medication rows are unchanged" <|
            List.concatMap
                (\( name, row ) ->
                    let
                        rowDate assembled =
                            resolveMedicationHistoryDate False assembled row
                    in
                    [ test (name ++ ": given in the Medication activity at two visits shows the later one") <|
                        \_ ->
                            todaysVisit
                                [ earlierVisit 8 [ inMedicationActivity row AdministeredToday ]
                                , earlierVisit 4 [ inMedicationActivity row AdministeredToday ]
                                ]
                                []
                                |> rowDate
                                |> Expect.equal (Just (weeksBefore 4))
                    , test (name ++ ": given in the Medication activity today shows today") <|
                        \_ ->
                            todaysVisit [] [ inMedicationActivity row AdministeredToday ]
                                |> rowDate
                                |> Expect.equal (Just reportDate)
                    , test (name ++ ": not given in the Medication activity shows --") <|
                        \_ ->
                            todaysVisit [ earlierVisit 4 [ inMedicationActivity row NonAdministrationLackOfStock ] ] []
                                |> rowDate
                                |> Expect.equal Nothing
                    , test (name ++ ": Mebendazole handed over in Next Steps does not date it") <|
                        \_ ->
                            todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ Mebendezole ] ] ]
                                [ handedOverInNextSteps [ Mebendezole ] ]
                                |> rowDate
                                |> Expect.equal Nothing
                    , -- Taken literally, "unchanged": a Next Steps hand-over
                      -- of this medicine still does not date its row.
                      test (name ++ ": handed over in Next Steps only shows --, as today") <|
                        \_ ->
                            todaysVisit [ earlierVisit 4 [ handedOverInNextSteps [ row ] ] ]
                                [ handedOverInNextSteps [ row ] ]
                                |> rowDate
                                |> Expect.equal Nothing
                    ]
                )
                otherRows
        ]
