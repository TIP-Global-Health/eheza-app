module Pages.Prenatal.ProgressReport.Test exposing (all)

import Backend.Measurement.Model
    exposing
        ( MedicationDistributionSign(..)
        , OutsideCareMedication(..)
        , ReinforceTreatmentSign(..)
        , emptyPrenatalMeasurements
        )
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet exposing (EverySet)
import Expect
import Html
import Pages.Prenatal.ProgressReport.View exposing (viewTreatmentForDiagnosis, viewTreatmentForOutsideCareDiagnosis)
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
    translate English Translate.ReinforceAdherenceQuestion
        ++ " "
        ++ translate English (Translate.MedicationDistributionSign medication)
        ++ ": "
        ++ answer


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
        , test "all answers No read no treatment administered" <|
            \_ ->
                renderAnemia DiagnosisModerateAnemiaInitialPhase [] (Just [ NoReinforceTreatmentSigns ])
                    |> Expect.all
                        [ Query.has [ text (String.toLower (translate English Translate.NoTreatmentAdministered)) ]
                        , Query.hasNot [ text (translate English Translate.ReinforceAdherenceQuestion) ]
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
