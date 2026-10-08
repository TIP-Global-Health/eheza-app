module Pages.Prenatal.ProgressReport.Test exposing (all)

import Backend.Measurement.Model exposing (OutsideCareMedication(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet
import Expect
import Html
import Pages.Prenatal.ProgressReport.View exposing (viewTreatmentForOutsideCareDiagnosis)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Time exposing (Month(..))
import Translate exposing (translate)
import Translate.Model exposing (Language(..))



-- The outside-care diagnosis line on the progress report. Each diagnosis
-- lists only the medicines of its own question, as the form stores it.
-- Medicines of another illness are recorded alongside to catch a wrong list.


render : PrenatalDiagnosis -> List OutsideCareMedication -> Query.Single msg
render diagnosis medications =
    viewTreatmentForOutsideCareDiagnosis English
        (Date.fromCalendarDate 2026 Oct 1)
        (Just (EverySet.fromList medications))
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


all : Test
all =
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
        ]
