module Pages.Prenatal.ProgressReport.Test exposing (all)

import Backend.Measurement.Model exposing (OutsideCareMedication(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date
import EverySet
import Html
import Pages.Prenatal.ProgressReport.View exposing (viewTreatmentForOutsideCareDiagnosis)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
import Time exposing (Month(..))
import Translate.Model exposing (Language(..))



-- The outside-care diagnosis line on the progress report. The form stores
-- syphilis and moderate anemia at their recurrent phase, and HIV's own
-- "None of these" must read as no treatment.


render : PrenatalDiagnosis -> List OutsideCareMedication -> Query.Single msg
render diagnosis medications =
    viewTreatmentForOutsideCareDiagnosis English
        (Date.fromCalendarDate 2026 Oct 1)
        (Just (EverySet.fromList medications))
        diagnosis
        |> Html.div []
        |> Query.fromHtml


all : Test
all =
    describe "outside care treatment phrase"
        [ test "syphilis (as the form stores it) shows the medicine" <|
            \_ -> render DiagnosisSyphilisRecurrentPhase [ OutsideCareMedicationPenecilin1 ] |> Query.has [ text "treated with" ]
        , test "moderate anemia (as the form stores it) shows the medicine" <|
            \_ -> render DiagnosisModerateAnemiaRecurrentPhase [ OutsideCareMedicationIron1 ] |> Query.has [ text "treated with" ]
        , test "HIV with None of these reads no treatment" <|
            \_ -> render DiagnosisHIVInitialPhase [ NoOutsideCareMedicationForHIV ] |> Query.has [ text "no treatment administered" ]
        , test "HIV with None of these is not 'treated with'" <|
            \_ -> render DiagnosisHIVInitialPhase [ NoOutsideCareMedicationForHIV ] |> Query.hasNot [ text "treated with" ]
        , test "HIV with TDF+3TC still shows the medicine" <|
            \_ -> render DiagnosisHIVInitialPhase [ OutsideCareMedicationTDF3TC ] |> Query.has [ text "treated with" ]
        ]
