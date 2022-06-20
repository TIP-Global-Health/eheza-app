module Pages.Prenatal.ProgressReport.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, prenatalLabExpirationPeriod)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortTuplesByDateDesc)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnosis
        , allObstetricalDiagnosis
        , allRiskFactors
        , allTrimesters
        )
import Backend.PrenatalActivity.Utils
    exposing
        ( generateRiskFactorAlertData
        , getEncounterTrimesterData
        )
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalProgressReportInitiator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Interval(..), Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask(..))
import Pages.Prenatal.Activity.Utils
    exposing
        ( outsideCareMedicationOptionsAnemia
        , outsideCareMedicationOptionsHIV
        , outsideCareMedicationOptionsHypertension
        , outsideCareMedicationOptionsMalaria
        , outsideCareMedicationOptionsSyphilis
        , respiratoryRateElevated
        )
import Pages.Prenatal.DemographicsReport.View exposing (viewItemHeading)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (viewActionButton)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Prenatal.ProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.RecurrentEncounter.Utils
import Pages.Prenatal.Utils
    exposing
        ( diagnosedMalaria
        , hypertensionDiagnoses
        , outsideCareDiagnoses
        , outsideCareDiagnosesWithPossibleMedication
        , recommendedTreatmentSignsForHypertension
        , recommendedTreatmentSignsForMalaria
        , recommendedTreatmentSignsForSyphilis
        , resolvePreviousHypertensionDiagnosis
        )
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPhotoThumbFromPhotoUrl)
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


generateFilteredDiagnoses : NominalDate -> EverySet PrenatalDiagnosis -> AssembledData -> List PrenatalDiagnosis -> List PrenatalDiagnosis
generateFilteredDiagnoses encounterDate encounterDiagnoses assembled filterList =
    List.filter
        -- We want to be looking at encounters performed
        -- before the encounter we're processing, to be able to locate
        -- previous cronichal diagnosis.
        (\( date, _, _ ) ->
            Date.compare date encounterDate == LT
        )
        assembled.nursePreviousMeasurementsWithDates
        |> resolvePreviousHypertensionDiagnosis
        |> Maybe.map
            (\previousHypertensionDiagnosis ->
                EverySet.insert previousHypertensionDiagnosis encounterDiagnoses
            )
        |> Maybe.withDefault encounterDiagnoses
        |> EverySet.toList
        |> List.filter (\diagnosis -> List.member diagnosis filterList)


hivResultNormal : PrenatalTestReport -> Bool
hivResultNormal =
    prenatalTestReportNormal


syphilisResultNormal : PrenatalTestResult -> Bool
syphilisResultNormal =
    prenatalTestResultNormal


hepatitisBResultNormal : PrenatalTestReport -> Bool
hepatitisBResultNormal =
    prenatalTestReportNormal


malariaResultNormal : PrenatalTestResult -> Bool
malariaResultNormal =
    prenatalTestResultNormal


proteinResultNormal : ProteinValue -> Bool
proteinResultNormal value =
    -- @todo
    True


phResultNormal : PHValue -> Bool
phResultNormal value =
    -- @todo
    True


glucoseResultNormal : GlucoseValue -> Bool
glucoseResultNormal value =
    -- @todo
    True


leukocytesResultNormal : LeukocytesValue -> Bool
leukocytesResultNormal value =
    -- @todo
    True


nitriteResultNormal : NitriteValue -> Bool
nitriteResultNormal value =
    -- @todo
    True


urobilinogenResultNormal : UrobilinogenValue -> Bool
urobilinogenResultNormal value =
    -- @todo
    True


urineHaemoglobinValueResultNormal : HaemoglobinValue -> Bool
urineHaemoglobinValueResultNormal value =
    -- @todo
    True


specificGravityResultNormal : SpecificGravityValue -> Bool
specificGravityResultNormal value =
    -- @todo
    True


ketoneResultNormal : KetoneValue -> Bool
ketoneResultNormal value =
    -- @todo
    True


bilirubinResultNormal : BilirubinValue -> Bool
bilirubinResultNormal value =
    -- @todo
    True


randomBloodSugarResultNormal : Float -> Bool
randomBloodSugarResultNormal value =
    -- @todo
    True


hemoglobinResultNormal : Float -> Bool
hemoglobinResultNormal value =
    -- @todo
    True


prenatalTestReportNormal : PrenatalTestReport -> Bool
prenatalTestReportNormal report =
    case report of
        TestPerformed result ->
            prenatalTestResultNormal result

        TestNotPerformedKnownAsPositive ->
            True


prenatalTestResultNormal : PrenatalTestResult -> Bool
prenatalTestResultNormal =
    (==) PrenatalTestNegative


diagnosisForProgressReportToString : Language -> PrenatalDiagnosis -> String
diagnosisForProgressReportToString language diagnosis =
    translate language <| Translate.PrenatalDiagnosisForProgressReport diagnosis


wrapWithLI : String -> List (Html any)
wrapWithLI =
    text >> List.singleton >> li [] >> List.singleton
