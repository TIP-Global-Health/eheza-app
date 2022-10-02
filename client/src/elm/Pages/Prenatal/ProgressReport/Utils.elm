module Pages.Prenatal.ProgressReport.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDateDesc, sortTuplesByDateDesc)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator(..))
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInYears)
import Backend.PrenatalActivity.Model
    exposing
        ( PregnancyTrimester(..)
        , allMedicalDiagnoses
        , allObstetricalDiagnoses
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
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils
    exposing
        ( outsideCareMedicationOptionsAnemia
        , outsideCareMedicationOptionsHIV
        , outsideCareMedicationOptionsHypertension
        , outsideCareMedicationOptionsMalaria
        , outsideCareMedicationOptionsSyphilis
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Utils exposing (respiratoryRateElevated)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Prenatal.ProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.RecurrentEncounter.Utils
import Pages.Prenatal.Utils
    exposing
        ( applyHypertensionlikeDiagnosesHierarchy
        , diagnosedMalaria
        , hypertensionDiagnoses
        , outsideCareDiagnoses
        , recommendedTreatmentSignsForHypertension
        , recommendedTreatmentSignsForMalaria
        , recommendedTreatmentSignsForSyphilis
        , resolvePreviousHypertensionDiagnosis
        )
import Pages.Report.Model exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), TestReport(..))
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


updateChronicHypertensionDiagnoses : NominalDate -> EverySet PrenatalDiagnosis -> AssembledData -> List PrenatalDiagnosis -> List PrenatalDiagnosis
updateChronicHypertensionDiagnoses encounterDate encounterDiagnoses assembled filterList =
    -- We want to be looking at encounters performed
    -- before the encounter we're processing, to be able to locate
    -- previous chronic diagnosis.
    filterNursePreviousEncountersDataToDate encounterDate assembled.nursePreviousEncountersData
        |> resolvePreviousHypertensionDiagnosis
        |> Maybe.map
            (\previousHypertensionDiagnosis ->
                EverySet.insert previousHypertensionDiagnosis encounterDiagnoses
                    |> -- Adding this to avoid a situation where we have 2 hypetensionlike diagnoses.
                       -- For example, if we had chronic Moderate Preeclampsia, and at current
                       -- encounter we diagnosed Severe Preeclampsia.
                       applyHypertensionlikeDiagnosesHierarchy
            )
        |> Maybe.withDefault encounterDiagnoses
        |> EverySet.toList
        |> List.filter (\diagnosis -> List.member diagnosis filterList)


filterNursePreviousEncountersDataToDate :
    NominalDate
    -> List PreviousEncounterData
    -> List PreviousEncounterData
filterNursePreviousEncountersDataToDate limitDate nursePreviousEncountersData =
    List.filter
        (\data ->
            Date.compare data.startDate limitDate == LT
        )
        nursePreviousEncountersData


diagnosisForProgressReportToString : Language -> PrenatalDiagnosis -> String
diagnosisForProgressReportToString language diagnosis =
    translate language <| Translate.PrenatalDiagnosisForProgressReport diagnosis


wrapWithLI : String -> List (Html any)
wrapWithLI =
    text >> List.singleton >> li [] >> List.singleton


chwActionToColor : CHWAction -> String
chwActionToColor action =
    case action of
        ActionPregnancyDating ->
            "purple"

        ActionLabs ->
            "purple"

        ActionDangerSignsPresent ->
            "velvet"

        ActionReferredToHealthCenter ->
            "velvet"

        ActionAppointmentConfirmation ->
            "cyan"

        ActionHealthEducation ->
            "cyan"

        ActionBirthPlan ->
            "cyan"


allCHWActions : List CHWAction
allCHWActions =
    [ ActionPregnancyDating
    , ActionLabs
    , ActionDangerSignsPresent
    , ActionReferredToHealthCenter
    , ActionAppointmentConfirmation
    , ActionHealthEducation
    , ActionBirthPlan
    ]
