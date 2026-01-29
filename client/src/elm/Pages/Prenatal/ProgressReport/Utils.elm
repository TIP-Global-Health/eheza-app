module Pages.Prenatal.ProgressReport.Utils exposing (allCHWActions, chwActionToColor, diagnosisForProgressReportToString, thumbnailDimensions, updateChronicHypertensionDiagnoses, wrapWithLI)

import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..))
import Pages.Prenatal.Utils
    exposing
        ( applyHypertensionlikeDiagnosesHierarchy
        , resolvePreviousHypertensionDiagnosis
        )
import Translate exposing (Language, translate)


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
