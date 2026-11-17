module Pages.Prenatal.ProgressReport.Utils exposing (allCHWActions, chwActionToColor, diagnosisForProgressReportToString, generateAllNurseEncountersData, resolveMedicationHistoryDate, thumbnailDimensions, updateChronicHypertensionDiagnoses, wrapWithLI)

import Backend.Measurement.Model exposing (AdministrationNote(..), MedicationDistributionSign(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.Prenatal.Model exposing (AssembledData, PreviousEncounterData)
import Pages.Prenatal.ProgressReport.Model exposing (CHWAction(..))
import Pages.Prenatal.Utils
    exposing
        ( applyHypertensionlikeDiagnosesHierarchy
        , mebendazoleDistributed
        , resolvePreviousHypertensionDiagnosis
        )
import Pages.Utils exposing (filterPreviousEncountersDataToDate)
import Translate exposing (Language, translate)
import Utils.NominalDate exposing (sortByDateDesc)


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
    filterPreviousEncountersDataToDate encounterDate assembled.nursePreviousEncountersData
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


generateAllNurseEncountersData : Bool -> AssembledData -> List PreviousEncounterData
generateAllNurseEncountersData isChw assembled =
    assembled.nursePreviousEncountersData
        ++ (if isChw then
                []

            else
                [ { startDate = assembled.encounter.startDate
                  , diagnoses = assembled.encounter.diagnoses
                  , pastDiagnoses = assembled.encounter.pastDiagnoses
                  , measurements = assembled.measurements
                  }
                ]
           )


{-| The Medication History date of a medication: the latest nurse visit
where it was handed over, in the Medication activity or, for Mebendazole,
in Next Steps.
-}
resolveMedicationHistoryDate : Bool -> AssembledData -> MedicationDistributionSign -> Maybe NominalDate
resolveMedicationHistoryDate isChw assembled medication =
    let
        dateGiven routes =
            generateAllNurseEncountersData isChw assembled
                |> List.sortWith (sortByDateDesc .startDate)
                |> List.filterMap
                    (\{ measurements } ->
                        List.filterMap (\route -> route measurements) routes
                            |> List.head
                    )
                |> List.head

        administered measurementFunc measurements =
            measurementFunc measurements
                |> Maybe.andThen
                    (\( _, measurement ) ->
                        if measurement.value == AdministeredToday then
                            Just measurement.dateMeasured

                        else
                            Nothing
                    )

        distributedMebendazole measurements =
            if mebendazoleDistributed measurements then
                Maybe.map (Tuple.second >> .dateMeasured) measurements.medicationDistribution

            else
                Nothing
    in
    case medication of
        Calcium ->
            dateGiven [ administered .calcium ]

        Fefol ->
            dateGiven [ administered .fefol ]

        FolicAcid ->
            dateGiven [ administered .folate ]

        Iron ->
            dateGiven [ administered .iron ]

        Mebendezole ->
            dateGiven [ administered .mebendazole, distributedMebendazole ]

        MMS ->
            dateGiven [ administered .mms ]

        _ ->
            Nothing
