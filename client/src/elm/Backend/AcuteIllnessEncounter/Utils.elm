module Backend.AcuteIllnessEncounter.Utils exposing (..)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.NCDEncounter.Utils
import Backend.PatientRecord.Utils
import Maybe.Extra
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


acuteIllnessDiagnosisToString : AcuteIllnessDiagnosis -> String
acuteIllnessDiagnosisToString diagnosis =
    case diagnosis of
        DiagnosisCovid19Suspect ->
            "covid19"

        DiagnosisSevereCovid19 ->
            "covid19-severe"

        DiagnosisPneuminialCovid19 ->
            "covid19-pneumonia"

        DiagnosisLowRiskCovid19 ->
            "covid19-low-risk"

        DiagnosisMalariaComplicated ->
            "malaria-complicated"

        DiagnosisMalariaUncomplicated ->
            "malaria-uncomplicated"

        DiagnosisMalariaUncomplicatedAndPregnant ->
            "malaria-uncomplicated-pregnant"

        DiagnosisGastrointestinalInfectionComplicated ->
            "gi-complicated"

        DiagnosisGastrointestinalInfectionUncomplicated ->
            "gi-uncomplicated"

        DiagnosisSimpleColdAndCough ->
            "cough-and-cold"

        DiagnosisRespiratoryInfectionComplicated ->
            "ri-complicated"

        DiagnosisRespiratoryInfectionUncomplicated ->
            "ri-uncomplicated"

        DiagnosisFeverOfUnknownOrigin ->
            "fever-of-unknown-origin"

        DiagnosisUndeterminedMoreEvaluationNeeded ->
            "undetermined"

        NoAcuteIllnessDiagnosis ->
            "none"


acuteIllnessDiagnosisFromString : String -> Maybe AcuteIllnessDiagnosis
acuteIllnessDiagnosisFromString diagnosis =
    case diagnosis of
        "covid19" ->
            Just DiagnosisCovid19Suspect

        "covid19-severe" ->
            Just DiagnosisSevereCovid19

        "covid19-pneumonia" ->
            Just DiagnosisPneuminialCovid19

        "covid19-low-risk" ->
            Just DiagnosisLowRiskCovid19

        "malaria-complicated" ->
            Just DiagnosisMalariaComplicated

        "malaria-uncomplicated" ->
            Just DiagnosisMalariaUncomplicated

        "malaria-uncomplicated-pregnant" ->
            Just DiagnosisMalariaUncomplicatedAndPregnant

        "gi-complicated" ->
            Just DiagnosisGastrointestinalInfectionComplicated

        "gi-uncomplicated" ->
            Just DiagnosisGastrointestinalInfectionUncomplicated

        "cough-and-cold" ->
            Just DiagnosisSimpleColdAndCough

        "ri-complicated" ->
            Just DiagnosisRespiratoryInfectionComplicated

        "ri-uncomplicated" ->
            Just DiagnosisRespiratoryInfectionUncomplicated

        "fever-of-unknown-origin" ->
            Just DiagnosisFeverOfUnknownOrigin

        "undetermined" ->
            Just DiagnosisUndeterminedMoreEvaluationNeeded

        "none" ->
            Just NoAcuteIllnessDiagnosis

        _ ->
            Nothing


progressReportInitiatorToUrlFragment : AcuteIllnessProgressReportInitiator -> String
progressReportInitiatorToUrlFragment initiator =
    case initiator of
        InitiatorEncounterPage ->
            "encounter-page"

        InitiatorIndividualNutritionProgressReport encounterId ->
            "nutrition-progress-report-" ++ fromEntityUuid encounterId

        InitiatorWellChildProgressReport encounterId ->
            "well-child-progress-report-" ++ fromEntityUuid encounterId

        InitiatorGroupNutritionProgressReport sessionId personId ->
            "progress-report-" ++ fromEntityUuid sessionId ++ "+++" ++ fromEntityUuid personId

        InitiatorPatientRecord patientRecordInitiator personId ->
            "patient-record-" ++ fromEntityUuid personId ++ "+++" ++ Backend.PatientRecord.Utils.progressReportInitiatorToUrlFragment patientRecordInitiator

        InitiatorNCDProgressReport ncdProgressReportInitiator ->
            "ncd-progress-report-" ++ Backend.NCDEncounter.Utils.progressReportInitiatorToUrlFragmemt ncdProgressReportInitiator


progressReportInitiatorFromUrlFragment : String -> Maybe AcuteIllnessProgressReportInitiator
progressReportInitiatorFromUrlFragment s =
    case s of
        "encounter-page" ->
            Just InitiatorEncounterPage

        _ ->
            if String.startsWith "well-child-progress-report-" s then
                String.dropLeft (String.length "well-child-progress-report-") s
                    |> toEntityUuid
                    |> InitiatorWellChildProgressReport
                    |> Just

            else if String.startsWith "nutrition-progress-report-" s then
                String.dropLeft (String.length "nutrition-progress-report-") s
                    |> toEntityUuid
                    |> InitiatorIndividualNutritionProgressReport
                    |> Just

            else if String.startsWith "progress-report-" s then
                let
                    ids =
                        String.dropLeft (String.length "progress-report-") s
                            |> String.split "+++"
                in
                -- In case of Group Nutrition report we need to know Session ID
                -- and Child ID. These 2 IDs are separated by '+++' string.
                -- '+' char is not used for UUIDs, so we have no risk of getting
                -- it wrong.
                if List.length ids /= 2 then
                    Nothing

                else
                    Maybe.map2
                        (\sessionId personId ->
                            Just <| InitiatorGroupNutritionProgressReport (toEntityUuid sessionId) (toEntityUuid personId)
                        )
                        (List.head ids)
                        (List.head (List.drop 1 ids))
                        |> Maybe.Extra.join

            else if String.startsWith "patient-record-" s then
                let
                    fragments =
                        String.dropLeft (String.length "patient-record-") s
                            |> String.split "+++"
                in
                if List.length fragments /= 2 then
                    Nothing

                else
                    Maybe.map2
                        (\personId patientRecordInitiator ->
                            Just <| InitiatorPatientRecord patientRecordInitiator (toEntityUuid personId)
                        )
                        (List.head fragments)
                        (List.drop 1 fragments
                            |> List.head
                            |> Maybe.andThen Backend.PatientRecord.Utils.progressReportInitiatorFromUrlFragment
                        )
                        |> Maybe.Extra.join

            else if String.startsWith "ncd-progress-report-" s then
                String.dropLeft (String.length "ncd-progress-report-") s
                    |> Backend.NCDEncounter.Utils.progressReportInitiatorFromUrlFragmemt
                    |> Maybe.map InitiatorNCDProgressReport

            else
                Nothing
