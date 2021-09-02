module Backend.AcuteIllnessEncounter.Utils exposing (..)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Maybe.Extra
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


acuteIllnessDiagnosisToString : AcuteIllnessDiagnosis -> String
acuteIllnessDiagnosisToString diagnosis =
    case diagnosis of
        DiagnosisCovid19 ->
            "covid19"

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
            Just DiagnosisCovid19

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


progressReportInitiatorToUrlFragmemt : AcuteIllnessProgressReportInitiator -> String
progressReportInitiatorToUrlFragmemt initiator =
    case initiator of
        InitiatorEncounterPage ->
            "encounter-page"

        InitiatorIndividualNutritionProgressReport encounterId ->
            "nutrition-progress-report-" ++ fromEntityUuid encounterId

        InitiatorWellChildProgressReport encounterId ->
            "well-child-progress-report-" ++ fromEntityUuid encounterId

        InitiatorGroupNutritionProgressReport sessionId personId ->
            "progress-report-" ++ fromEntityUuid sessionId ++ "+++" ++ fromEntityUuid personId


progressReportInitiatorFromUrlFragmemt : String -> Maybe AcuteIllnessProgressReportInitiator
progressReportInitiatorFromUrlFragmemt s =
    case s of
        "encounter-page" ->
            Just InitiatorEncounterPage

        _ ->
            if String.startsWith "well-child-progress-report" s then
                String.dropLeft (String.length "well-child-progress-report-") s
                    |> toEntityUuid
                    |> InitiatorWellChildProgressReport
                    |> Just

            else if String.startsWith "nutrition-progress-report" s then
                String.dropLeft (String.length "nutrition-progress-report-") s
                    |> toEntityUuid
                    |> InitiatorIndividualNutritionProgressReport
                    |> Just

            else if String.startsWith "progress-report" s then
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

            else
                Nothing
