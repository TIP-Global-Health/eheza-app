module Backend.EducationSession.Utils exposing (..)

import Backend.EducationSession.Model exposing (EducationTopic(..))


educationTopicToString : EducationTopic -> String
educationTopicToString topic =
    case topic of
        TopicTuberculosis ->
            "tuberculosis"

        TopicSTD ->
            "std"

        TopicMentalHealth ->
            "mental-health"

        TopicMalaria ->
            "malaria"

        TopicChildhoodIllnesses ->
            "childhood-illnesses"

        TopicMalnutrition ->
            "malnutrition"

        TopicANCPostpartum ->
            "anc-postpartum"

        TopicFamilyPlanning ->
            "family-planning"

        TopicGender ->
            "gender"

        TopicNCD ->
            "ncd"


educationTopicFromString : String -> Maybe EducationTopic
educationTopicFromString str =
    case str of
        "tuberculosis" ->
            Just TopicTuberculosis

        "std" ->
            Just TopicSTD

        "mental-health" ->
            Just TopicMentalHealth

        "malaria" ->
            Just TopicMalaria

        "childhood-illnesses" ->
            Just TopicChildhoodIllnesses

        "malnutrition" ->
            Just TopicMalnutrition

        "anc-postpartum" ->
            Just TopicANCPostpartum

        "family-planning" ->
            Just TopicFamilyPlanning

        "gender" ->
            Just TopicGender

        "ncd" ->
            Just TopicNCD

        _ ->
            Nothing
