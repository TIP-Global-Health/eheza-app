module Backend.EducationSession.Utils exposing (applyUpdateToSessions, educationTopicFromString, educationTopicToString)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Model exposing (EducationSession, EducationTopic(..), Msg(..))
import Backend.Entities exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


{-| Applies an education session update to the sessions dict we hold at
ModelIndexedDb, right when the update is dispatched.

Session update is performed by a full entity PATCH, which is rebuilt from the
session we hold at that dict. The dict is refreshed when the revision for the
PATCH echoes back from the service worker, which happens asynchronously.
Therefore, we apply the update to the dict right away. Otherwise, an update
that is triggered before the echo of its predecessor lands would rebuild the
entity from the pre-update session, and revert the field that predecessor had
set. For example, ending the session right after the last participant was
checked in would drop that participant.

-}
applyUpdateToSessions :
    EducationSessionId
    -> Msg
    -> Dict EducationSessionId (WebData EducationSession)
    -> Dict EducationSessionId (WebData EducationSession)
applyUpdateToSessions sessionId msg sessions =
    case msg of
        HandleUpdated _ ->
            sessions

        Update updateFunc ->
            Dict.get sessionId sessions
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (\session ->
                        Dict.insert sessionId (Success <| updateFunc session) sessions
                    )
                |> Maybe.withDefault sessions


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
