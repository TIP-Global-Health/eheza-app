module ParticipantManager.Utils
    exposing
        ( getChildren
        , getMother
        , getParticipant
        , wrapParticipantsDict
        , unwrapParticipantsDict
        )

import Backend.Entities exposing (..)
import Backend.Child.Model exposing (Child)
import Backend.Mother.Model exposing (Mother)
import Dict exposing (Dict)
import Drupal.Restful exposing (fromNodeId, toNodeId)
import Participant.Model exposing (Participant, ParticipantsDict, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..))
import ParticipantManager.Model as ParticipantManager
import RemoteData exposing (RemoteData(..), WebData)


getChildren : Mother -> ParticipantManager.Model -> List (WebData ( ChildId, Child ))
getChildren mother model =
    List.map
        (\childId ->
            -- TODO: Fix up types to avoid `fromNodeId`
            getParticipant (fromNodeId childId) model
                |> getChild childId
        )
        mother.children


getChild : ChildId -> WebData Participant -> WebData ( ChildId, Child )
getChild childId participantWebData =
    case participantWebData of
        Success participant ->
            case participant.info of
                ParticipantChild child ->
                    Success ( childId, child )

                _ ->
                    NotAsked

        _ ->
            Loading


getMother : Maybe MotherId -> ParticipantManager.Model -> WebData Mother
getMother maybeMotherId model =
    Maybe.map
        (\motherId ->
            -- TODO: Fix up types so we don't need the `fromNodeId`
            case getParticipant (fromNodeId motherId) model of
                Success participant ->
                    case participant.info of
                        ParticipantMother mother ->
                            Success mother

                        _ ->
                            NotAsked

                _ ->
                    Loading
        )
        maybeMotherId
        |> Maybe.withDefault NotAsked


getParticipant : ParticipantId -> ParticipantManager.Model -> WebData Participant
getParticipant id model =
    Dict.get id model.participants
        |> Maybe.withDefault NotAsked


wrapParticipantsDict : ParticipantsDict -> Dict ParticipantId (WebData Participant)
wrapParticipantsDict =
    Dict.map (\_ participant -> Success participant)


unwrapParticipantsDict : Dict ParticipantId (WebData Participant) -> ParticipantsDict
unwrapParticipantsDict wrappedParticipantsDict =
    wrappedParticipantsDict
        |> Dict.foldl
            (\participantId wrappedParticipant accum ->
                case wrappedParticipant of
                    Success participant ->
                        ( participantId, participant ) :: accum

                    _ ->
                        accum
            )
            []
        |> Dict.fromList
