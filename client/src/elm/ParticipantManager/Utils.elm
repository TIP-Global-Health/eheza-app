module ParticipantManager.Utils
    exposing
        ( getChildren
        , getMother
        , getParticipant
        , wrapParticipantsDict
        , unwrapParticipantsDict
        )

import Child.Model exposing (Child, ChildId)
import Dict exposing (Dict)
import Mother.Model exposing (Mother, MotherId)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantsDict)
import ParticipantManager.Model as ParticipantManager
import RemoteData exposing (RemoteData(..), WebData)


getChildren : Mother -> ParticipantManager.Model -> List (WebData ( ChildId, Child ))
getChildren mother model =
    List.map
        (\childId ->
            getParticipant childId model
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
            case getParticipant motherId model of
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
