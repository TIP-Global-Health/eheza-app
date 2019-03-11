module Pages.ParticipantRegistration.Fetch exposing (fetch)

import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import Maybe.Extra
import Pages.ParticipantRegistration.Model exposing (..)
import Participant.Model exposing (ParticipantId(..), ParticipantType(..))
import RemoteData exposing (RemoteData(..))


fetch : ModelIndexedDb -> Model -> List MsgIndexedDb
fetch db model =
    let
        forDialog =
            case model.dialogState of
                Just (Registering participant) ->
                    case participant of
                        ChildParticipant _ ->
                            db.postChild
                                |> RemoteData.toMaybe
                                |> Maybe.map FetchChild
                                |> Maybe.Extra.toList

                        MotherParticipant _ ->
                            []

                _ ->
                    []

        forPhase =
            case model.registrationPhase of
                ParticipantSearch _ ->
                    let
                        trimmed =
                            model.submittedSearch
                                |> Maybe.withDefault ""
                                |> String.trim
                    in
                    if trimmed == "" then
                        []

                    else
                        [ FetchParticipantsByName trimmed
                        , FetchHealthCenters
                        ]

                ParticipantView participantId ->
                    case participantId of
                        ParticipantChild childId ->
                            let
                                fetchMother =
                                    EveryDict.get childId db.children
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.andThen .motherId
                                        |> Maybe.map FetchMother
                            in
                            List.filterMap identity
                                [ Just <| FetchChild childId
                                , Just FetchHealthCenters
                                , fetchMother
                                ]

                        ParticipantMother motherId ->
                            [ FetchMother motherId
                            , FetchChildrenOfMother motherId
                            , FetchHealthCenters
                            ]

                ParticipantRegistration step ->
                    [ FetchClinics
                    , FetchHealthCenters
                    ]
    in
    forDialog ++ forPhase
