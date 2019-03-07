module Pages.PatientRegistration.Fetch exposing (fetch)

import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import Pages.PatientRegistration.Model exposing (..)
import Participant.Model exposing (ParticipantId(..))
import RemoteData exposing (RemoteData(..))


fetch : ModelIndexedDb -> Model -> List MsgIndexedDb
fetch db model =
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
                [ FetchParticipantsByName trimmed ]

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
                        , fetchMother
                        ]

                ParticipantMother motherId ->
                    [ FetchMother motherId
                    , FetchChildrenOfMother motherId
                    ]

        ParticipantRegistration step ->
            []
