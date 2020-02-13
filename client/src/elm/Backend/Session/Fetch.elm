module Backend.Session.Fetch exposing (fetchEditableSession)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import RemoteData exposing (RemoteData(..))


{-| Given a sessionId, what messages will we need to send in
order to successfully construct an `EditableSession`?
-}
fetchEditableSession : SessionId -> ModelIndexedDb -> List MsgIndexedDb
fetchEditableSession sessionId db =
    let
        participantData =
            EveryDict.get sessionId db.expectedParticipants
                |> Maybe.withDefault NotAsked

        childrenIdData =
            RemoteData.map
                (.byChildId >> EveryDict.keys)
                participantData

        motherIdData =
            RemoteData.map
                (.byMotherId >> EveryDict.keys)
                participantData

        -- It would be more efficient here to have messages that could fetch a
        -- whole bunch of people at once. However, since we're talking to
        -- IndexedDb, it's unlikely to make any noticeable difference in
        -- practice. We could look at it if there is any perceptible delay.
        fetchChildren =
            childrenIdData
                |> RemoteData.map (List.map FetchPerson)
                |> RemoteData.withDefault []

        fetchMothers =
            motherIdData
                |> RemoteData.map (List.map FetchPerson)
                |> RemoteData.withDefault []

        fetchChildMeasurements =
            childrenIdData
                |> RemoteData.map (List.map FetchChildMeasurements)
                |> RemoteData.withDefault []

        fetchMotherMeasurements =
            motherIdData
                |> RemoteData.map (List.map FetchMotherMeasurements)
                |> RemoteData.withDefault []

        alwaysFetch =
            [ FetchSession sessionId
            , FetchClinics
            , FetchEveryCounselingSchedule
            , FetchParticipantForms
            , FetchExpectedParticipants sessionId
            ]
    in
    List.concat
        [ alwaysFetch
        , fetchMotherMeasurements
        , fetchChildMeasurements
        , fetchMothers
        , fetchChildren
        ]
