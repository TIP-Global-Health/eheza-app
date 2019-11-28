module Backend.Session.Fetch exposing (fetchEditableSession)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


{-| Given a sessionId, what messages will we need to send in
order to successfully construct an `EditableSession`?
-}
fetchEditableSession : SessionId -> ModelIndexedDb -> List MsgIndexedDb
fetchEditableSession sessionId db =
    let
        -- We allow passing the `batch`, so we could lazy load items, without trying to get them all at once.
        getIds property batch =
            Dict.foldl
                (\k v accum ->
                    if List.length accum >= batch then
                        accum

                    else if RemoteData.isNotAsked v then
                        k :: accum

                    else
                        accum
                )
                []
                property

        fetchPeople =
            [ FetchPeople <| getIds db.people 1000 ]

        fetchChildrenMeasurements =
            [ FetchChildrenMeasurements <| getIds db.childMeasurements 1000 ]

        fetchMothersMeasurements =
            [ FetchMothersMeasurements <| getIds db.motherMeasurements 1000 ]

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
        , fetchMothersMeasurements
        , fetchChildrenMeasurements
        , fetchPeople
        ]
