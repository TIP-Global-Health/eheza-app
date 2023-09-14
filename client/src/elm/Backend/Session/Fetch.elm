module Backend.Session.Fetch exposing (fetchEditableSession)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Session.Model exposing (batchSize)
import RemoteData


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
            [ FetchPeople <| getIds db.people batchSize ]

        fetchChildrenMeasurements =
            [ FetchChildrenMeasurements <| getIds db.childMeasurements batchSize ]

        fetchMothersMeasurements =
            [ FetchMothersMeasurements <| getIds db.motherMeasurements batchSize ]

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
