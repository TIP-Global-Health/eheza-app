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
        getIds property =
            Dict.foldl
                (\k v accum ->
                    if List.length accum >= 1000 then
                        accum

                    else if RemoteData.isNotAsked v then
                        k :: accum

                    else
                        accum
                )
                []
                property

        fetchPeople =
            [ FetchPeople <| getIds db.people ]

        fetchChildrenMeasurements =
            [ FetchChildrenMeasurements <| getIds db.childMeasurements ]

        fetchMothersMeasurements =
            [ FetchMothersMeasurements <| getIds db.motherMeasurements ]

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
