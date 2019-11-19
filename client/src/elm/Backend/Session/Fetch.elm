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
        peopleIds =
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
                db.people

        fetchPeople =
            [ FetchPeople peopleIds ]

        --        fetchChildMeasurements =
        --            childrenIdData
        --                |> RemoteData.map (List.map FetchChildMeasurements)
        --                |> RemoteData.withDefault []
        --
        --        fetchMotherMeasurements =
        --            motherIdData
        --                |> RemoteData.map (List.map FetchMotherMeasurements)
        --                |> RemoteData.withDefault []
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

        --        , fetchMotherMeasurements
        --        , fetchChildMeasurements
        , fetchPeople
        ]
