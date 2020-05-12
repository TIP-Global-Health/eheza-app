module Pages.Session.Fetch exposing (fetch)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import Activity.Utils
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Session.Fetch exposing (fetchEditableSession)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Pages.Activities.Fetch
import Pages.Activity.Fetch
import Pages.Attendance.Fetch
import Pages.NutritionEncounter.Fetch
import Pages.Page exposing (SessionPage(..))
import Pages.Participant.Fetch
import Pages.Participants.Fetch
import Pages.ProgressReport.Fetch
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> SessionId -> SessionPage -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate sessionId sessionPage db =
    let
        ( forSessionPage, calculations ) =
            case sessionPage of
                ActivityPage activity ->
                    let
                        maybeChildActivity =
                            case activity of
                                ChildActivity Height ->
                                    Just Height

                                ChildActivity Muac ->
                                    Just Muac

                                ChildActivity Weight ->
                                    Just Weight

                                _ ->
                                    Nothing

                        firstPendingParticipant =
                            maybeChildActivity
                                |> Maybe.andThen
                                    (\childActivity ->
                                        Dict.get sessionId db.editableSessions
                                            |> Maybe.withDefault NotAsked
                                            |> RemoteData.toMaybe
                                            |> Maybe.andThen
                                                (\ediatbleSession ->
                                                    ediatbleSession.checkedIn
                                                        |> LocalData.unwrap
                                                            { completed = Dict.empty
                                                            , pending = Dict.empty
                                                            }
                                                            (Activity.Utils.summarizeChildActivity currentDate childActivity ediatbleSession.offlineSession False)
                                                        |> .pending
                                                        |> Dict.toList
                                                        |> List.head
                                                        |> Maybe.map Tuple.first
                                                )
                                    )

                        fetchIndividualDataMsgs =
                            firstPendingParticipant
                                |> Maybe.map (\childId -> Pages.NutritionEncounter.Fetch.fetchForChild childId db)
                                |> Maybe.withDefault []

                        ( forActivityPage, activityPageCalculations ) =
                            Pages.Activity.Fetch.fetch sessionId
                    in
                    ( forActivityPage ++ fetchIndividualDataMsgs, activityPageCalculations )

                ActivitiesPage ->
                    Pages.Activities.Fetch.fetch sessionId

                AttendancePage ->
                    Pages.Attendance.Fetch.fetch sessionId

                ChildPage childId ->
                    let
                        ( forChildPage, childPageCalculations ) =
                            Pages.Participant.Fetch.fetch sessionId
                    in
                    ( forChildPage ++ Pages.NutritionEncounter.Fetch.fetchForChild childId db, childPageCalculations )

                MotherPage _ ->
                    Pages.Participant.Fetch.fetch sessionId

                ParticipantsPage ->
                    Pages.Participants.Fetch.fetch sessionId

                ProgressReportPage childId ->
                    Pages.ProgressReport.Fetch.fetch childId

        -- We gather all the msgs needed to construct an editable session, and
        -- also the message that indicates that we want the EditableSession
        -- itself, i.e. that particular organization of the session data.
        forEditableSession =
            fetchEditableSession sessionId db
    in
    -- We send 'calculations' in 2 places here:
    -- 1. To be sent after 'FetchEditableSession' is completed, which may bring
    --    new editableSession, with all LocalData parts as NotNeeded.
    -- 2. Separatly, for case when 'FetchEditableSession' will not do anything
    --    as it's already fetched.
    forSessionPage ++ calculations ++ forEditableSession ++ [ FetchEditableSession sessionId calculations ]
