module Pages.Session.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Session.Fetch exposing (fetchEditableSession)
import Pages.Activities.Fetch
import Pages.Activity.Fetch
import Pages.Attendance.Fetch
import Pages.Page exposing (SessionPage(..))
import Pages.Participant.Fetch
import Pages.Participants.Fetch
import Pages.ProgressReport.Fetch


fetch : SessionId -> SessionPage -> ModelIndexedDb -> List MsgIndexedDb
fetch sessionId sessionPage db =
    let
        forSessionPage =
            case sessionPage of
                ActivityPage _ ->
                    Pages.Activity.Fetch.fetch sessionId

                ActivitiesPage ->
                    Pages.Activities.Fetch.fetch sessionId

                AttendancePage ->
                    Pages.Attendance.Fetch.fetch sessionId

                ChildPage _ ->
                    Pages.Participant.Fetch.fetch sessionId

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
    forSessionPage ++ forEditableSession ++ [ FetchEditableSession sessionId ]
