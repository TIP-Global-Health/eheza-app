module Pages.Session.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Session.Fetch exposing (fetchEditableSession)
import Pages.Page exposing (SessionPage(..))
import Pages.ProgressReport.Fetch


fetch : SessionId -> SessionPage -> ModelIndexedDb -> List MsgIndexedDb
fetch sessionId sessionPage db =
    let
        forSessionPage =
            case sessionPage of
                ProgressReportPage childId ->
                    Pages.ProgressReport.Fetch.fetch childId

                _ ->
                    []

        -- We gather all the msgs needed to construct an editable session, and
        -- also the message that indicates that we want the EditableSession
        -- itself, i.e. that particular organization of the session data.
        forEditableSession =
            fetchEditableSession sessionId db
    in
    forSessionPage ++ forEditableSession ++ [ FetchEditableSession sessionId ]
