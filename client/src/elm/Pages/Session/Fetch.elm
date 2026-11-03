module Pages.Session.Fetch exposing (fetch)

import Activity.Model exposing (Activity(..))
import Activity.Utils
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Session.Fetch exposing (fetchEditableSession)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Pages.Activities.Fetch
import Pages.Activity.Fetch
import Pages.Activity.Model
import Pages.Activity.Utils
import Pages.Attendance.Fetch
import Pages.Page exposing (SessionPage(..))
import Pages.Participant.Fetch
import Pages.Participants.Fetch
import Pages.ProgressReport.Fetch
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (SiteFeature)
import ZScore.Model


fetch :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> SessionId
    -> SessionPage
    -> ModelIndexedDb
    -> List MsgIndexedDb
fetch currentDate zscores features sessionId sessionPage db =
    let
        fetchForChild childId =
            let
                ( forChildPage, childPageCalculations ) =
                    Pages.Participant.Fetch.fetch sessionId
            in
            ( forChildPage ++ Backend.NutritionEncounter.Fetch.fetch childId db, childPageCalculations )

        ( forSessionPage, calculations ) =
            case sessionPage of
                ActivityPage activity ->
                    let
                        childToLoad =
                            case activity of
                                ChildActivity childActivity ->
                                    Dict.get sessionId db.editableSessions
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.andThen
                                            (\editableSession ->
                                                editableSession.checkedIn
                                                    |> LocalData.unwrap
                                                        { completed = Dict.empty
                                                        , pending = Dict.empty
                                                        }
                                                        (Activity.Utils.summarizeChildActivity currentDate
                                                            zscores
                                                            features
                                                            childActivity
                                                            editableSession.offlineSession
                                                            False
                                                            db
                                                        )
                                                    |> Pages.Activity.Utils.resolveChildToLoad childActivity Pages.Activity.Model.emptyModel
                                            )

                                _ ->
                                    Nothing

                        fetchIndividualDataMsgs =
                            childToLoad
                                |> Maybe.map (\childId -> Backend.NutritionEncounter.Fetch.fetch childId db)
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
                    fetchForChild childId

                MotherPage _ ->
                    Pages.Participant.Fetch.fetch sessionId

                NextStepsPage childId _ ->
                    fetchForChild childId

                ParticipantsPage ->
                    Pages.Participants.Fetch.fetch sessionId

                ProgressReportPage childId ->
                    Pages.ProgressReport.Fetch.fetch childId db

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
