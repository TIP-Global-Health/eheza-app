module Pages.Session.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (emptyMotherMeasurementData, getMotherMeasurementData)
import Maybe.Extra
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Pages.Activities.Update
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Attendance.Update
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Update
import Pages.Session.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Utils.Upgrade exposing (force)


update : SessionId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update sessionId db msg model =
    let
        sessionData =
            Dict.get sessionId db.editableSessions
                |> Maybe.withDefault NotAsked
    in
    case sessionData of
        Success session ->
            updateFoundSession sessionId session msg model

        _ ->
            -- We're handling UI messages here, and the UI should only be shown if
            -- we have an editable session, so this shouldn't really happen. But
            -- perhaps we should log some kind of error if it does?
            ( model, Cmd.none, [] )


{-| We need the editableSession in order to pass on some needed data. But we
don't modify it directly ... instead, we return messages to do so.
-}
updateFoundSession : SessionId -> EditableSession -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
updateFoundSession sessionId session msg model =
    case msg of
        MsgActivities subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Pages.Activities.Update.update session subMsg model.activitiesPage
            in
            ( { model | activitiesPage = subModel }
            , Cmd.map MsgActivities subCmd
            , List.map (App.Model.MsgLoggedIn << App.Model.MsgPageSession sessionId) extraMsgs
            )

        MsgAttendance subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Pages.Attendance.Update.update subMsg model.attendancePage
            in
            ( { model | attendancePage = subModel }
            , Cmd.map MsgAttendance subCmd
            , List.map (App.Model.MsgLoggedIn << App.Model.MsgPageSession sessionId) extraMsgs
            )

        -- TODO: Figure out whether maybeChildId really needs to be a Maybe.
        MsgChildActivity activityType maybeChildId subMsg ->
            let
                activityPage =
                    Dict.get activityType model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                childForm =
                    Maybe.map (\childId -> getChildForm childId model session) maybeChildId

                updateReturns =
                    Pages.Activity.Update.updateChild subMsg activityPage childForm

                sessionMsgs =
                    maybeChildId
                        |> Maybe.map
                            (\childId ->
                                [ Maybe.map (Backend.Session.Model.MeasurementOutMsgChild childId) updateReturns.outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgIndexedDb << Backend.Model.MsgSession sessionId)
                            )
                        |> Maybe.withDefault []

                childForms =
                    Maybe.map2 (\childId form -> Dict.insert childId form model.childForms) maybeChildId updateReturns.form
                        |> Maybe.withDefault model.childForms

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage updateReturns.page
                        |> Maybe.Extra.toList
            in
            -- So, to summarize
            --
            -- - we own the subModel, subCmd, and subForm, so we handle them normally
            -- - we turn the redirect page into a message, if provided
            -- - we send a message to implement the OutMsg, if provided
            ( { model
                | childActivityPages = Dict.insert activityType updateReturns.model model.childActivityPages
                , childForms = childForms
              }
            , Cmd.map (MsgChildActivity activityType maybeChildId) updateReturns.cmd
            , redirectMsgs ++ sessionMsgs
            )

        -- TODO: Figure out whether `maybeMotherId` must be a Maybe.
        MsgMotherActivity activityType maybeMotherId subMsg ->
            let
                activityPage =
                    Dict.get activityType model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                motherForm =
                    Maybe.map (\motherId -> getMotherForm motherId model session) maybeMotherId

                measurements =
                    maybeMotherId
                        |> Maybe.map (\motherId -> force <| getMotherMeasurementData motherId session)
                        |> Maybe.withDefault (emptyMotherMeasurementData session)

                updateReturns =
                    Pages.Activity.Update.updateMother subMsg activityPage motherForm measurements

                sessionMsgs =
                    maybeMotherId
                        |> Maybe.map
                            (\motherId ->
                                [ Maybe.map (Backend.Session.Model.MeasurementOutMsgMother motherId) updateReturns.outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgIndexedDb << Backend.Model.MsgSession sessionId)
                            )
                        |> Maybe.withDefault []

                motherForms =
                    Maybe.map2 (\motherId form -> Dict.insert motherId form model.motherForms) maybeMotherId updateReturns.form
                        |> Maybe.withDefault model.motherForms

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage updateReturns.page
                        |> Maybe.Extra.toList
            in
            -- So, to summarize
            --
            -- - we own the subModel and subCmd, so we handle them normally
            -- - the EditableSession owns the subForm, so we send a message to update that
            -- - we turn the redirect page into a message, if provided
            -- - we send a message to implement the OutMsg, if provided
            ( { model
                | motherActivityPages = Dict.insert activityType updateReturns.model model.motherActivityPages
                , motherForms = motherForms
              }
            , Cmd.map (MsgMotherActivity activityType maybeMotherId) updateReturns.cmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgChild childId subMsg ->
            let
                childForm =
                    getChildForm childId model session

                childPage =
                    Dict.get childId model.childPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateChild subMsg childPage childForm

                sessionMsgs =
                    List.map (App.Model.MsgIndexedDb << Backend.Model.MsgSession sessionId)
                        (Maybe.Extra.toList (Maybe.map (Backend.Session.Model.MeasurementOutMsgChild childId) outMsg))

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
            -- So, to summarize
            --
            -- - we own the subModel, subCmd, and subForm so we handle them normally
            -- - we turn the redirect page into a message, if provided
            -- - we send a message to implement the OutMsg, if provided
            ( { model
                | childPages = Dict.insert childId subModel model.childPages
                , childForms = Dict.insert childId subForm model.childForms
              }
            , Cmd.map (MsgChild childId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgSession subMsg ->
            -- Just route it over to the backend ...
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb <|
                    Backend.Model.MsgSession sessionId subMsg
              ]
            )

        MsgMother motherId subMsg ->
            let
                motherForm =
                    getMotherForm motherId model session

                motherPage =
                    Dict.get motherId model.motherPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                measurements =
                    getMotherMeasurementData motherId session

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateMother subMsg motherPage motherForm (force measurements)

                sessionMsgs =
                    List.map (App.Model.MsgIndexedDb << Backend.Model.MsgSession sessionId)
                        (Maybe.Extra.toList (Maybe.map (Backend.Session.Model.MeasurementOutMsgMother motherId) outMsg))

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
            -- So, to summarize
            --
            -- - we own the subModel, subCmd, and subForm, so we handle them normally
            -- - we turn the redirect page into a message, if provided
            -- - we send a message to implement the OutMsg, if provided
            ( { model
                | motherPages = Dict.insert motherId subModel model.motherPages
                , motherForms = Dict.insert motherId subForm model.motherForms
              }
            , Cmd.map (MsgMother motherId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgParticipants subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Pages.Participants.Update.update session subMsg model.participantsPage
            in
            ( { model | participantsPage = subModel }
            , Cmd.map MsgParticipants subCmd
            , List.map (App.Model.MsgLoggedIn << App.Model.MsgPageSession sessionId) extraMsgs
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )
