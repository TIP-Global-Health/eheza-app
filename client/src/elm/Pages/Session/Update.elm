module Pages.Session.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..))
import Backend.Session.Utils exposing (emptyMotherMeasurementData, getMotherMeasurementData)
import EveryDict
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


update : SessionId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update sessionId msg model =
    --TODO
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
                    EveryDict.get activityType model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                childForm =
                    Maybe.map (\childId -> getChildForm childId model session) maybeChildId

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateChild subMsg activityPage childForm

                sessionMsgs =
                    maybeChildId
                        |> Maybe.map
                            (\childId ->
                                [ Maybe.map (MeasurementOutMsgChild childId) outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgIndexedDb << Backend.Model.MsgEditableSession sessionId)
                            )
                        |> Maybe.withDefault []

                childForms =
                    Maybe.map2 (\childId form -> EveryDict.insert childId form model.childForms) maybeChildId subForm
                        |> Maybe.withDefault model.childForms

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
                | childActivityPages = EveryDict.insert activityType subModel model.childActivityPages
                , childForms = childForms
              }
            , Cmd.map (MsgChildActivity activityType maybeChildId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        -- TODO: Figure out whether `maybeMotherId` must be a Maybe.
        MsgMotherActivity activityType maybeMotherId subMsg ->
            let
                activityPage =
                    EveryDict.get activityType model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                motherForm =
                    Maybe.map (\motherId -> getMotherForm motherId model session) maybeMotherId

                measurements =
                    maybeMotherId
                        |> Maybe.map (\motherId -> getMotherMeasurementData motherId session)
                        |> Maybe.withDefault (emptyMotherMeasurementData session)

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateMother subMsg activityPage motherForm measurements

                sessionMsgs =
                    maybeMotherId
                        |> Maybe.map
                            (\motherId ->
                                [ Maybe.map (MeasurementOutMsgMother motherId) outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgIndexedDb << Backend.Model.MsgEditableSession sessionId)
                            )
                        |> Maybe.withDefault []

                motherForms =
                    Maybe.map2 (\motherId form -> EveryDict.insert motherId form model.motherForms) maybeMotherId subForm
                        |> Maybe.withDefault model.motherForms

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
            -- So, to summarize
            --
            -- - we own the subModel and subCmd, so we handle them normally
            -- - the EditableSession owns the subForm, so we send a message to update that
            -- - we turn the redirect page into a message, if provided
            -- - we send a message to implement the OutMsg, if provided
            ( { model
                | motherActivityPages = EveryDict.insert activityType subModel model.motherActivityPages
                , motherForms = motherForms
              }
            , Cmd.map (MsgMotherActivity activityType maybeMotherId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgChild childId subMsg ->
            let
                childForm =
                    getChildForm childId model session

                childPage =
                    EveryDict.get childId model.childPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateChild subMsg childPage childForm

                sessionMsgs =
                    List.map (App.Model.MsgIndexedDb << Backend.Model.MsgEditableSession sessionId)
                        (Maybe.Extra.toList (Maybe.map (MeasurementOutMsgChild childId) outMsg))

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
                | childPages = EveryDict.insert childId subModel model.childPages
                , childForms = EveryDict.insert childId subForm model.childForms
              }
            , Cmd.map (MsgChild childId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgEditableSession subMsg ->
            -- Just route it over to the backend ...
            ( model
            , Cmd.none
            , [ App.Model.MsgIndexedDb <|
                    Backend.Model.MsgEditableSession sessionId subMsg
              ]
            )

        MsgMother motherId subMsg ->
            let
                motherForm =
                    getMotherForm motherId model session

                motherPage =
                    EveryDict.get motherId model.motherPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                measurements =
                    getMotherMeasurementData motherId session

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateMother subMsg motherPage motherForm measurements

                sessionMsgs =
                    List.map (App.Model.MsgIndexedDb << Backend.Model.MsgEditableSession sessionId)
                        (Maybe.Extra.toList (Maybe.map (MeasurementOutMsgMother motherId) outMsg))

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
                | motherPages = EveryDict.insert motherId subModel model.motherPages
                , motherForms = EveryDict.insert motherId subForm model.motherForms
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
