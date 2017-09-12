port module Pages.Participant.Update exposing (update, subscriptions)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(..))
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Child.Model exposing (Child)
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import Editable
import EveryDictList
import Examination.Model exposing (emptyExaminationChild)
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isJust)
import Measurement.Model as Measurement exposing (Msg(..))
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate, sequenceExtra)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)
import Utils.EditableWebData as EditableWebData


update :
    Date
    -> BackendUrl
    -> String
    -> User
    -> Language
    -> ParticipantsDict
    -> ( ParticipantId, Participant )
    -> Pages.Participant.Model.Msg
    -> Model
    -> ( Participant, Model, Cmd Pages.Participant.Model.Msg, Maybe Page )
update currentDate backendUrl accessToken user language participants ( participantId, participant ) msg model =
    case msg of
        HandlePusherEventData event ->
            case event of
                ParticipantUpdate newParticipant ->
                    -- So, the idea is that we have a new or updated participant,
                    -- which has already been saved at the server. Note that
                    -- we may have just pushed this change ourselves, so it's
                    -- already reflected here.
                    ( newParticipant
                    , model
                    , Cmd.none
                    , Nothing
                    )

        MsgFilePicker subMsg ->
            let
                ( subModel, cmd ) =
                    FilePicker.Update.update backendUrl language subMsg model.filePicker
            in
                ( participant
                , { model | filePicker = subModel }
                , Cmd.map MsgFilePicker cmd
                , Nothing
                )

        MsgMeasurement subMsg ->
            let
                ( measurementsUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken user ( participantId, participant ) subMsg model.measurements

                newDate =
                    (Date.toTime currentDate) + (24 * 60 * 60 * 1000) |> Date.fromTime

                -- Hard-wiring the period of one day, while we consider
                -- the Activity completed.
                participantUpdated =
                    case maybeActivityTypeCompleted of
                        Nothing ->
                            participant

                        Just activityTypeCompleted ->
                            updateActivityDate newDate activityTypeCompleted participant

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                additionalMsgs =
                    if isJust maybeActivityTypeCompleted then
                        [ SetSelectedActivity <| nextActivity currentDate participants model
                        ]
                    else
                        []
            in
                sequenceExtra (update currentDate backendUrl accessToken user language participants ( participantId, participant ))
                    additionalMsgs
                    ( participantUpdated
                    , modelWithMeasurements
                    , Cmd.map MsgMeasurement cmds
                    , Nothing
                    )

        SetRedirectPage page ->
            ( participant, model, Cmd.none, Just page )

        SetSelectedActivity maybeActivityType ->
            let
                additionalMsgs =
                    case maybeActivityType of
                        Just (Child ChildPicture) ->
                            [ MsgFilePicker <| FilePicker.Model.Bind ]

                        _ ->
                            [ MsgFilePicker <| FilePicker.Model.Unbind ]
            in
                sequenceExtra (update currentDate backendUrl accessToken user language participants ( participantId, participant ))
                    additionalMsgs
                    ( participant, { model | selectedActivity = maybeActivityType }, Cmd.none, Nothing )

        SetSelectedTab tab ->
            sequenceExtra (update currentDate backendUrl accessToken user language participants ( participantId, participant ))
                [ SetSelectedActivity Nothing ]
                ( participant, { model | selectedTab = tab }, Cmd.none, Nothing )


nextActivity : Date -> ParticipantsDict -> Model -> Maybe ActivityType
nextActivity currentDate participants model =
    let
        allActivityList =
            getActivityList currentDate Children participants

        pendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals) > 0) allActivityList
    in
        if List.isEmpty pendingActivities then
            Nothing
        else
            let
                firstActivity =
                    List.head pendingActivities
            in
                case firstActivity of
                    Just activityInfo ->
                        Just activityInfo.activity.activityType

                    Nothing ->
                        Nothing


subscriptions : Model -> Sub Pages.Participant.Model.Msg
subscriptions model =
    Sub.map MsgMeasurement <| Measurement.Update.subscriptions model.measurements
