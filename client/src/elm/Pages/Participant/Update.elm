port module Pages.Participant.Update exposing (init, nextActivity, update, subscriptions)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(..))
import Activity.Utils exposing (getActivityList)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import EveryDict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isJust)
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant(..), ParticipantId(..), ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getExamination, setExamination)
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)


{-| Construct a default model for a participant. This assumes one examination
-per participant, so will need to change.
-}
init : Participant -> Model
init participant =
    { emptyModel | measurements = toMeasurements (getExamination participant) }


{-| This is implicitly scoped to a particular examination ... for now, we're
just picking out the single examination that we've mocked for the participant.
Eventually, we'll need to parameterize this in some way.
-}
update :
    BackendUrl
    -> String
    -> User
    -> Language
    -> ( ParticipantId, Participant )
    -> Pages.Participant.Model.Msg
    -> Model
    -> ( Participant, Model, Cmd Pages.Participant.Model.Msg, Maybe Page )
update backendUrl accessToken user language ( participantId, participant ) msg model =
    case msg of
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
                ( measurementsUpdated, examinationUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken participantId subMsg model.measurements (getExamination participant)

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                additionalMsgs =
                    if isJust maybeActivityTypeCompleted then
                        [ SetSelectedActivity <| nextActivity ( participantId, participant ) model
                        ]
                    else
                        []
            in
                sequenceExtra (update backendUrl accessToken user language ( participantId, participant ))
                    additionalMsgs
                    ( setExamination examinationUpdated participant
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
                sequenceExtra (update backendUrl accessToken user language ( participantId, participant ))
                    additionalMsgs
                    ( participant, { model | selectedActivity = maybeActivityType }, Cmd.none, Nothing )

        SetSelectedTab tab ->
            sequenceExtra (update backendUrl accessToken user language ( participantId, participant ))
                [ SetSelectedActivity Nothing ]
                ( participant, { model | selectedTab = tab }, Cmd.none, Nothing )


{-| Eventually, this will need to be parameterized to deal with multiple examinations.
-}
nextActivity : ( ParticipantId, Participant ) -> Model -> Maybe ActivityType
nextActivity ( participantId, participant ) model =
    let
        participants =
            EveryDict.insert participantId participant EveryDict.empty

        allActivityList =
            getActivityList Children participants

        pendingActivities =
            List.filter (\activity -> (Tuple.first activity.totals) > 0 && (Just <| activity.activity.activityType) /= model.selectedActivity) allActivityList
    in
        if List.isEmpty pendingActivities then
            Nothing
        else
            let
                firstPendingActivity =
                    List.head <| pendingActivities
            in
                case firstPendingActivity of
                    Just activityInfo ->
                        Just activityInfo.activity.activityType

                    Nothing ->
                        Nothing


subscriptions : Model -> Sub Pages.Participant.Model.Msg
subscriptions model =
    Measurement.Update.subscriptions model.measurements
        |> Sub.map MsgMeasurement
