port module Pages.Participant.Update exposing (update, subscriptions, init)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Examination.Model exposing (Examination(..), emptyExaminationChild, emptyExaminationMother)
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isJust)
import Measurement.Model
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..))
import Participant.Utils exposing (getExamination, setExamination)
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)


{-| Construct a default model for a participant. This assumes one examination
per participant, so will need to change.
-}
init : Participant -> Model
init participant =
    case getExamination participant of
        MotherExamination exam ->
            emptyModel

        ChildExamination exam ->
            let
                measurementModel =
                    Measurement.Model.emptyModel

                measurements =
                    { measurementModel
                        | height = Maybe.map (Tuple.second >> toString) exam.height
                        , weight = Maybe.map (Tuple.second >> toString) exam.weight
                        , muac = Maybe.map (Tuple.second >> toString) exam.muac
                    }
            in
                { emptyModel | measurements = measurements }


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
                ( measurementsUpdated, examinationUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken participantId subMsg model.measurements (getExamination participant)

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                additionalMsgs =
                    if isJust maybeActivityTypeCompleted then
                        [ SetSelectedActivity Nothing ]
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


subscriptions : Model -> Sub Pages.Participant.Model.Msg
subscriptions model =
    Sub.map MsgMeasurement <| Measurement.Update.subscriptions model.measurements
