port module Pages.Participant.Update exposing (update, subscriptions, init)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(..))
import App.Model exposing (DropzoneConfig)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Examination.Model exposing (Examination(..), emptyExaminationChild, emptyExaminationMother)
import Maybe.Extra exposing (isJust)
import Measurement.Model
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..), emptyModel)
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
                        | height = exam.height
                        , weight = exam.weight
                        , muac = exam.muac
                    }
            in
                { emptyModel | measurements = measurements }


{-| This is implicitly scoped to a particular examination ... for now, we're
just picking out the single examination that we've mocked for the participant.
Eventually, we'll need to parameterize this in some way.
-}
update : BackendUrl -> String -> User -> Language -> Pages.Participant.Model.Msg -> ( ParticipantId, Participant ) -> Model -> ( Participant, Model, Cmd Pages.Participant.Model.Msg, Maybe Page )
update backendUrl accessToken user language msg ( participantId, participant ) model =
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

        MsgMeasurement subMsg ->
            let
                ( measurementsUpdated, examinationUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken participantId subMsg model.measurements (getExamination participant)

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                selectedActivity =
                    if isJust maybeActivityTypeCompleted then
                        Maybe.map (\( _, redirectToActivity ) -> Just redirectToActivity) maybeActivityTypeCompleted
                            |> Maybe.withDefault Nothing
                    else
                        model.selectedActivity

                ( _, modelWithSelectedActivity, selectedActivityCmds, maybePage ) =
                    update backendUrl accessToken user language (SetSelectedActivity selectedActivity) ( participantId, participant ) modelWithMeasurements
            in
                ( setExamination examinationUpdated participant
                , modelWithSelectedActivity
                , Cmd.batch
                    [ Cmd.map MsgMeasurement cmds
                    , selectedActivityCmds
                    ]
                , Nothing
                )

        SetRedirectPage page ->
            ( participant, model, Cmd.none, Just page )

        SetSelectedActivity maybeActivityType ->
            ( participant
            , { model | selectedActivity = maybeActivityType }
            , setDropzone backendUrl language maybeActivityType
            , Nothing
            )

        SetSelectedTab tab ->
            ( participant
            , { model | selectedTab = tab, selectedActivity = Nothing }
            , Cmd.none
            , Nothing
            )


{-| Activate the dropzone on a specific activity type.
-}
setDropzone : String -> Language -> Maybe ActivityType -> Cmd Pages.Participant.Model.Msg
setDropzone backendUrl language activity =
    let
        isActive =
            case activity of
                Just (Child ChildPicture) ->
                    True

                _ ->
                    False

        config =
            { active = isActive
            , backendUrl = backendUrl
            , defaultMessage = translate language Trans.DropzoneDefaultMessage
            }
    in
        dropzoneConfig config


subscriptions : Model -> Sub Pages.Participant.Model.Msg
subscriptions model =
    Sub.map MsgMeasurement <| Measurement.Update.subscriptions model.measurements


{-| Send dropzone config to JS.
-}
port dropzoneConfig : DropzoneConfig -> Cmd msg
