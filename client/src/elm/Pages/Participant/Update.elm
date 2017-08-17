port module Pages.Participant.Update exposing (update, subscriptions)

import Activity.Model exposing (ActivityType(Child), ChildActivityType(..))
import App.Model exposing (DropzoneConfig)
import App.PageType exposing (Page(..))
import Child.Model exposing (Child)
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Editable
import EveryDictList
import Examination.Model exposing (emptyExaminationChild)
import Maybe.Extra exposing (isJust)
import Measurement.Model as Measurement exposing (Msg(..))
import Measurement.Update
import Pages.Participant.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..))
import Pusher.Model exposing (PusherEventData(..))
import Translate as Trans exposing (Language, translate)
import User.Model exposing (..)
import Utils.EditableWebData as EditableWebData


update : Date -> BackendUrl -> String -> User -> Language -> Pages.Participant.Model.Msg -> ( ParticipantId, Participant ) -> Model -> ( Participant, Model, Cmd Pages.Participant.Model.Msg, Maybe Page )
update currentDate backendUrl accessToken user language msg ( participantId, participant ) model =
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

                        Just ( activtyTypeCompleted, activityToRedirect ) ->
                            updateActivityDate newDate activtyTypeCompleted participant

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                selectedActivity =
                    if isJust maybeActivityTypeCompleted then
                        Maybe.map (\( _, redirectToActivity ) -> Just redirectToActivity) maybeActivityTypeCompleted
                            |> Maybe.withDefault Nothing
                    else
                        model.selectedActivity

                ( _, modelWithSelectedAtivity, selectedActivityCmds, maybePage ) =
                    update currentDate backendUrl accessToken user language (SetSelectedActivity selectedActivity) ( participantId, participant ) modelWithMeasurements
            in
                ( participantUpdated
                , modelWithSelectedAtivity
                , Cmd.batch
                    [ Cmd.map MsgMeasurement cmds
                    , selectedActivityCmds
                    ]
                , Nothing
                )

        SetRedirectPage page ->
            ( participant, model, Cmd.none, Just page )

        SetSelectedActivity maybectivityType ->
            ( participant
            , { model | selectedActivity = maybectivityType }
            , setDropzone backendUrl language maybectivityType
            , Nothing
            )

        SetSelectedTab tab ->
            ( participant
            , { model | selectedTab = tab }
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
