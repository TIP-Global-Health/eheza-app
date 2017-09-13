module Pages.Activity.Update exposing (nextParticipant, update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Dict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isNothing)
import Measurement.Model exposing (saveMeasurementMessage)
import Measurement.Update
import Pages.Activity.Utils exposing (participantsWithPendingActivity)
import Participant.Utils exposing (getParticipantName)
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate, sequenceExtra)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Translate as Trans exposing (Language)


update :
    Date
    -> BackendUrl
    -> String
    -> User
    -> Language
    -> ParticipantsDict
    -> Msg
    -> Model
    -> ( Maybe ( ParticipantId, Participant, Measurement.Model.Model ), Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user language participants msg model =
    case msg of
        MsgFilePicker subMsg ->
            let
                ( subModel, cmd ) =
                    FilePicker.Update.update backendUrl language subMsg model.filePicker
            in
                ( Nothing
                , { model | filePicker = subModel }
                , Cmd.map MsgFilePicker cmd
                , Nothing
                )

        MsgMeasurement ( participantId, participant ) subMsg ->
            let
                ( measurementsUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken user ( participantId, participant ) subMsg model.measurements

                newDate =
                    (Date.toTime currentDate) + (24 * 60 * 60 * 1000) |> Date.fromTime

                -- Hard-wiring the period of one day, while we consider
                -- the Activity completed.
                ( participantUpdated, additionalMsgs ) =
                    case maybeActivityTypeCompleted of
                        Nothing ->
                            ( participant, [] )

                        Just activityTypeCompleted ->
                            ( updateActivityDate newDate activityTypeCompleted participant
                            , [ SetSelectedParticipant <| nextParticipant currentDate participants model ]
                            )

                updatedModel =
                    if saveMeasurementMessage subMsg then
                        { model | measurements = Measurement.Model.emptyModel }
                    else
                        { model | measurements = measurementsUpdated }
            in
                sequenceExtra (update currentDate backendUrl accessToken user language participants)
                    additionalMsgs
                    ( Just ( participantId, participantUpdated, measurementsUpdated )
                    , updatedModel
                    , Cmd.map (MsgMeasurement ( participantId, participantUpdated )) cmds
                    , Nothing
                    )

        SetRedirectPage page ->
            sequenceExtra (update currentDate backendUrl accessToken user language participants)
                [ SetSelectedParticipant Nothing ]
                ( Nothing, model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            let
                ( updatedModel, additionaMsgs ) =
                    if maybeParticipant /= model.selectedParticipant then
                        ( { model
                            | selectedParticipant = maybeParticipant
                            , measurements = Measurement.Model.emptyModel
                          }
                        , if isNothing maybeParticipant then
                            [ MsgFilePicker <| FilePicker.Model.Unbind ]
                          else
                            case model.selectedActivity of
                                Child ChildPicture ->
                                    [ MsgFilePicker <| FilePicker.Model.Bind ]

                                _ ->
                                    []
                        )
                    else
                        ( model, [] )
            in
                sequenceExtra (update currentDate backendUrl accessToken user language participants)
                    additionaMsgs
                    ( Nothing, updatedModel, Cmd.none, Nothing )

        SetSelectedTab tab ->
            let
                updatedModel =
                    { model | selectedTab = tab }

                additionalMsgs =
                    case model.selectedActivity of
                        Child ChildPicture ->
                            [ SetSelectedParticipant Nothing ]

                        _ ->
                            []
            in
                sequenceExtra (update currentDate backendUrl accessToken user language participants)
                    additionalMsgs
                    ( Nothing, updatedModel, Cmd.none, Nothing )


nextParticipant : Date -> ParticipantsDict -> Model -> Maybe ( ParticipantId, Participant )
nextParticipant currentDate participants model =
    let
        pendingParticipants =
            List.sortBy
                (\( _, participant ) ->
                    getParticipantName participant
                )
            <|
                Dict.toList <|
                    participantsWithPendingActivity currentDate participants model
    in
        -- At this point, the just completed form is still in pendingActivities.
        if List.length pendingParticipants < 2 then
            Nothing
        else
            let
                -- We have this trick to grab the 2nd pending element, as
                -- at this moment, the currently completed participant sits
                -- at the first place.
                firstPendingParticipant =
                    List.head <| List.reverse <| List.take 2 pendingParticipants
            in
                case firstPendingParticipant of
                    Just ( id, participant ) ->
                        Just ( id, participant )

                    Nothing ->
                        Nothing


subscriptions : Model -> ( ParticipantId, Participant ) -> Sub Pages.Activity.Model.Msg
subscriptions model ( participantId, participant ) =
    Sub.map (MsgMeasurement ( participantId, participant )) <| Measurement.Update.subscriptions model.measurements
