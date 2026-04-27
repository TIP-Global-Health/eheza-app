module Pages.MessagingCenter.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (resilienceRoleFromString)
import Backend.Person.Utils
    exposing
        ( educationLevelFromInt
        , genderFromString
        , maritalStatusFromString
        , ubudeheFromInt
        )
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.MessagingCenter.Model exposing (Model, Msg(..), emptySurveyForm)
import Pages.MessagingCenter.Utils exposing (resolveSurveyScoreDialogState, surveyAnswerToScore, surveyQuestionsAnswered)
import Time
import Time.Extra


update : Time.Posix -> NominalDate -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentTime currentDate db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetRole value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | role = resilienceRoleFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBirthDate date ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | birthDate = Just date }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBirthDateSelectorState state ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | dateSelectorPopupState = state }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetGender value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | gender = genderFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetEducationLevel value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form
                        | educationLevel =
                            String.toInt value
                                |> Maybe.andThen educationLevelFromInt
                    }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetUbudehe value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form
                        | ubudehe =
                            String.toInt value
                                |> Maybe.andThen ubudeheFromInt
                    }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SetMaritalStatus value ->
            let
                form =
                    model.kickOffForm

                updatedForm =
                    { form | maritalStatus = maritalStatusFromString value }
            in
            ( { model | kickOffForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveKickOffSurvey nurseId nurse ->
            let
                form =
                    model.kickOffForm

                updatedNurse =
                    { nurse
                        | resilienceProgramStartDate = Just currentDate
                        , resilienceRole = form.role
                        , resilienceBirthDate = form.birthDate
                        , resilienceGender = form.gender
                        , resilienceEducationLevel = form.educationLevel
                        , resilienceUbudehe = form.ubudehe
                        , resilienceMaritalStatus = form.maritalStatus
                    }
            in
            ( model
            , Cmd.none
            , [ Backend.Nurse.Model.UpdateNurse nurseId updatedNurse
                    |> Backend.Model.MsgNurse nurseId
                    |> App.Model.MsgIndexedDb
              ]
            )

        SetSurveyAnswer question answer ->
            let
                updatedForm =
                    Dict.insert question answer model.surveyForm
            in
            ( { model | surveyForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveSurvey surveyType nurseId ->
            let
                ( msgs, extraMsgs ) =
                    if surveyQuestionsAnswered surveyType model.surveyForm then
                        -- We need all questions to have answers, to proceed with
                        -- save operations.
                        let
                            survey =
                                { nurse = nurseId
                                , dateMeasured = currentDate
                                , surveyType = surveyType
                                , signs = model.surveyForm
                                }

                            surveyScore =
                                Dict.values model.surveyForm
                                    |> List.map surveyAnswerToScore
                                    |> List.sum
                        in
                        ( [ Backend.ResilienceSurvey.Model.CreateResilienceSurvey survey
                                |> Backend.Model.MsgResilienceSurvey nurseId
                                |> App.Model.MsgIndexedDb
                          ]
                        , [ resolveSurveyScoreDialogState currentDate nurseId surveyType surveyScore db
                                |> Just
                                |> SetSurveyScoreDialogState
                          ]
                        )

                    else
                        ( [], [] )
            in
            ( { model | surveyForm = emptySurveyForm }
            , Cmd.none
            , msgs
            )
                |> sequenceExtra (update currentTime currentDate db) extraMsgs

        SetSurveyScoreDialogState state ->
            ( { model | surveyScoreDialogState = state }
            , Cmd.none
            , []
            )

        SetActiveTab tab ->
            ( { model | activeTab = tab }
            , Cmd.none
            , []
            )

        ScrollTab step ->
            ( { model | tabScrollPosition = model.tabScrollPosition + step }
            , Cmd.none
            , []
            )

        ResilienceMessageClicked messageId nurseId nurse updateTimeRead ->
            let
                ( updatedModel, msgs ) =
                    if EverySet.member messageId model.expandedMessages then
                        ( { model | expandedMessages = EverySet.remove messageId model.expandedMessages }
                        , if updateTimeRead then
                            markMessageReadAction currentTime messageId nurseId nurse

                          else
                            []
                        )

                    else
                        ( { model | expandedMessages = EverySet.insert messageId model.expandedMessages }
                        , []
                        )
            in
            ( updatedModel
            , Cmd.none
            , msgs
            )

        SetMessageOptionsDialogState value ->
            ( { model | messageOptionsDialogState = value }
            , Cmd.none
            , []
            )

        ToggleMessageRead messageId nurseId nurse isRead ->
            let
                action =
                    if isRead then
                        -- To mark message as unread, we set reminder to current time,
                        -- and message read time to one second before current time.
                        -- In essence, scheduling message reminder to right now.
                        updateMessageAction messageId
                            nurseId
                            nurse
                            (\message ->
                                { message
                                    | nextReminder = Just currentTime
                                    , timeRead = Just <| Time.Extra.add Time.Extra.Second -1 Time.utc currentTime
                                }
                            )

                    else
                        markMessageReadAction currentTime messageId nurseId nurse
            in
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , action
            )

        ToggleMessageFavorite messageId nurseId nurse ->
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , updateMessageAction messageId
                nurseId
                nurse
                (\message -> { message | isFavorite = not message.isFavorite })
            )

        ScheduleMessageReminder hours messageId nurseId nurse ->
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , -- Marking message as read, and setting reminder time in X hours.
              updateMessageAction messageId
                nurseId
                nurse
                (\message ->
                    { message
                        | nextReminder = Just <| Time.Extra.add Time.Extra.Hour hours Time.utc currentTime
                        , timeRead = Just currentTime
                    }
                )
            )


updateMessageAction : ResilienceMessageId -> NurseId -> Nurse -> (ResilienceMessage -> ResilienceMessage) -> List App.Model.Msg
updateMessageAction messageId nurseId nurse updateFunc =
    Dict.get messageId nurse.resilienceMessages
        |> Maybe.map
            (\message ->
                let
                    updatedNurse =
                        { nurse
                            | resilienceMessages =
                                Dict.insert
                                    messageId
                                    (updateFunc message)
                                    nurse.resilienceMessages
                        }
                in
                [ Backend.Nurse.Model.UpdateNurse nurseId updatedNurse
                    |> Backend.Model.MsgNurse nurseId
                    |> App.Model.MsgIndexedDb
                ]
            )
        |> Maybe.withDefault []


markMessageReadAction : Time.Posix -> ResilienceMessageId -> NurseId -> Nurse -> List App.Model.Msg
markMessageReadAction currentTime messageId nurseId nurse =
    updateMessageAction messageId
        nurseId
        nurse
        (\message -> { message | timeRead = Just currentTime })
