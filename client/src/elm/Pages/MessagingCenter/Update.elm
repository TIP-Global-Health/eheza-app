module Pages.MessagingCenter.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model
import Backend.Nurse.Model
import Backend.Nurse.Utils exposing (resilienceRoleFromString)
import Backend.Person.Utils
    exposing
        ( educationLevelFromInt
        , genderFromString
        , maritalStatusFromString
        , ubudeheFromInt
        )
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestionOption(..), ResilienceSurveyType(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils
    exposing
        ( adoptionSurveyQuestions
        , quarterlySurveyQuestions
        , resolveSurveyScoreDialogState
        , surveyQuestionsAnswered
        )
import Time
import Time.Extra


update : Time.Posix -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentTime currentDate msg model =
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
                                    |> List.map
                                        (\answer ->
                                            case answer of
                                                ResilienceSurveyQuestionOption0 ->
                                                    1

                                                ResilienceSurveyQuestionOption1 ->
                                                    2

                                                ResilienceSurveyQuestionOption2 ->
                                                    3

                                                ResilienceSurveyQuestionOption3 ->
                                                    4

                                                ResilienceSurveyQuestionOption4 ->
                                                    5

                                                _ ->
                                                    0
                                        )
                                    |> List.sum
                        in
                        ( [ Backend.ResilienceSurvey.Model.CreateResilienceSurvey survey
                                |> Backend.Model.MsgResilienceSurvey nurseId
                                |> App.Model.MsgIndexedDb
                          ]
                        , [ resolveSurveyScoreDialogState surveyType surveyScore
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
                |> sequenceExtra (update currentTime currentDate) extraMsgs

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

        ResilienceMessageClicked nurseId messageId message updateTimeRead ->
            let
                ( updatedModel, msgs ) =
                    if EverySet.member messageId model.expandedMessages then
                        ( { model | expandedMessages = EverySet.remove messageId model.expandedMessages }
                        , if updateTimeRead then
                            [ markMessageReadAction currentTime nurseId messageId message ]

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

        ToggleMessageRead nurseId messageId message isRead ->
            let
                action =
                    if isRead then
                        -- To mark message as unread, we set reminder to current time,
                        -- and message read time to one second before current time.
                        -- In essence, scheduling message reminder to right now.
                        [ Backend.ResilienceMessage.Model.UpdateMessage messageId
                            { message
                                | nextReminder = Just currentTime
                                , timeRead = Just <| Time.Extra.add Time.Extra.Second -1 Time.utc currentTime
                            }
                            |> Backend.Model.MsgResilienceMessage nurseId
                            |> App.Model.MsgIndexedDb
                        ]

                    else
                        [ markMessageReadAction currentTime nurseId messageId message ]
            in
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , action
            )

        ToggleMessageFavorite nurseId messageId message ->
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , [ Backend.ResilienceMessage.Model.UpdateMessage messageId { message | isFavorite = not message.isFavorite }
                    |> Backend.Model.MsgResilienceMessage nurseId
                    |> App.Model.MsgIndexedDb
              ]
            )

        ScheduleMessageReminder hours nurseId messageId message ->
            ( { model
                | expandedMessages = EverySet.remove messageId model.expandedMessages
                , messageOptionsDialogState = Nothing
              }
            , Cmd.none
            , -- Marking message as read, and setting reminder time in X hours.
              [ Backend.ResilienceMessage.Model.UpdateMessage messageId
                    { message
                        | nextReminder = Just <| Time.Extra.add Time.Extra.Hour hours Time.utc currentTime
                        , timeRead = Just currentTime
                    }
                    |> Backend.Model.MsgResilienceMessage nurseId
                    |> App.Model.MsgIndexedDb
              ]
            )


markMessageReadAction : Time.Posix -> NurseId -> ResilienceMessageId -> ResilienceMessage -> App.Model.Msg
markMessageReadAction currentTime nurseId messageId message =
    Backend.ResilienceMessage.Model.UpdateMessage messageId { message | timeRead = Just currentTime }
        |> Backend.Model.MsgResilienceMessage nurseId
        |> App.Model.MsgIndexedDb
