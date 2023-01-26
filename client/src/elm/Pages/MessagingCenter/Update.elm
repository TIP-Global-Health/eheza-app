module Pages.MessagingCenter.Update exposing (update)

import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model
import Backend.Nurse.Utils exposing (resilienceRoleFromString)
import Backend.Person.Utils
    exposing
        ( educationLevelFromInt
        , genderFromString
        , maritalStatusFromString
        , ubudeheFromInt
        )
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyType(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (monthlySurveyQuestions)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
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

        SetMonthlySurveyAnswer question answer ->
            let
                updatedForm =
                    Dict.insert question answer model.monthlySurveyForm
            in
            ( { model | monthlySurveyForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveMonthlySurvey nurseId ->
            let
                msgs =
                    if Dict.size model.monthlySurveyForm == List.length monthlySurveyQuestions then
                        -- We need all questions to have answers, to proceed with
                        -- save operations.
                        let
                            survey =
                                { nurse = nurseId
                                , dateMeasured = currentDate
                                , surveyType = ResilienceSurveyMonthly
                                , signs = model.monthlySurveyForm
                                }
                        in
                        [ Backend.ResilienceSurvey.Model.CreateResilienceSurvey survey
                            |> Backend.Model.MsgResilienceSurvey nurseId
                            |> App.Model.MsgIndexedDb
                        ]

                    else
                        []
            in
            ( model
            , Cmd.none
            , msgs
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
