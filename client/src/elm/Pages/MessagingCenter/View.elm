module Pages.MessagingCenter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole(..))
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.NutritionEncounter.Utils exposing (sortEncounterTuplesDesc)
import Backend.Person.Model exposing (EducationLevel(..), MaritalStatus(..), Ubudehe(..), allUbudehes)
import Backend.Person.Utils exposing (educationLevelToInt, genderToString, maritalStatusToString, ubudeheToInt)
import Backend.ResilienceSurvey.Model
    exposing
        ( ResilienceSurveyQuestion(..)
        , ResilienceSurveyQuestionOption(..)
        )
import Backend.ResilienceSurvey.Utils exposing (resilienceSurveyQuestionOptionToString)
import Date exposing (Month, Unit(..), isBetween, numberToMonth)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.Utils
    exposing
        ( taskCompleted
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSelectListInput
        )
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.Html exposing (spinner, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentDate nurseId nurse db model =
    let
        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.Wellbeing
                    , span [ class "counter" ] [ text "8" ]
                    ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage PinCodePage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        content =
            Maybe.map
                (\startDate ->
                    viewMonthlySurvey language currentDate nurseId model.monthlySurveyForm
                )
                nurse.resilienceProgramStartDate
                |> Maybe.withDefault (viewKickOffSurvey language currentDate nurseId nurse model.kickOffForm)
    in
    div [ class "page-activity messaging-center" ]
        [ header
        , content
        ]


viewKickOffSurvey : Language -> NominalDate -> NurseId -> Nurse -> KickOffForm -> Html Msg
viewKickOffSurvey language currentDate nurseId nurse form =
    let
        roleInput =
            [ viewQuestionLabel language Translate.ResilienceKickOffRoleQuestion
            , viewSelectListInput language
                form.role
                [ ResilienceRoleCHW
                , ResilienceRoleNurse
                , ResilienceRoleLineManager
                , ResilienceRoleSupervisor
                , ResilienceRoleDirector
                ]
                resilienceRoleToString
                SetRole
                Translate.ResilienceRole
                "role"
            ]

        birthDateInput =
            let
                executionDateForView =
                    Maybe.map formatDDMMYYYY form.birthDate
                        |> Maybe.withDefault ""

                dateSelectorConfig =
                    { select = SetBirthDate
                    , close = SetBirthDateSelectorState Nothing
                    , dateFrom = Date.add Years -90 currentDate
                    , dateTo = Date.add Years -10 currentDate
                    , dateDefault = Nothing
                    }
            in
            [ viewQuestionLabel language Translate.ResilienceKickOffBirthDateQuestion
            , div
                [ class "form-input date"
                , onClick <| SetBirthDateSelectorState (Just dateSelectorConfig)
                ]
                [ text executionDateForView ]
            , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.birthDate
            ]

        genderInput =
            [ viewQuestionLabel language Translate.ResilienceKickOffGenderQuestion
            , viewSelectListInput language
                form.gender
                [ Male
                , Female
                ]
                genderToString
                SetGender
                Translate.Gender
                "gender"
            ]

        educationLevelInput =
            [ viewQuestionLabel language Translate.ResilienceKickOffEducationLevelQuestion
            , viewSelectListInput language
                form.educationLevel
                [ NoSchooling
                , PrimarySchool
                , VocationalTrainingSchool
                , SecondarySchool
                , AdvancedDiploma
                , HigherEducation
                , MastersDegree
                ]
                (educationLevelToInt >> String.fromInt)
                SetEducationLevel
                Translate.LevelOfEducationForResilience
                "education-level"
            ]

        ubudeheInput =
            [ viewQuestionLabel language Translate.ResilienceKickOffUbudeheQuestion
            , viewSelectListInput language
                form.ubudehe
                allUbudehes
                (ubudeheToInt >> String.fromInt)
                SetUbudehe
                Translate.UbudeheNumber
                "ubudehe"
            ]

        maritalStatusInput =
            [ viewQuestionLabel language Translate.ResilienceKickOffMaritalStatusQuestion
            , viewSelectListInput language
                form.maritalStatus
                [ Divorced
                , Married
                , Single
                , Widowed
                , LivingWithPartner
                , Religious
                ]
                maritalStatusToString
                SetMaritalStatus
                Translate.MaritalStatus
                "marital-status"
            ]

        tasksCompleted =
            taskCompleted form.role
                + taskCompleted form.birthDate
                + taskCompleted form.gender
                + taskCompleted form.educationLevel
                + taskCompleted form.ubudehe
                + taskCompleted form.maritalStatus

        totalTasks =
            6
    in
    div [ class "ui unstackable items" ]
        [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form kick-off-survey" ] <|
                    roleInput
                        ++ birthDateInput
                        ++ genderInput
                        ++ educationLevelInput
                        ++ ubudeheInput
                        ++ maritalStatusInput
                , div [ class "actions" ]
                    [ button
                        [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                        , onClick <| SaveKickOffSurvey nurseId nurse
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]
            ]
        ]


viewMonthlySurvey : Language -> NominalDate -> NurseId -> MonthlySurveyForm -> Html Msg
viewMonthlySurvey language currentDate nurseId form =
    let
        questionInput question =
            [ viewCustomLabel language (Translate.ResilienceMonthlySurveyQuestion question) "." "label"
            , viewCustomLabel language Translate.ChooseOne ":" "instructions"
            , viewCheckBoxSelectInput language
                [ ResilienceSurveyQuestionOption0
                , ResilienceSurveyQuestionOption1
                , ResilienceSurveyQuestionOption2
                , ResilienceSurveyQuestionOption3
                , ResilienceSurveyQuestionOption4
                ]
                []
                (Dict.get question form)
                (SetMonthlySurveyAnswer question)
                Translate.ResilienceSurveyQuestionOption
            ]

        questions =
            [ ResilienceSurveyQuestion1
            , ResilienceSurveyQuestion2
            , ResilienceSurveyQuestion3
            , ResilienceSurveyQuestion4
            ]

        tasksCompleted =
            Dict.size form

        totalTasks =
            List.length questions
    in
    div [ class "ui unstackable items" ]
        [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form monthly-survey" ] <|
                    List.concat <|
                        List.map questionInput questions
                , div [ class "actions" ]
                    [ button
                        [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]

                        -- , onClick <| SaveMonthlySurvey nurseId nurse
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]
            ]
        ]
