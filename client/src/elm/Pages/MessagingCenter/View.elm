module Pages.MessagingCenter.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole(..))
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.Person.Model exposing (EducationLevel(..), MaritalStatus(..), allUbudehes)
import Backend.Person.Utils exposing (educationLevelToInt, genderToString, maritalStatusToString, ubudeheToInt)
import Backend.ResilienceMessage.Model exposing (ResilienceCategory(..), ResilienceMessage, ResilienceMessageOrder(..))
import Backend.ResilienceSurvey.Model
    exposing
        ( ResilienceSurveyQuestionOption(..)
        , ResilienceSurveyType(..)
        )
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( customPopup
        , taskCompleted
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSelectListInput
        )
import RemoteData
import Time exposing (posixToMillis)
import Translate exposing (Language, translate, translateText)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (renderDate, sortByDateDesc)


view : Language -> Time.Posix -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
view language currentTime nurseId nurse db model =
    let
        currentDate =
            fromLocalDateTime currentTime

        numberOfUnreadMessages =
            resolveNumberOfUnreadMessages currentTime currentDate nurseId nurse db

        header =
            div [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.ResilienceMessage
                    , span [ class "counter" ] [ text <| String.fromInt numberOfUnreadMessages ]
                    ]
                , span
                    [ class "link-back"
                    , onClick <| SetActivePage <| UserPage WellbeingPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]

        content =
            Maybe.map
                (\programStartDate ->
                    let
                        messagingCenterView =
                            viewMessagingCenter language currentTime currentDate programStartDate nurseId nurse db model
                    in
                    Dict.get nurseId db.resilienceSurveysByNurse
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (\surveys ->
                                let
                                    surveysSorted =
                                        Dict.values surveys
                                            |> List.sortWith (sortByDateDesc .dateMeasured)

                                    runQuarterlySurvey =
                                        List.filter (.surveyType >> (==) ResilienceSurveyQuarterly) surveysSorted
                                            |> List.head
                                            |> Maybe.map
                                                (\survey ->
                                                    -- Run a 3 months survey if 3 month has passed since
                                                    -- last survey was completed.
                                                    Date.diff Months survey.dateMeasured currentDate >= 3
                                                )
                                            -- We need to run the survey if
                                            -- it is the first time to use the program.
                                            |> Maybe.withDefault True
                                in
                                if runQuarterlySurvey then
                                    viewQuarterlySurvey language currentDate nurseId model.quarterlySurveyForm

                                else
                                    messagingCenterView
                            )
                        |> Maybe.withDefault messagingCenterView
                )
                nurse.resilienceProgramStartDate
                |> Maybe.withDefault (viewKickOffSurvey language currentDate nurseId nurse model.kickOffForm)
    in
    div [ class "page-activity messaging-center" ]
        [ header
        , content
        , viewModal <|
            surveyScoreDialog language model.surveyScoreDialogState
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


viewQuarterlySurvey : Language -> NominalDate -> NurseId -> QuarterlySurveyForm -> Html Msg
viewQuarterlySurvey language currentDate nurseId form =
    let
        questionInput question =
            [ viewCustomLabel language (Translate.ResilienceQuarterlySurveyQuestion question) "." "label"
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
                (SetQuarterlySurveyAnswer question)
                Translate.ResilienceSurveyQuestionOption
            ]

        tasksCompleted =
            Dict.size form

        totalTasks =
            List.length quarterlySurveyQuestions
    in
    div [ class "ui unstackable items" ]
        [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form monthly-survey" ] <|
                    List.concatMap questionInput quarterlySurveyQuestions
                , div [ class "actions" ]
                    [ button
                        [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                        , onClick <| SaveQuarterlySurvey nurseId
                        ]
                        [ text <| translate language Translate.Save ]
                    ]
                ]
            ]
        ]


surveyScoreDialog :
    Language
    -> Maybe SurveyScoreDialogState
    -> Maybe (Html Msg)
surveyScoreDialog language =
    Maybe.map
        (\dialogState ->
            case dialogState of
                QuarterlySurveyScore score ->
                    let
                        data =
                            ( p [ class "score" ] [ text <| String.fromInt score ++ "/20" ]
                            , p [ class "interpretation" ] [ text <| translate language <| Translate.QuarterlySurveyScoreInterpretation score ]
                            , SetSurveyScoreDialogState Nothing
                            )
                    in
                    customPopup language False Translate.Continue "survey-score-popup blue" data
        )


viewMessagingCenter : Language -> Time.Posix -> NominalDate -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
viewMessagingCenter language currentTime currentDate programStartDate nurseId nurse db model =
    let
        messages =
            resolveInboxMessages currentDate programStartDate nurseId db

        ( unread, read ) =
            Dict.toList messages
                |> List.partition
                    (\( _, message ) ->
                        case message.timeRead of
                            Nothing ->
                                True

                            Just timeRead ->
                                Maybe.map
                                    (\nextReminder ->
                                        let
                                            nextReminderMillis =
                                                posixToMillis nextReminder
                                        in
                                        -- Reminder was set to latter time than the
                                        -- time at which message was read.
                                        (nextReminderMillis > posixToMillis timeRead)
                                            && -- Scheduled reminder time was reached.
                                               (posixToMillis currentTime > nextReminderMillis)
                                    )
                                    message.nextReminder
                                    |> Maybe.withDefault False
                    )

        content =
            let
                viewMessage =
                    viewResilienceMessage language nurseId nurse model

                viewFilteredByCategory category =
                    List.filter (Tuple.second >> .category >> (==) category) read
                        |> List.map viewMessage
            in
            case model.activeTab of
                TabGuide ->
                    viewGuideMessage language nurse

                TabUnread ->
                    List.map viewMessage unread

                TabFavorites ->
                    Dict.toList messages
                        |> List.filter (Tuple.second >> .isFavorite)
                        |> List.map viewMessage

                TabGrowth ->
                    viewFilteredByCategory ResilienceCategoryGrowth

                TabConnecting ->
                    viewFilteredByCategory ResilienceCategoryConnecting

                TabSelfcare ->
                    viewFilteredByCategory ResilienceCategorySelfCare

                TabStress ->
                    viewFilteredByCategory ResilienceCategoryStressManagement

                TabMindfullnes ->
                    viewFilteredByCategory ResilienceCategoryMindfulness
    in
    div []
        [ viewTabs language model
        , div [ class "ui report unstackable items" ]
            content
        , viewModal <|
            Maybe.map (messageOptionsDialog language currentTime currentDate nurseId model.activeTab)
                model.messageOptionsDialogState
        ]


viewTabs : Language -> Model -> Html Msg
viewTabs language model =
    let
        allTabs =
            [ TabGuide
            , TabUnread
            , TabFavorites
            , TabGrowth
            , TabConnecting
            , TabSelfcare
            , TabStress
            , TabMindfullnes
            ]

        numberOfTabsToDsipaly =
            5

        scrollButtonLeft =
            if model.tabScrollPosition > 0 then
                scrollButton "left" (ScrollTab -1)

            else
                emptyNode

        scrollRightButton =
            if model.tabScrollPosition + numberOfTabsToDsipaly < List.length allTabs then
                scrollButton "right" (ScrollTab 1)

            else
                emptyNode

        scrollButton direction action =
            span
                [ class <| "action-icon " ++ direction
                , onClick action
                ]
                []

        tabs =
            List.drop model.tabScrollPosition allTabs
                |> List.take numberOfTabsToDsipaly
                |> List.map renderButton

        renderButton tab =
            button
                [ classList
                    [ ( "active", tab == model.activeTab )
                    , ( "primary ui button", True )
                    ]
                , onClick <| SetActiveTab tab
                ]
                [ translateText language <| Translate.MessagingTab tab ]
    in
    div [ class "ui segment tabs" ] <|
        scrollButtonLeft
            :: tabs
            ++ [ scrollRightButton ]


viewResilienceMessage : Language -> NurseId -> Nurse -> Model -> ( ResilienceMessageId, ResilienceMessage ) -> Html Msg
viewResilienceMessage language nurseId nurse model ( messageId, message ) =
    let
        ( extraClass, ( head, body ) ) =
            case message.category of

                ResilienceCategoryIntroduction ->
                    ( "introduction"
                    , viewIntroductionMessage language nurse message.order
                    )

                ResilienceCategoryGrowth ->
                    ( "growth"
                    , viewGrowthMessage language message.order
                    )

                ResilienceCategoryStressManagement ->
                    ( "stress-management"
                    , viewStressManagementMessage language nurse message.order
                    )

                ResilienceCategoryMindfulness ->
                    ( "mindfulness"
                    , viewMindfulnessMessage language message.order
                    )

                ResilienceCategoryConnecting ->
                    ( "connecting"
                    , viewConnectingMessage language message.order
                    )

                ResilienceCategorySelfCare ->
                    ( "self-care"
                    , viewSelfCareMessage language message.order
                    )

                ResilienceCategoryEndOfPeriod ->
                    ( "end-of-period"
                    , viewEndOfPeriodMessage language message.order
                    )

        shrank =
            not <| EverySet.member messageId model.expandedMessages

        updateTimeRead =
            model.activeTab == TabGuide

        messageClickedAction =
            ResilienceMessageClicked nurseId messageId message updateTimeRead

        sentDate =
            Maybe.map (Date.add Days message.displayDay) nurse.resilienceProgramStartDate

        dateSent =
            sentDate
                |> Maybe.map
                    (\date ->
                        span [ class "date-sent" ]
                            [ text <| renderDate language date ]
                    )
                |> Maybe.withDefault emptyNode

        title =
            let
                messageCategory =
                    div [] [span [ class "category-header" ] [ text <| translate language Translate.ResilienceMessageGuide  ]
                    , span [ class "category-header" ] [ text <| translate language <| Translate.ResilienceCategory message.category ]]

                titleWrapperClass =
                    case model.activeTab of
                        TabUnread ->
                            "velvet"

                        TabFavorites ->
                            "purple"

                        _ ->
                            "blue"

                plainTitle =
                    div [ class <| "header", onClick messageClickedAction ]
                        [ i [ class <| "icon-" ++ extraClass ++ " " ++ titleWrapperClass ] []
                        , messageCategory
                        , dateSent
                        , div
                            [ class "title" ]
                            head
                        ]
            in
            div [ class <| "title-wrapper " ++ titleWrapperClass ]
                [ plainTitle
                , div
                    [ class "icon-options"
                    , onClick <| SetMessageOptionsDialogState <| Just <| MessageOptionsStateMain ( messageId, message )
                    ]
                    []
                ]
    in
    div
        [ classList
            [ ( "resilience-message " ++ extraClass, True )
            , ( "shrank", shrank )
            ]
        ]
        [ title
        , div
            [ class "content"
            , onClick messageClickedAction
            ]
            body
        ]


viewGuideMessage : Language -> Nurse -> List (Html Msg)
viewGuideMessage language nurse =
    [ div [ class "guide-message" ]
        [ p [ class "title" ]
            --
            [ text <| translate language Translate.ResilienceMessageGuideTitle1
            ]
        , ul []
            [ li []
                [ text <| translate language Translate.ResilienceMessageGuide1Bullet1
                ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide1Bullet2
                ]
            ]
        , p [ class "title" ]
            [ text <| translate language Translate.ResilienceMessageGuideTitle2
            ]
        , ul []
            [ li []
                [ text <| translate language Translate.ResilienceMessageGuide2Bullet1 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide2Bullet2 ]
            ]
        , p [ class "title" ]
            [ text <| translate language Translate.ResilienceMessageGuideTitle3
            ]
        , ul []
            [ li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet1 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet2 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet3 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet4 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet5 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet6 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide3Bullet7 ]
            ]
        , p [ class "title note" ]
            [ text <| translate language Translate.ResilienceMessageGuide3Note ]
        , p [ class "title" ]
            [ text <| translate language Translate.ResilienceMessageGuideTitle4
            ]
        , p []
            [ text <| translate language Translate.ResilienceMessageGuide4Text ]
        , p [ class "title" ]
            [ text <| translate language Translate.ResilienceMessageGuideTitle5
            ]
        , ul []
            [ li []
                [ text <| translate language Translate.ResilienceMessageGuide5Bullet1 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide5Bullet2 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide5Bullet3 ]
            ]
        , p [ class "title" ]
            [ text <| translate language Translate.ResilienceMessageGuideTitle6
            ]
        , ul []
            [ li []
                [ text <| translate language Translate.ResilienceMessageGuide6Bullet1 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide6Bullet2 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide6Bullet3 ]
            , li []
                [ text <| translate language Translate.ResilienceMessageGuide6Bullet4
                , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageGuide6Bullet5 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageGuide6Bullet6 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageGuide6Bullet7 ]
                    ]
                ]
            ]
        , p [ class "title note" ]
            [ text <| translate language Translate.ResilienceMessageGuide6Note ]
        ]
    ]


viewIntroductionMessage : Language -> Nurse -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewIntroductionMessage language nurse order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction1Title ]
            , [ p [] [ text <| translate language <| Translate.ResilienceMessageIntroduction1Paragraph1 nurse.name ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction1Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction1Paragraph3 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction2Paragraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageIntroduction2Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction2Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction2Bullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction2Bullet4 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction2Bullet5 ]
                    ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction3Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction3Paragraph3 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction4Paragraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageIntroduction4Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction4Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageIntroduction4Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction4Paragraph3 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction5Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction5Paragraph1 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction6Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction6Paragraph1 ]
              ]
            )

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction7Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction7Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction7Paragraph2 ]
              ]
            )

        ResilienceMessage8 ->
            ( [ text <| translate language Translate.ResilienceMessageIntroduction8Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageIntroduction8Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction8Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageIntroduction8Paragraph3 ]
              ]
            )


viewGrowthMessage : Language -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewGrowthMessage language order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth1Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth1Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth1Paragraph2 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth2Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth2Paragraph3 ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth3Paragraph2 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth4Paragraph2 ]
              ]
            )

        _ ->
            ( [], [] )


viewStressManagementMessage : Language -> Nurse -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewStressManagementMessage language nurse order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement1Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement1Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement1Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement1Paragraph3 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement2Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement2Paragraph3 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageStressManagement2Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement2Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement2Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement2Paragraph4 ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement3Paragraph2 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement4Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement4Paragraph3 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ text <| translate language <| Translate.ResilienceMessageStressManagement5Title nurse.name ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement5Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement5Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement5Paragraph3 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement6Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement6Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement6Paragraph2 ]
              ]
            )

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement7Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement7Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageStressManagement7Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement7Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement7Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement7Paragraph2 ]
              ]
            )

        _ ->
            ( [], [] )


viewMindfulnessMessage : Language -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewMindfulnessMessage language order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness1Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness1Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness1Paragraph2 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness2Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness2Paragraph3 ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness3Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness3Paragraph3 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness3Paragraph4 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness4Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness4Paragraph3 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness5Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness5Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageMindfulness5Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageMindfulness5Bullet2 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness5Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness5Paragraph3 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness6Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness6Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageMindfulness6Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageMindfulness6Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageMindfulness6Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness6Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness6Paragraph3 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness6Paragraph4 ]
              ]
            )

        _ ->
            ( [], [] )


viewConnectingMessage : Language -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewConnectingMessage language order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting1Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting1Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting1Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting1Paragraph3 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting2Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting2Paragraph3 ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting3Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting3Paragraph3 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting4Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting4Paragraph3 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting5Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting5Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting5Paragraph2 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting6Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting6Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting6Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting6Paragraph3 ]
              ]
            )

        _ ->
            ( [], [] )


viewSelfCareMessage : Language -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewSelfCareMessage language order =
    case order of
        ResilienceMessage1 ->
            ( [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare1Title ] ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare1Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare1Paragraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageSelfCare1Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare1Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare1Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare1Paragraph3 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare1Paragraph4 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageSelfCare2Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare2Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare2Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare2Paragraph3 ]
              ]
            )

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageSelfCare3Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare3Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare3Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare3Paragraph3 ]
              ]
            )

        _ ->
            ( [], [] )


viewEndOfPeriodMessage : Language -> ResilienceMessageOrder -> ( List (Html Msg), List (Html Msg) )
viewEndOfPeriodMessage language order =
    case order of
        ResilienceMessage1 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthParagraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthParagraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthBullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthBullet4 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFirstMonthParagraph3 ]
              ]
            )

        ResilienceMessage2 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthParagraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthParagraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthBullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthBullet4 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthBullet5 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfSecondMonthParagraph3 ]
              ]
            )

        _ ->
            ( [], [] )


messageOptionsDialog :
    Language
    -> Time.Posix
    -> NominalDate
    -> NurseId
    -> MessagingTab
    -> MessageOptionsDialogState
    -> Html Msg
messageOptionsDialog language currentTime currentDate nurseId tab state =
    case state of
        MessageOptionsStateMain ( messageId, message ) ->
            let
                content =
                    if tab == TabFavorites then
                        favoriteUnfavorite ++ reminder

                    else
                        readUnread ++ favoriteUnfavorite ++ reminder

                readUnread =
                    let
                        isRead =
                            tab /= TabUnread
                    in
                    [ button
                        [ class "ui fluid button cyan"
                        , onClick <| ToggleMessageRead nurseId messageId message isRead
                        ]
                        [ img [ src "assets/images/envelope.svg" ] []
                        , text <| translate language <| Translate.ReadToggle isRead
                        ]
                    ]

                favoriteUnfavorite =
                    [ button
                        [ class "ui fluid button purple"
                        , onClick <| ToggleMessageFavorite nurseId messageId message
                        ]
                        [ img [ src "assets/images/star.svg" ] []
                        , text <| translate language <| Translate.FavoriteToggle message.isFavorite
                        ]
                    ]

                reminder =
                    [ button
                        [ class "ui fluid button velvet"
                        , onClick <| SetMessageOptionsDialogState <| Just <| MessageOptionsStateReminder ( messageId, message )
                        ]
                        [ img [ src "assets/images/exclamation-mark.svg" ] []
                        , text <| translate language Translate.RemindMe
                        ]
                    ]
            in
            div [ class "ui active modal main" ]
                [ div [ class "content" ]
                    content
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| SetMessageOptionsDialogState Nothing
                        ]
                        [ text <| translate language Translate.Close ]
                    ]
                ]

        MessageOptionsStateReminder ( messageId, message ) ->
            let
                buttonForView hours =
                    button
                        [ class "ui fluid button primary"
                        , onClick <| ScheduleMessageReminder hours nurseId messageId message
                        ]
                        [ text <| translate language <| Translate.HoursSinglePlural hours
                        ]
            in
            div [ class "ui active modal reminder" ]
                [ div
                    [ class "content" ]
                    [ p [] [ text <| translate language Translate.RemindMePhrase ]
                    , buttonForView 1
                    , buttonForView 6
                    , buttonForView 12
                    ]
                , div
                    [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| SetMessageOptionsDialogState Nothing
                        ]
                        [ text <| translate language Translate.Cancel ]
                    ]
                ]
