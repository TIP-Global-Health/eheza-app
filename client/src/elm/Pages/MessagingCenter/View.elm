module Pages.MessagingCenter.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole(..))
import Backend.Nurse.Utils exposing (resilienceRoleToString)
import Backend.Person.Model exposing (EducationLevel(..), MaritalStatus(..), allUbudehes)
import Backend.Person.Utils exposing (educationLevelToInt, genderToString, maritalStatusToString, ubudeheToInt)
import Backend.ResilienceMessage.Model exposing (ReasonForNotConsenting(..), ResilienceCategory(..), ResilienceMessage, ResilienceMessageOrder(..))
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestionOption(..), ResilienceSurveyType(..))
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
import Pages.MessagingConsent.View
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (customPopup, taskCompleted, viewBoolInput, viewCheckBoxSelectInput, viewCustomLabel, viewQuestionLabel, viewSaveAction, viewSelectListInput, viewTasksCount)
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
            resolveNumberOfUnreadMessages currentTime currentDate nurse

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
    in
    if not model.hasGivenConsent then
        Pages.MessagingConsent.View.view language
            currentTime
            nurseId
            nurse
            model

    else
        let
            content =
                Maybe.map
                    (\programStartDate ->
                        let
                            surveys =
                                Dict.get nurseId db.resilienceSurveysByNurse
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> Maybe.map Dict.values
                                    |> Maybe.withDefault []

                            surveysSorted =
                                List.sortWith (sortByDateDesc .dateMeasured) surveys

                            runSurvey surveyType =
                                let
                                    filteredSurveys =
                                        List.filter (.surveyType >> (==) surveyType) surveysSorted

                                    surveyCount =
                                        List.length filteredSurveys

                                    filterCondition survey =
                                        if surveyCount == 0 then
                                            -- We need to have at least one survey completed.
                                            True

                                        else if surveyCount >= 3 then
                                            -- There can be up to 3 surveys during program which lasts
                                            -- 6 months. At the begining, after 3 months and at the end.
                                            -- So, if we have 3 surveys already, there's no need to another one,
                                            False

                                        else
                                            let
                                                diffMonthsProgramStartLastSurvey =
                                                    Date.diff Months programStartDate survey.dateMeasured
                                            in
                                            if diffMonthsProgramStartLastSurvey >= 6 then
                                                -- Last survey run after 6 months from programstart date.
                                                -- It means that program has alreqady ended there.
                                                -- No need to run another one.
                                                False

                                            else
                                                let
                                                    diffMonthsLastSurveyCurrent =
                                                        Date.diff Months survey.dateMeasured currentDate
                                                in
                                                diffMonthsLastSurveyCurrent >= 3
                                in
                                List.head filteredSurveys
                                    |> Maybe.map filterCondition
                                    |> Maybe.withDefault True

                            runQuarterlySurvey =
                                runSurvey ResilienceSurveyQuarterly
                        in
                        if runQuarterlySurvey then
                            viewQuarterlySurvey language currentDate nurseId model.surveyForm

                        else
                            let
                                runAdoptionSurvey =
                                    runSurvey ResilienceSurveyAdoption
                            in
                            if runAdoptionSurvey then
                                viewAdoptionSurvey language currentDate nurseId model.surveyForm

                            else
                                viewMessagingCenter language currentTime currentDate programStartDate nurseId nurse db model
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
        [ viewTasksCount language tasksCompleted totalTasks
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form kick-off-survey" ] <|
                    roleInput
                        ++ birthDateInput
                        ++ genderInput
                        ++ educationLevelInput
                        ++ ubudeheInput
                        ++ maritalStatusInput
                , viewSaveAction language (SaveKickOffSurvey nurseId nurse) (tasksCompleted /= totalTasks)
                ]
            ]
        ]


viewQuarterlySurvey : Language -> NominalDate -> NurseId -> SurveyForm -> Html Msg
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
                (SetSurveyAnswer question)
                Translate.ResilienceSurveyQuestionOption
            ]

        tasksCompleted =
            Dict.size form

        totalTasks =
            List.length quarterlySurveyQuestions
    in
    div [ class "ui unstackable items" ]
        [ viewTasksCount language tasksCompleted totalTasks
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form monthly-survey" ] <|
                    List.concatMap questionInput quarterlySurveyQuestions
                , viewSaveAction language (SaveSurvey ResilienceSurveyQuarterly nurseId) (tasksCompleted /= totalTasks)
                ]
            ]
        ]


viewAdoptionSurvey : Language -> NominalDate -> NurseId -> SurveyForm -> Html Msg
viewAdoptionSurvey language currentDate nurseId form =
    let
        questionInput question =
            [ viewCustomLabel language (Translate.ResilienceSurveyAdoptionQuestion question) "." "label"
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
                (SetSurveyAnswer question)
                (Translate.ResilienceSurveyAdoptionOptionsForQuestion question)
            ]

        tasksCompleted =
            Dict.size form

        totalTasks =
            List.length adoptionSurveyQuestions
    in
    div [ class "ui unstackable items" ]
        [ viewTasksCount language tasksCompleted totalTasks
        , div [ class "ui full segment" ]
            [ div [ class "full content" ]
                [ div [ class "ui form monthly-survey" ] <|
                    List.concatMap questionInput adoptionSurveyQuestions
                , viewSaveAction language (SaveSurvey ResilienceSurveyAdoption nurseId) (tasksCompleted /= totalTasks)
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
            let
                ( scoreText, interpretationFunction, additionalMessage ) =
                    case dialogState of
                        QuarterlySurveyScore score ->
                            ( String.fromInt score ++ "/20"
                            , Translate.QuarterlySurveyScoreInterpretation score
                            , Nothing
                            )

                        AdoptionSurveyScore scores ->
                            case scores of
                                [ first ] ->
                                    ( String.fromInt first ++ "/60"
                                    , Translate.AdoptionSurveyScoreInterpretation first
                                    , Nothing
                                    )

                                [ first, second ] ->
                                    let
                                        messageContent =
                                            if first < second then
                                                [ text (translate language <| Translate.AdoptionSurveyProgressImproving), span [ class "icon-up" ] [] ]

                                            else if first > second then
                                                [ text (translate language <| Translate.AdoptionSurveyProgressNotImproving), span [ class "icon-down" ] [] ]

                                            else
                                                [ text (translate language <| Translate.AdoptionSurveyProgressSame) ]
                                    in
                                    ( String.fromInt second ++ "/60"
                                    , Translate.AdoptionSurveyScoreInterpretation second
                                    , Just <| p [ class "message" ] messageContent
                                    )

                                [ first, second, third ] ->
                                    let
                                        message =
                                            Just <|
                                                ul [ class "summary" ]
                                                    [ li [] [ text (translate language (Translate.AdoptionSurveyBaselineScore first)) ]
                                                    , li [] [ text (translate language (Translate.AdoptionSurvey3MonthScore second)) ]
                                                    ]
                                    in
                                    ( String.fromInt third ++ "/60"
                                    , Translate.AdoptionSurveyScoreInterpretation third
                                    , message
                                    )

                                _ ->
                                    ( "", Translate.EmptyString, Nothing )

                bottomMessage =
                    let
                        interpretation =
                            p [ class "interpretation" ] [ text <| translate language <| interpretationFunction ]
                    in
                    Maybe.map
                        (\message ->
                            div []
                                [ interpretation
                                , message
                                ]
                        )
                        additionalMessage
                        |> Maybe.withDefault interpretation

                dialogData =
                    ( p [ class "score" ] [ text scoreText ]
                    , bottomMessage
                    , SetSurveyScoreDialogState Nothing
                    )
            in
            customPopup language False Translate.Continue "survey-score-popup blue" dialogData
        )


viewMessagingCenter : Language -> Time.Posix -> NominalDate -> NominalDate -> NurseId -> Nurse -> ModelIndexedDb -> Model -> Html Msg
viewMessagingCenter language currentTime currentDate programStartDate nurseId nurse_ db model =
    let
        nurse =
            -- Genrate full list of messages that are supposed to
            -- appear at inbox on current date.
            { nurse_
                | resilienceMessages =
                    generateInboxMessages currentDate programStartDate nurse_.resilienceMessages
            }

        ( unread, read ) =
            Dict.toList nurse.resilienceMessages
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
                TabUnread ->
                    List.sortBy (\( _, message ) -> message.displayDay) unread
                        |> List.map viewMessage

                TabFavorites ->
                    Dict.toList nurse.resilienceMessages
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
            Maybe.map (messageOptionsDialog language currentTime currentDate nurseId nurse model.activeTab)
                model.messageOptionsDialogState
        ]


viewTabs : Language -> Model -> Html Msg
viewTabs language model =
    let
        allTabs =
            [ TabUnread
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
            model.activeTab == TabUnread

        messageClickedAction =
            ResilienceMessageClicked messageId nurseId nurse updateTimeRead

        title =
            let
                messageCategory =
                    span [ class "category-header" ] [ text <| translate language <| Translate.ResilienceCategory message.category ]

                titleWrapperClass =
                    case model.activeTab of
                        TabUnread ->
                            "velvet"

                        TabFavorites ->
                            "purple"

                        _ ->
                            "blue"

                dateSent =
                    Maybe.map (Date.add Days message.displayDay) nurse.resilienceProgramStartDate
                        |> Maybe.map
                            (\date ->
                                span [ class "date-sent" ]
                                    [ text <| renderDate language date ]
                            )
                        |> Maybe.withDefault emptyNode

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

        _ ->
            ( [], [] )


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

        ResilienceMessage5 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth5Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth5Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth5Paragraph2 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth6Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth6Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth6Paragraph2 ]
              ]
            )

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth7Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth7Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth7Paragraph2 ]
              ]
            )

        ResilienceMessage8 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth8Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth8Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth8Paragraph2 ]
              ]
            )

        ResilienceMessage9 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth9Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth9Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth9Paragraph2 ]
              ]
            )

        ResilienceMessage10 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth10Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth10Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth10Paragraph2 ]
              ]
            )

        ResilienceMessage11 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth11Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth11Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth11Paragraph2 ]
              ]
            )

        ResilienceMessage12 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth12Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth12Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth12Paragraph2 ]
              ]
            )

        ResilienceMessage13 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth13Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth13Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth13Paragraph2 ]
              ]
            )

        ResilienceMessage14 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth14Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth14Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth14Paragraph2 ]
              ]
            )

        ResilienceMessage15 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth15Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth15Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth15Paragraph2 ]
              ]
            )

        ResilienceMessage16 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth16Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth16Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth16Paragraph2 ]
              ]
            )

        ResilienceMessage17 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth17Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth17Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth17Paragraph2 ]
              ]
            )

        ResilienceMessage18 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth18Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth18Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth18Paragraph2 ]
              ]
            )

        ResilienceMessage19 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth19Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth19Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth19Paragraph2 ]
              ]
            )

        ResilienceMessage20 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth20Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth20Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth20Paragraph2 ]
              ]
            )

        ResilienceMessage21 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth21Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth21Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth21Paragraph2 ]
              ]
            )

        ResilienceMessage22 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth22Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth22Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth22Paragraph2 ]
              ]
            )

        ResilienceMessage23 ->
            ( [ text <| translate language Translate.ResilienceMessageGrowth23Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageGrowth23Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageGrowth23Paragraph2 ]
              ]
            )


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

        ResilienceMessage8 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement8Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement8Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageStressManagement8Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement8Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement8Bullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement8Bullet4 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement8Paragraph2 ]
              ]
            )

        ResilienceMessage9 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement9Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement9Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement9Paragraph2 ]
              ]
            )

        ResilienceMessage10 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement10Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement10Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement10Paragraph2 ]
              ]
            )

        ResilienceMessage11 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement11Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement11Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement11Paragraph2 ]
              ]
            )

        ResilienceMessage12 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement12Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement12Paragraph1 ]
              , ol []
                    [ li [] [ text <| translate language Translate.ResilienceMessageStressManagement12Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement12Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageStressManagement12Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement8Paragraph2 ]
              ]
            )

        ResilienceMessage13 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement13Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement13Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement13Paragraph2 ]
              ]
            )

        ResilienceMessage14 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement14Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement14Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement14Paragraph2 ]
              ]
            )

        ResilienceMessage15 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement15Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement15Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement15Paragraph2 ]
              ]
            )

        ResilienceMessage16 ->
            ( [ text <| translate language Translate.ResilienceMessageStressManagement16Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageStressManagement16Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageStressManagement16Paragraph2 ]
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

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness7Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness7Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness7Paragraph2 ]
              ]
            )

        ResilienceMessage8 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness8Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness8Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness8Paragraph2 ]
              ]
            )

        ResilienceMessage9 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness9Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness9Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness9Paragraph2 ]
              ]
            )

        ResilienceMessage10 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness10Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness10Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageMindfulness10Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageMindfulness10Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageMindfulness10Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness10Paragraph2 ]
              ]
            )

        ResilienceMessage11 ->
            ( [ text <| translate language Translate.ResilienceMessageMindfulness11Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageMindfulness11Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageMindfulness11Paragraph2 ]
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

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting7Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting7Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting7Paragraph2 ]
              ]
            )

        ResilienceMessage8 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting8Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting8Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting8Paragraph2 ]
              ]
            )

        ResilienceMessage9 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting9Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting9Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting9Paragraph2 ]
              ]
            )

        ResilienceMessage10 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting10Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting10Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting10Paragraph2 ]
              ]
            )

        ResilienceMessage11 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting11Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting11Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting11Paragraph2 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting11Paragraph3 ]
              ]
            )

        ResilienceMessage12 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting12Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting12Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting12Paragraph2 ]
              ]
            )

        ResilienceMessage13 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting13Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting13Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting13Paragraph2 ]
              ]
            )

        ResilienceMessage14 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting14Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting14Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting14Paragraph2 ]
              ]
            )

        ResilienceMessage15 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting15Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting15Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting15Paragraph2 ]
              ]
            )

        ResilienceMessage16 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting16Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting16Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting16Paragraph2 ]
              ]
            )

        ResilienceMessage17 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting17Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting17Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting17Paragraph2 ]
              ]
            )

        ResilienceMessage18 ->
            ( [ text <| translate language Translate.ResilienceMessageConnecting18Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageConnecting18Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageConnecting18Paragraph2 ]
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

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageSelfCare4Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare4Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare4Paragraph2 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare5Title ] ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare5Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageSelfCare5Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare5Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare5Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare5Paragraph2 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare6Title ] ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare6Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageSelfCare6Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare6Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare6Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare6Paragraph2 ]
              ]
            )

        ResilienceMessage7 ->
            ( [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare7Title ] ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare7Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageSelfCare7Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare7Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare7Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare7Paragraph2 ]
              ]
            )

        ResilienceMessage8 ->
            ( [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare8Title ] ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare8Paragraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageSelfCare8Bullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare8Bullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageSelfCare8Bullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare8Paragraph2 ]
              ]
            )

        ResilienceMessage9 ->
            ( [ text <| translate language Translate.ResilienceMessageSelfCare9Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare9Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare9Paragraph2 ]
              ]
            )

        ResilienceMessage10 ->
            ( [ text <| translate language Translate.ResilienceMessageSelfCare10Title ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageSelfCare10Paragraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageSelfCare10Paragraph2 ]
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

        ResilienceMessage3 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthParagraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthBullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthBullet4 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfThirdMonthParagraph2 ]
              ]
            )

        ResilienceMessage4 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthParagraph1 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthBullet3 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthBullet4 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthBullet4 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFourthMonthParagraph2 ]
              ]
            )

        ResilienceMessage5 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthParagraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthParagraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthBullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthParagraph3 ]
              ]
            )

        ResilienceMessage6 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthParagraph1 ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthParagraph2 ]
              , ul []
                    [ li [] [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthBullet1 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthBullet2 ]
                    , li [] [ text <| translate language Translate.ResilienceMessageEndOfSixthMonthBullet3 ]
                    ]
              , p [] [ text <| translate language Translate.ResilienceMessageEndOfFifthMonthParagraph3 ]
              ]
            )

        ResilienceMessage7 ->
            ( [ text <| translate language Translate.ResilienceMessageEndOfSeventhMonthTitle ]
            , [ p [] [ text <| translate language Translate.ResilienceMessageEndOfSeventhMonthParagraph1 ]
              ]
            )

        _ ->
            ( [], [] )


messageOptionsDialog :
    Language
    -> Time.Posix
    -> NominalDate
    -> NurseId
    -> Nurse
    -> MessagingTab
    -> MessageOptionsDialogState
    -> Html Msg
messageOptionsDialog language currentTime currentDate nurseId nurse tab state =
    case state of
        MessageOptionsStateMain ( messageId, message ) ->
            let
                content =
                    if tab == TabFavorites then
                        favoriteUnfavorite ++ reminder

                    else
                        let
                            readUnread =
                                let
                                    isRead =
                                        tab /= TabUnread
                                in
                                [ button
                                    [ class "ui fluid button cyan"
                                    , onClick <| ToggleMessageRead messageId nurseId nurse isRead
                                    ]
                                    [ img [ src "assets/images/envelope.svg" ] []
                                    , text <| translate language <| Translate.ReadToggle isRead
                                    ]
                                ]
                        in
                        readUnread ++ favoriteUnfavorite ++ reminder

                favoriteUnfavorite =
                    [ button
                        [ class "ui fluid button purple"
                        , onClick <| ToggleMessageFavorite messageId nurseId nurse
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
                        , onClick <| ScheduleMessageReminder hours messageId nurseId nurse
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
