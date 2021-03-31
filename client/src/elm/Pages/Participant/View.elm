module Pages.Participant.View exposing (viewChild, viewMother, viewUbudehe)

import Activity.Model exposing (Activity(..), ChildActivity(..), CompletedAndPending, MotherActivity(..))
import Activity.Utils exposing (getActivityIcon, summarizeChildParticipant, summarizeMotherParticipant)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (generatePreviousValuesForChild)
import Backend.Person.Model exposing (Gender(..), Person, Ubudehe(..))
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getChildren, getMother, getMotherMeasurementData, getMyMother)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import LocalData
import Maybe.Extra
import Measurement.Model
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Measurement.View
import Pages.NutritionActivity.View exposing (warningPopup)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model exposing (Model, Msg(..), Tab(..))
import Pages.Session.Model
import Participant.Model exposing (Participant)
import Participant.Utils exposing (childParticipant, motherParticipant)
import Translate exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import ZScore.Model


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 222
    , height = 222
    }


viewChild :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> PersonId
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model ChildActivity
    -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewChild language currentDate zscores isChw childId ( sessionId, session ) pages db model =
    -- It's nice to just pass in the childId. If the session is consistent, we
    -- should always be able to get the child.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getChild childId session.offlineSession of
        Just child ->
            viewFoundChild language currentDate zscores isChw ( childId, child ) ( sessionId, session ) pages db model

        Nothing ->
            -- TODO: Make this error a little nicer, and translatable ... it
            -- could occur for real if an invalid or out-of-date URL is
            -- entered, for instance. It shouldn't occur through normal
            -- navigation if the session is consistent (i.e. absent bugs).
            div [ class "wrap" ]
                [ h3 [] [ text "Internal error" ]
                , p [] [ text "Error in Pages.Participant.View.viewChild -- child could not be found." ]
                ]


{-| This one needs the `currentDate` in order to calculate ages from dates of birth.
-}
viewFoundChild :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> ( PersonId, Person )
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model ChildActivity
    -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewFoundChild language currentDate zscores isChw ( childId, child ) ( sessionId, session ) pages db model =
    let
        maybeMother =
            getMyMother childId session.offlineSession
                |> Maybe.map Tuple.second

        motherInfo =
            maybeMother
                |> Maybe.map (\mother -> text <| translate language <| Translate.MotherName mother.name)
                |> Maybe.Extra.toList

        dateOfBirth =
            child.birthDate
                |> Maybe.map (renderDate language)
                |> Maybe.withDefault (translate language Translate.NotAvailable)
                |> Translate.ReportDOB
                |> translate language
                |> text

        age =
            child.birthDate
                |> Maybe.map (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                |> Maybe.withDefault (translate language Translate.NotAvailable)
                |> Translate.ReportAge
                |> translate language
                |> text

        gender =
            child.gender
                |> Translate.Gender
                |> translate language
                |> text

        break =
            br [] []

        config =
            childParticipant

        activities =
            summarizeChildParticipant currentDate zscores childId session.offlineSession isChw db

        selectedActivity =
            case model.selectedTab of
                ProgressReport ->
                    model.selectedActivity

                Completed ->
                    -- We show the selected activity, or the first completed activity
                    -- if there is none or the selected activity is not completed.
                    model.selectedActivity
                        |> Maybe.andThen
                            (\activity ->
                                if List.member activity activities.completed then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head activities.completed)

                Pending ->
                    model.selectedActivity
                        |> Maybe.andThen
                            (\activity ->
                                if List.member activity activities.pending then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head activities.pending)

        content =
            if model.selectedTab == ProgressReport then
                [ div
                    [ class "ui segment"
                    ]
                    [ a
                        [ ProgressReportPage childId
                            |> SessionPage sessionId
                            |> UserPage
                            |> Redirect
                            |> onClick
                        ]
                        [ span [ class "icon-progress-report" ] []
                        , text <| translate language Translate.ViewProgressReport
                        ]
                    ]
                    |> keyed "progress-report"
                ]

            else
                case selectedActivity of
                    Just activity ->
                        let
                            form =
                                getChildForm childId pages session
                        in
                        getChildMeasurementData childId session
                            |> LocalData.unwrap
                                []
                                (\measurements ->
                                    [ generatePreviousValuesForChild childId db
                                        |> Measurement.View.viewChild language currentDate isChw child activity measurements zscores session form
                                        |> Html.map MsgMeasurement
                                        |> keyed "content"
                                    ]
                                )

                    Nothing ->
                        []

        popup =
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
                |> viewModal
                |> keyed "pupup"
                |> List.singleton
    in
    divKeyed [ class "wrap page-participant group" ] <|
        List.concat
            [ [ viewHeader language sessionId |> keyed "header"
              , div [ class "ui unstackable items participant-page child" ]
                    [ div
                        [ class "item" ]
                        [ div
                            [ class "ui image" ]
                            [ thumbnailImage "child" child.avatarUrl child.name thumbnailDimensions.height thumbnailDimensions.width ]
                        , div
                            [ class "content" ]
                            [ h2
                                [ class "ui header" ]
                                [ text child.name ]
                            , p [] <|
                                motherInfo
                                    ++ [ break, dateOfBirth, break, age, break, gender ]
                            , viewFamilyLinks childParticipant language childId ( sessionId, session )
                            ]
                        ]
                    ]
                    |> keyed "child-info"
              ]
            , viewActivityCards childParticipant language activities model.selectedTab session.offlineSession.session.clinicType selectedActivity
            , content
            , popup
            ]


viewMother :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> PersonId
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model MotherActivity
    -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewMother language currentDate zscores isChw motherId ( sessionId, session ) pages db model =
    -- It's nice to just pass in the motherId. If the session is consistent, we
    -- should always be able to get the mother.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getMother motherId session.offlineSession of
        Just mother ->
            viewFoundMother language currentDate zscores isChw ( motherId, mother ) ( sessionId, session ) pages db model

        Nothing ->
            -- TODO: Make this error a little nicer, and translatable ... it
            -- could occur for real if an invalid or out-of-date URL is
            -- entered, for instance. It shouldn't occur through normal
            -- navigation if the session is consistent (i.e. absent bugs).
            div [ class "wrap" ]
                [ h3 [] [ text "Internal error" ]
                , p [] [ text "Error in Pages.Participant.View.viewMother -- mother could not be found." ]
                ]


viewFoundMother :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> ( PersonId, Person )
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model MotherActivity
    -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewFoundMother language currentDate zscores isChw ( motherId, mother ) ( sessionId, session ) pages db model =
    let
        break =
            br [] []

        childrenList =
            getChildren motherId session.offlineSession
                |> List.indexedMap
                    (\index ( _, child ) ->
                        text <| translate language Translate.Baby ++ " " ++ String.fromInt (index + 1) ++ ": " ++ child.name
                    )
                |> List.intersperse break

        activities =
            summarizeMotherParticipant currentDate zscores motherId session.offlineSession isChw db

        selectedActivity =
            case model.selectedTab of
                ProgressReport ->
                    model.selectedActivity

                Completed ->
                    -- We show the selected activity, or the first completed activity
                    -- if there is none or the selected activity is not completed.
                    model.selectedActivity
                        |> Maybe.andThen
                            (\activity ->
                                if List.member activity activities.completed then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head activities.completed)

                Pending ->
                    model.selectedActivity
                        |> Maybe.andThen
                            (\activity ->
                                if List.member activity activities.pending then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head activities.pending)

        content =
            case selectedActivity of
                Just activity ->
                    let
                        form =
                            getMotherForm motherId pages session
                    in
                    getMotherMeasurementData motherId session
                        |> LocalData.unwrap
                            []
                            (\measurements ->
                                [ Measurement.View.viewMother language currentDate mother activity session.offlineSession.session.clinicType measurements form
                                    |> Html.map MsgMeasurement
                                    |> keyed "content"
                                ]
                            )

                Nothing ->
                    []
    in
    divKeyed [ class "wrap page-participant group" ] <|
        List.concat
            [ [ viewHeader language sessionId |> keyed "header"
              , div
                    [ class "ui unstackable items participant-page mother" ]
                    [ div
                        [ class "item" ]
                        [ div
                            [ class "ui image" ]
                            [ thumbnailImage "mother" mother.avatarUrl mother.name thumbnailDimensions.height thumbnailDimensions.width ]
                        , div
                            [ class "content" ]
                            [ h2
                                [ class "ui header" ]
                                [ text mother.name ]
                            , showMaybe <|
                                Maybe.map
                                    (\educationLevel ->
                                        p [ class "education-level-wrapper" ]
                                            [ label [] [ text <| translate language Translate.LevelOfEducationLabel ++ ": " ]
                                            , span [] [ text <| translate language <| Translate.LevelOfEducation educationLevel ]
                                            ]
                                    )
                                    mother.educationLevel
                            , showMaybe <|
                                Maybe.map
                                    (\ubudehe ->
                                        p [ class "ubudehe-wrapper" ]
                                            [ label [] [ text <| translate language Translate.UbudeheLabel ]
                                            , span [] [ text <| viewUbudehe ubudehe ]
                                            ]
                                    )
                                    mother.ubudehe
                            , p [] childrenList
                            , viewFamilyLinks motherParticipant language motherId ( sessionId, session )
                            ]
                        ]
                    ]
                    |> keyed "mother"
              ]
            , viewActivityCards motherParticipant language activities model.selectedTab session.offlineSession.session.clinicType selectedActivity
            , content
            ]


viewActivityCards :
    Participant id value activity msg NominalDate
    -> Language
    -> CompletedAndPending (List activity)
    -> Tab
    -> ClinicType
    -> Maybe activity
    -> List ( String, Html (Msg activity any) )
viewActivityCards config language activities selectedTab clinicType selectedActivity =
    let
        pendingActivitiesView =
            if List.isEmpty activities.pending then
                [ span [] [ text <| translate language Translate.NoActivitiesPendingForThisParticipant ] ]

            else
                List.map (viewActivityListItem config language clinicType selectedActivity) activities.pending

        completedActivitiesView =
            if List.isEmpty activities.completed then
                [ span [] [ text <| translate language Translate.NoActivitiesCompletedForThisParticipant ] ]

            else
                List.map (viewActivityListItem config language clinicType selectedActivity) activities.completed

        activeView =
            if selectedTab == ProgressReport then
                emptyNode

            else
                div [ class "ui task segment" ]
                    [ div [ class "ui five column grid" ] <|
                        if selectedTab == Pending then
                            pendingActivitiesView

                        else
                            completedActivitiesView
                    ]

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length activities.pending

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length activities.completed

        progressTabTitle =
            translate language <| Translate.ProgressReport

        extraTabs =
            if config.showProgressReportTab then
                [ tabItem progressTabTitle (selectedTab == ProgressReport) "progressreport" (SetSelectedTab ProgressReport) ]

            else
                []

        tabs =
            div [ class "ui tabular menu" ] <|
                [ tabItem pendingTabTitle (selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]
                    ++ extraTabs
    in
    [ tabs |> keyed "tabs"
    , activeView |> keyed "active-view"
    ]


viewActivityListItem : Participant id value activity msg NominalDate -> Language -> ClinicType -> Maybe activity -> activity -> Html (Msg activity any)
viewActivityListItem config language clinicType selectedActivity activityItem =
    let
        activity =
            config.tagActivity activityItem

        activityTitle =
            if activity == ChildActivity ChildFbf && clinicType == Achi then
                Translate.ActivitityTitleAchi

            else
                Translate.ActivitiesTitle activity
    in
    div [ class "column" ]
        [ a
            [ onClick <| SetSelectedActivity activityItem
            , classList
                [ ( "link-section", True )
                , ( "active", selectedActivity == Just activityItem )
                ]
            ]
            [ span [ class ("icon-section icon-" ++ getActivityIcon (config.tagActivity activityItem)) ] []
            , text <| translate language activityTitle
            ]
        ]


viewHeader : Language -> SessionId -> Html (Msg activity any)
viewHeader language id =
    div
        [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.Assessment ]
        , a
            [ class "link-back"
            , SessionPage id ParticipantsPage
                |> UserPage
                |> Redirect
                |> onClick
            ]
            [ span [ class "icon-back" ] [] ]
        ]


{-| Given a mother or a child, this figures out who the whole family is, and shows links allowing
you to switch between any family member.
-}
viewFamilyLinks : Participant id value activity msg NominalDate -> Language -> id -> ( SessionId, EditableSession ) -> Html (Msg activity any)
viewFamilyLinks config language participantId ( sessionId, session ) =
    let
        -- Whether we've looking at a child or a mother, we figure out who the
        -- mother is. This will never be `Nothing` so long as the
        -- `EditableSession` is consistent, but it would be difficult to
        -- convince the compiler of that.
        maybeMotherId =
            config.getMotherId participantId session

        -- Whether we're originally given a mother or a child, we figure out who all the
        -- children are, by looking at the motherId we got.
        children =
            maybeMotherId
                |> Maybe.map (\motherId -> getChildren motherId session.offlineSession)
                |> Maybe.withDefault []
                |> List.map Tuple.first

        -- Generate markup for each child
        childrenMarkup =
            List.indexedMap viewChildMarkup children

        viewChildMarkup index childId =
            let
                -- This determines whether this child is the one we were given
                active =
                    config.toChildId participantId == Just childId

                attributes =
                    if active then
                        [ class "active" ]

                    else
                        [ ChildPage childId
                            |> SessionPage sessionId
                            |> UserPage
                            |> Redirect
                            |> onClick
                        ]
            in
            li attributes
                [ a []
                    [ span [ class "icon-baby" ] []
                    , span
                        [ class "count" ]
                        [ text <| String.fromInt (index + 1) ]
                    ]
                ]

        motherMarkup =
            Maybe.map viewMotherMarkup maybeMotherId
                |> Maybe.Extra.toList

        -- Generate the markup for the mother, given a definite motherId
        viewMotherMarkup motherId =
            let
                -- Figures out whether we're actually looking at this mother
                active =
                    config.toMotherId participantId == Just motherId

                attributes =
                    if active then
                        [ class "active" ]

                    else
                        [ MotherPage motherId
                            |> SessionPage sessionId
                            |> UserPage
                            |> Redirect
                            |> onClick
                        ]
            in
            li attributes
                [ a []
                    [ span [ class "icon-mother" ] []
                    ]
                ]
    in
    ul
        [ class "links-body" ]
        (motherMarkup ++ childrenMarkup)


viewUbudehe : Ubudehe -> String
viewUbudehe ubudehe =
    case ubudehe of
        Ubudehe1 ->
            "1"

        Ubudehe2 ->
            "2"

        Ubudehe3 ->
            "3"

        Ubudehe4 ->
            "4"
