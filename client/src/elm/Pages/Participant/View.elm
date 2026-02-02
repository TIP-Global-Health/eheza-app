module Pages.Participant.View exposing (viewChild, viewMother)

import Activity.Model exposing (Activity(..), ChildActivity(..), CompletedAndPending, MotherActivity)
import Activity.Utils exposing (getActivityIcon, isCaregiver, summarizeChildParticipant, summarizeMotherParticipant)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolvePreviousValuesSetForChild)
import Backend.Person.Model exposing (Person)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getChildren, getMother, getMotherMeasurementData, getMyMother)
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import LocalData
import Maybe.Extra
import Measurement.Model
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Measurement.View
import Pages.Nutrition.Activity.View exposing (warningPopup)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model exposing (DialogType(..), Model, Msg(..), Tab(..))
import Pages.Session.Model
import Pages.Utils exposing (isAboveAgeOf2Years, viewSkipNCDADialog)
import Participant.Model exposing (Participant)
import Participant.Utils exposing (childParticipant, motherParticipant)
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderAgeYearsMonths, renderDate)
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
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> PersonId
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model ChildActivity
    -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewChild language currentDate zscores site features isChw childId ( sessionId, session ) pages db model =
    -- It's nice to just pass in the childId. If the session is consistent, we
    -- should always be able to get the child.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getChild childId session.offlineSession of
        Just child ->
            viewFoundChild language currentDate zscores site features isChw ( childId, child ) ( sessionId, session ) pages db model

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
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> ( PersonId, Person )
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model ChildActivity
    -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewFoundChild language currentDate zscores site features isChw ( childId, child ) ( sessionId, session ) pages db model =
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
                |> Maybe.map
                    (\birthDate ->
                        let
                            renderAgeFunc =
                                if isAboveAgeOf2Years currentDate child then
                                    renderAgeYearsMonths

                                else
                                    renderAgeMonthsDays
                        in
                        renderAgeFunc language birthDate currentDate
                    )
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

        activities =
            summarizeChildParticipant currentDate zscores features childId session.offlineSession isChw db

        filteredActivities =
            { pending = List.filter (\activity -> EverySet.member activity model.skippedActivities |> not) activities.pending
            , completed = List.filter (\activity -> EverySet.member activity model.skippedActivities |> not) activities.completed
            }

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
                                if List.member activity filteredActivities.completed then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head filteredActivities.completed)

                Pending ->
                    model.selectedActivity
                        |> Maybe.andThen
                            (\activity ->
                                if List.member activity filteredActivities.pending then
                                    Just activity

                                else
                                    Nothing
                            )
                        |> Maybe.Extra.orElse (List.head filteredActivities.pending)

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
                                getChildForm site childId pages session
                        in
                        getChildMeasurementData childId session
                            |> LocalData.unwrap
                                []
                                (\measurements ->
                                    [ resolvePreviousValuesSetForChild currentDate site childId db
                                        |> Measurement.View.viewChild language currentDate site isChw ( childId, child ) activity measurements zscores session db form
                                        |> Html.map MsgMeasurement
                                        |> keyed "content"
                                    ]
                                )

                    Nothing ->
                        []

        dialog =
            Maybe.andThen
                (\state ->
                    case state of
                        DialogWarning assessments ->
                            warningPopup language
                                (SetDialogState Nothing)
                                assessments

                        DialogSkipNCDA ->
                            Just <|
                                viewSkipNCDADialog language
                                    (SetSelectedActivity NCDA)
                                    (SkipActivity NCDA)
                )
                model.dialogState
                |> viewModal
                |> keyed "pupup"
                |> List.singleton
    in
    divKeyed [ class "wrap page-participant group" ] <|
        List.concat
            [ [ viewHeader language sessionId |> keyed "header"
              , div [ class "ui unstackable items participant-page child" ]
                    [ div [ class "item" ]
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
                            , viewFamilyLinks childParticipant childId ( sessionId, session )
                            ]
                        ]
                    ]
                    |> keyed "child-info"
              ]
            , viewActivityCards childParticipant language session.offlineSession childId filteredActivities model.selectedTab selectedActivity
            , content
            , dialog
            ]


viewMother :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> PersonId
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model MotherActivity
    -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewMother language currentDate zscores site features isChw motherId ( sessionId, session ) pages db model =
    -- It's nice to just pass in the motherId. If the session is consistent, we
    -- should always be able to get the mother.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getMother motherId session.offlineSession of
        Just mother ->
            viewFoundMother language currentDate zscores site features isChw ( motherId, mother ) ( sessionId, session ) pages db model

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
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> ( PersonId, Person )
    -> ( SessionId, EditableSession )
    -> Pages.Session.Model.Model
    -> ModelIndexedDb
    -> Model MotherActivity
    -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewFoundMother language currentDate zscores site features isChw ( motherId, mother ) ( sessionId, session ) pages db model =
    let
        break =
            br [] []

        ubudeheForView =
            if site == SiteRwanda then
                [ showMaybe <|
                    Maybe.map
                        (\ubudehe ->
                            p [ class "ubudehe-wrapper" ]
                                [ label [] [ text <| translate language Translate.UbudeheLabel ++ ": " ]
                                , span [] [ text <| translate language <| Translate.UbudeheNumber ubudehe ]
                                ]
                        )
                        mother.ubudehe
                ]

            else
                []

        childrenList =
            getChildren motherId session.offlineSession
                |> List.indexedMap
                    (\index ( _, child ) ->
                        text <| translate language Translate.Baby ++ " " ++ String.fromInt (index + 1) ++ ": " ++ child.name
                    )
                |> List.intersperse break

        activities =
            summarizeMotherParticipant currentDate zscores features motherId session.offlineSession isChw db

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
                                [ Measurement.View.viewMother language activity session.offlineSession.session.clinicType measurements form
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
                        , div [ class "content" ] <|
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
                            ]
                                ++ ubudeheForView
                                ++ [ p [] childrenList
                                   , viewFamilyLinks motherParticipant motherId ( sessionId, session )
                                   ]
                        ]
                    ]
                    |> keyed "mother"
              ]
            , viewActivityCards motherParticipant language session.offlineSession motherId activities model.selectedTab selectedActivity
            , content
            ]


viewActivityCards :
    Participant id value activity msg NominalDate
    -> Language
    -> OfflineSession
    -> PersonId
    -> CompletedAndPending (List activity)
    -> Tab
    -> Maybe activity
    -> List ( String, Html (Msg activity any) )
viewActivityCards config language offlineSession personId activities selectedTab selectedActivity =
    let
        activeView =
            if selectedTab == ProgressReport then
                emptyNode

            else
                let
                    clinicType =
                        offlineSession.session.clinicType

                    adultIsCaregiver =
                        isCaregiver personId offlineSession
                in
                div [ class "ui task segment" ]
                    [ div [ class "ui five column grid" ] <|
                        if selectedTab == Pending then
                            if List.isEmpty activities.pending then
                                let
                                    messageTransId =
                                        if adultIsCaregiver then
                                            Translate.CaregiverMessage

                                        else
                                            Translate.NoActivitiesPendingForThisParticipant
                                in
                                [ span [] [ text <| translate language messageTransId ] ]

                            else
                                List.map (viewActivityListItem config language clinicType selectedActivity) activities.pending

                        else if List.isEmpty activities.completed then
                            let
                                messageTransId =
                                    if adultIsCaregiver then
                                        Translate.CaregiverMessage

                                    else
                                        Translate.NoActivitiesCompletedForThisParticipant
                            in
                            [ span [] [ text <| translate language messageTransId ] ]

                        else
                            List.map (viewActivityListItem config language clinicType selectedActivity) activities.completed
                    ]

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length activities.pending

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length activities.completed

        extraTabs =
            if config.showProgressReportTab then
                let
                    progressTabTitle =
                        translate language Translate.ProgressReport
                in
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

        action =
            if activity == ChildActivity NCDA then
                SetDialogState <| Just DialogSkipNCDA

            else
                SetSelectedActivity activityItem
    in
    div [ class "column" ]
        [ a
            [ onClick action
            , classList
                [ ( "link-section", True )
                , ( "active", selectedActivity == Just activityItem )
                ]
            ]
            [ span [ class ("icon-activity-task icon-" ++ getActivityIcon (config.tagActivity activityItem)) ] []
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
        , span
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
viewFamilyLinks : Participant id value activity msg NominalDate -> id -> ( SessionId, EditableSession ) -> Html (Msg activity any)
viewFamilyLinks config participantId ( sessionId, session ) =
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
                [ span [ class "icon" ]
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
                [ span [ class "icon" ]
                    [ span [ class "icon-mother" ] []
                    ]
                ]
    in
    ul
        [ class "links-body" ]
        (motherMarkup ++ childrenMarkup)
