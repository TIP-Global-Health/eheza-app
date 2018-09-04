module Pages.Participant.View exposing (viewChild, viewMother)

import Activity.Model exposing (Activity(..), ChildActivity, CompletedAndPending, MotherActivity(..))
import Activity.Utils exposing (getActivityIcon, getCheckedIn, summarizeChildParticipant, summarizeMotherParticipant)
import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Mother.Model exposing (Mother, Ubudehe(..))
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getChildren, getMother, getMotherMeasurementData, getMyMother)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra
import Measurement.Model
import Measurement.Utils exposing (fromChildMeasurementData, fromMotherMeasurementData, getChildForm, getMotherForm)
import Measurement.View
import Pages.Page exposing (Page(..), SessionPage(..))
import Pages.Participant.Model exposing (Model, Msg(..), Tab(..))
import Participant.Model exposing (Participant)
import Participant.Utils exposing (childParticipant, motherParticipant)
import Translate as Trans exposing (Language, translate)
import Utils.Html exposing (tabItem, thumbnailImage)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import ZScore.Model


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 222
    , height = 222
    }


viewChild : Language -> NominalDate -> ZScore.Model.Model -> ChildId -> EditableSession -> Model ChildActivity -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewChild language currentDate zscores childId session model =
    -- It's nice to just pass in the childId. If the session is consistent, we
    -- should always be able to get the child.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getChild childId session.offlineSession of
        Just child ->
            viewFoundChild language currentDate zscores ( childId, child ) session model

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
viewFoundChild : Language -> NominalDate -> ZScore.Model.Model -> ( ChildId, Child ) -> EditableSession -> Model ChildActivity -> Html (Msg ChildActivity Measurement.Model.MsgChild)
viewFoundChild language currentDate zscores ( childId, child ) session model =
    let
        maybeMother =
            child.motherId
                |> Maybe.andThen (\motherId -> getMother motherId session.offlineSession)

        motherInfo =
            maybeMother
                |> Maybe.map (\mother -> text <| translate language <| Trans.MotherName mother.name)
                |> Maybe.Extra.toList

        dateOfBirth =
            renderDate language child.birthDate
                |> Trans.ReportDOB
                |> translate language
                |> text

        age =
            renderAgeMonthsDays language child.birthDate currentDate
                |> Trans.ReportAge
                |> translate language
                |> text

        gender =
            child.gender
                |> Trans.Gender
                |> translate language
                |> text

        break =
            br [] []

        config =
            childParticipant

        activities =
            summarizeChildParticipant childId session

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
                            |> SessionPage
                            |> Redirect
                            |> onClick
                        ]
                        [ span [ class "icon-progress-report" ] []
                        , text <| translate language Trans.ViewProgressReport
                        ]
                    ]
                    |> keyed "progress-report"
                ]
            else
                case selectedActivity of
                    Just activity ->
                        let
                            measurements =
                                getChildMeasurementData childId session

                            form =
                                getChildForm childId session
                        in
                        [ Measurement.View.viewChild language currentDate child activity measurements zscores session form
                            |> Html.map MsgMeasurement
                            |> keyed "content"
                        ]

                    Nothing ->
                        []
    in
    divKeyed [ class "wrap" ] <|
        List.concat
            [ [ viewHeader language |> keyed "header"
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
                            , viewFamilyLinks childParticipant language childId session
                            ]
                        ]
                    ]
                    |> keyed "child-info"
              ]
            , viewActivityCards childParticipant language activities model.selectedTab selectedActivity
            , content
            ]


viewMother : Language -> MotherId -> EditableSession -> Model MotherActivity -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewMother language motherId session model =
    -- It's nice to just pass in the motherId. If the session is consistent, we
    -- should always be able to get the mother.  But it would be hard to
    -- convince the compiler of that, so we put in a pro-forma error message.
    case getMother motherId session.offlineSession of
        Just mother ->
            viewFoundMother language ( motherId, mother ) session model

        Nothing ->
            -- TODO: Make this error a little nicer, and translatable ... it
            -- could occur for real if an invalid or out-of-date URL is
            -- entered, for instance. It shouldn't occur through normal
            -- navigation if the session is consistent (i.e. absent bugs).
            div [ class "wrap" ]
                [ h3 [] [ text "Internal error" ]
                , p [] [ text "Error in Pages.Participant.View.viewMother -- mother could not be found." ]
                ]


viewFoundMother : Language -> ( MotherId, Mother ) -> EditableSession -> Model MotherActivity -> Html (Msg MotherActivity Measurement.Model.MsgMother)
viewFoundMother language ( motherId, mother ) session model =
    let
        break =
            br [] []

        childrenList =
            getChildren motherId session.offlineSession
                |> List.indexedMap
                    (\index ( _, child ) ->
                        text <| translate language Trans.Baby ++ " " ++ toString (index + 1) ++ ": " ++ child.name
                    )
                |> List.intersperse break

        activities =
            summarizeMotherParticipant motherId session

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
                        measurements =
                            getMotherMeasurementData motherId session

                        form =
                            getMotherForm motherId session
                    in
                    [ Measurement.View.viewMother language activity measurements form
                        |> Html.map MsgMeasurement
                        |> keyed "content"
                    ]

                Nothing ->
                    []
    in
    divKeyed [ class "wrap" ] <|
        List.concat
            [ [ viewHeader language |> keyed "header"
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
                                            [ label [] [ text <| translate language Trans.LevelOfEducationLabel ]
                                            , span [] [ text <| translate language <| Trans.LevelOfEducation educationLevel ]
                                            ]
                                    )
                                    mother.educationLevel
                            , showMaybe <|
                                Maybe.map
                                    (\ubudehe ->
                                        p [ class "ubudehe-wrapper" ]
                                            [ label [] [ text <| translate language Trans.UbudeheLabel ]
                                            , span [] [ text <| viewUbudehe ubudehe ]
                                            ]
                                    )
                                    mother.ubudehe
                            , p [] childrenList
                            , viewFamilyLinks motherParticipant language motherId session
                            ]
                        ]
                    ]
                    |> keyed "mother"
              ]
            , viewActivityCards motherParticipant language activities model.selectedTab selectedActivity
            , content
            ]


viewActivityCards : Participant id value activity msg -> Language -> CompletedAndPending (List activity) -> Tab -> Maybe activity -> List ( String, Html (Msg activity any) )
viewActivityCards config language activities selectedTab selectedActivity =
    let
        pendingActivitiesView =
            if List.isEmpty activities.pending then
                [ span [] [ text <| translate language Trans.NoActivitiesPendingForThisParticipant ] ]
            else
                List.map (viewActivityListItem config language selectedActivity) activities.pending

        completedActivitiesView =
            if List.isEmpty activities.completed then
                [ span [] [ text <| translate language Trans.NoActivitiesCompletedForThisParticipant ] ]
            else
                List.map (viewActivityListItem config language selectedActivity) activities.completed

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
            translate language <| Trans.ActivitiesToComplete <| List.length activities.pending

        completedTabTitle =
            translate language <| Trans.ActivitiesCompleted <| List.length activities.completed

        progressTabTitle =
            translate language <| Trans.ProgressReport

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


viewActivityListItem : Participant id value activity msg -> Language -> Maybe activity -> activity -> Html (Msg activity any)
viewActivityListItem config language selectedActivity activityItem =
    div [ class "column" ]
        [ a
            [ onClick <| SetSelectedActivity activityItem
            , classList
                [ ( "link-section", True )
                , ( "active", selectedActivity == Just activityItem )
                ]
            ]
            [ span [ class ("icon-section icon-" ++ getActivityIcon (config.tagActivity activityItem)) ] []
            , text <| translate language <| Trans.ActivitiesTitle <| config.tagActivity activityItem
            ]
        ]


viewHeader : Language -> Html (Msg activity any)
viewHeader language =
    div
        [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Trans.Assessment ]
        , a
            [ class "link-back"
            , SessionPage ParticipantsPage
                |> Redirect
                |> onClick
            ]
            [ span [ class "icon-back" ] [] ]
        ]


{-| Given a mother or a child, this figures out who the whole family is, and shows links allowing
you to switch between any family member.
-}
viewFamilyLinks : Participant id value activity msg -> Language -> id -> EditableSession -> Html (Msg activity any)
viewFamilyLinks config language participantId session =
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
            List.indexedMap viewChild children

        viewChild index childId =
            let
                -- This determines whether this child is the one we were given
                active =
                    config.toChildId participantId == Just childId

                attributes =
                    if active then
                        [ class "active" ]
                    else
                        [ ChildPage childId
                            |> SessionPage
                            |> Redirect
                            |> onClick
                        ]
            in
            li attributes
                [ a []
                    [ span [ class "icon-baby" ] []
                    , span
                        [ class "count" ]
                        [ text <| toString (index + 1) ]
                    ]
                ]

        motherMarkup =
            Maybe.map viewMother maybeMotherId
                |> Maybe.Extra.toList

        -- Generate the markup for the mother, given a definite motherId
        viewMother motherId =
            let
                -- Figures out whether we're actually looking at this mother
                active =
                    config.toMotherId participantId == Just motherId

                attributes =
                    if active then
                        [ class "active" ]
                    else
                        [ MotherPage motherId
                            |> SessionPage
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
