module Pages.NCD.Encounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (NCDMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.NCDActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.NCD.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.NCD.Encounter.Model exposing (..)
import Pages.NCD.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id db model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate id db model assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter ncd" ]
        [ header
        , content
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language assembled =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.NCDEncounter
                        False
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NCDParticipantPage InitiatorParticipantsPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate id db assembled model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate id db assembled model =
    let
        ( completedActivities, pendingActivities ) =
            List.filter (expectActivity currentDate assembled db) getAllActivities
                |> List.partition (activityCompleted currentDate assembled db)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.ProgressReport

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
                ]

        viewCard activity =
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| NCDActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.NCDActivityTitle activity
                            |> translate language
                            |> String.toUpper
                            |> text
                        ]
                    ]
                ]

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        viewReportLink labelTransId redirectPage =
            div
                [ class "report-wrapper"
                , onClick <| SetActivePage redirectPage
                ]
                [ div [ class "icon-progress-report" ] []
                , div [ class "report-text" ]
                    [ div [ class "report-label" ] [ text <| translate language labelTransId ]
                    , div [ class "report-link" ] [ text <| translate language Translate.View ]
                    ]
                ]

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink Translate.ProgressReport (UserPage <| NCDProgressReportPage assembled.id)
                    ]

            else
                div [ class "full content" ]
                    [ div [ class "wrap-cards" ]
                        [ div [ class "ui four cards" ] <|
                            if List.isEmpty selectedActivities then
                                [ span [] [ text emptySectionMessage ] ]

                            else
                                List.map viewCard selectedActivities
                        ]
                    ]

        allowEndEcounter =
            allowEndingEcounter pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState
                ]
    in
    [ tabs
    , content
    ]


allowEndingEcounter : List NCDActivity -> Bool
allowEndingEcounter pendingActivities =
    -- @todo
    True
