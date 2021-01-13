module Pages.NutritionEncounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (NutritionMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.NutritionActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.NutritionEncounter.Model exposing (..)
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        data =
            generateAssembledData id db

        header =
            viewWebData language (viewHeader language) identity data

        content =
            viewWebData language (viewContent language currentDate zscores id isChw model) identity data
    in
    div [ class "page-encounter nutrition" ] <|
        [ header
        , content
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores id isChw data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate zscores id isChw data model =
    let
        measurements =
            data.measurements

        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.filter (expectActivity currentDate zscores data.person isChw measurements)
                |> List.partition (activityCompleted currentDate zscores data.person isChw measurements)

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
                    , onClick <| SetActivePage <| UserPage <| NutritionActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.NutritionActivityTitle activity
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
                    [ viewReportLink Translate.ProgressReport (UserPage <| NutritionProgressReportPage data.id)
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
            case pendingActivities of
                [] ->
                    True

                [ Photo ] ->
                    True

                _ ->
                    False

        endEcounterButtonAttributes =
            if allowEndEcounter then
                [ class "ui fluid primary button"
                , onClick <| CloseEncounter id
                ]

            else
                [ class "ui fluid primary button disabled" ]

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , div [ class "actions" ]
                    [ button
                        endEcounterButtonAttributes
                        [ text <| translate language Translate.EndEncounter ]
                    ]
                ]
    in
    [ tabs
    , content
    ]
