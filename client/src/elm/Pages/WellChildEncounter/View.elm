module Pages.WellChildEncounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (WellChildMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.WellChildActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.WellChildEncounter.Model exposing (..)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id db model data =
    let
        header =
            viewHeader language data

        content =
            viewContent language currentDate zscores id db model data
    in
    div [ class "page-encounter home-visit" ]
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
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| WellChildParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id db model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores id db data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate zscores id db data model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.filter (expectActivity currentDate data db)
                |> List.partition (activityCompleted currentDate zscores data db)

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
                ]

        viewCard activity =
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| WellChildActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.WellChildActivityTitle activity
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
            List.isEmpty pendingActivities

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
