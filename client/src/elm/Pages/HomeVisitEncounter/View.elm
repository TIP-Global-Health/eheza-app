module Pages.HomeVisitEncounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.HomeVisitActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (HomeVisitMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.HomeVisitActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.HomeVisitEncounter.Model exposing (..)
import Pages.HomeVisitEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw db model) identity data


viewHeaderAndContent : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw db model data =
    let
        header =
            viewHeader language data

        content =
            viewContent language currentDate id isChw db model data
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
                        Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionParticipantPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id isChw db model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate id isChw db data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate id isChw db data model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.filter (expectActivity currentDate data.person isChw data db)
                |> List.partition (activityCompleted currentDate data.person isChw data db)

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
                    , onClick <| SetActivePage <| UserPage <| HomeVisitActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.HomeVisitActivityTitle activity
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
