module Pages.HomeVisit.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Utils exposing (allActivities, getActivityIcon)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.HomeVisit.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.HomeVisit.Encounter.Model exposing (AssembledData, Model, Msg(..), Tab(..))
import Pages.HomeVisit.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewCustomAction, viewPersonDetails)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw model) identity data


viewHeaderAndContent : Language -> NominalDate -> HomeVisitEncounterId -> Bool -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw model data =
    let
        header =
            viewHeader language isChw data

        content =
            viewContent language currentDate id model data
    in
    div [ class "page-encounter home-visit" ]
        [ header
        , content
        ]


viewHeader : Language -> Bool -> AssembledData -> Html Msg
viewHeader language isChw data =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter
                        isChw
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionParticipantPage InitiatorParticipantsPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent : Language -> NominalDate -> HomeVisitEncounterId -> Model -> AssembledData -> Html Msg
viewContent language currentDate id model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language id data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> HomeVisitEncounterId -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language id data model =
    let
        ( completedActivities, pendingActivities ) =
            allActivities
                |> List.filter expectActivity
                |> List.partition (activityCompleted data)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                ]

        viewCard activity =
            activityCard language
                (Translate.HomeVisitActivityTitle activity)
                (getActivityIcon activity)
                (SetActivePage <| UserPage <| HomeVisitActivityPage id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

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

        allowEndEncounter =
            List.isEmpty pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewCustomAction language (CloseEncounter id) (not allowEndEncounter) Translate.EndEncounter
                ]
    in
    [ tabs
    , content
    ]
