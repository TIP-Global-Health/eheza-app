module Pages.NCD.RecurrentEncounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Utils exposing (getRecurrentActivityIcon)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.NCD.Model exposing (..)
import Pages.NCD.RecurrentActivity.Utils exposing (activityCompleted, expectActivity)
import Pages.NCD.RecurrentEncounter.Model exposing (..)
import Pages.NCD.RecurrentEncounter.Utils exposing (..)
import Pages.NCD.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEncounterActionButton, viewPersonDetailsExtended, viewReportLink)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> NCDEncounterId -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id model assembled =
    let
        header =
            viewHeader language

        content =
            viewContent language currentDate assembled model
    in
    div [ class "page-encounter ncd" ]
        [ header
        , content
        ]


viewHeader : Language -> Html Msg
viewHeader language =
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
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> AssembledData -> Model -> Html Msg
viewContent language currentDate assembled model =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate assembled model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate assembled model =
    let
        ( completedActivities, pendingActivities ) =
            List.filter (expectActivity currentDate assembled) allActivities
                |> List.partition (activityCompleted currentDate assembled)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        reportsTabTitle =
            translate language Translate.Reports

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle (model.selectedTab == Pending) "pending" (SetSelectedTab Pending)
                , tabItem completedTabTitle (model.selectedTab == Completed) "completed" (SetSelectedTab Completed)
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetSelectedTab Reports)
                ]

        viewCard activity =
            activityCard language
                (Translate.NCDRecurrentActivitiesTitle activity)
                (getRecurrentActivityIcon activity)
                (SetActivePage <| UserPage <| NCDRecurrentActivityPage assembled.id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
                    ( [], "" )

        innerContent =
            if model.selectedTab == Reports then
                div [ class "reports-wrapper" ]
                    [ viewReportLink language
                        Translate.ProgressReport
                        (SetActivePage <|
                            UserPage <|
                                NCDProgressReportPage (InitiatorRecurrentEncounterPage assembled.id)
                        )
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

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEncounterActionButton language
                    Translate.LeaveEncounter
                    "primary"
                    True
                    (SetActivePage <| UserPage GlobalCaseManagementPage)
                ]
    in
    [ tabs
    , content
    ]
