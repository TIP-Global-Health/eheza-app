module Pages.HIV.Encounter.View exposing (view)

import Backend.Entities exposing (..)
import Backend.HIVActivity.Model exposing (HIVActivity)
import Backend.HIVActivity.Utils exposing (allActivities, getActivityIcon)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.HIV.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.HIV.Encounter.Model exposing (..)
import Pages.HIV.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( viewConfirmationDialog
        , viewEndEncounterButton
        , viewPersonDetailsExtended
        , viewReportLink
        )
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> HIVEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate db model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate db model assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewConfirmationDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter assembled.id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter hiv" ]
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
                        Backend.IndividualEncounterParticipant.Model.HIVEncounter
                        False
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HIVParticipantPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate db assembled model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate db assembled model =
    let
        ( completedActivities, pendingActivities ) =
            List.filter (expectActivity currentDate assembled) allActivities
                |> List.partition (activityCompleted currentDate assembled)

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
            activityCard language
                (Translate.HIVActivityTitle activity)
                (getActivityIcon activity)
                (SetActivePage <| UserPage <| HIVActivityPage assembled.id activity)

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
                    [-- @todo:
                     -- viewReportLink language
                     --     Translate.ProgressReport
                     --     (SetActivePage <|
                     --         UserPage <|
                     --             HIVProgressReportPage (InitiatorEncounterPage assembled.id)
                     --     )
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

        allowEndEncounter =
            allowEndingEncounter pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEndEncounterButton language allowEndEncounter (SetEndEncounterDialogState True)
                ]
    in
    [ tabs
    , content
    ]


allowEndingEncounter : List HIVActivity -> Bool
allowEndingEncounter pendingActivities =
    List.isEmpty pendingActivities
