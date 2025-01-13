module Pages.ChildScoreboard.Encounter.View exposing (view)

import Backend.ChildScoreboardActivity.Utils exposing (..)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (NCDASign(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.ChildScoreboard.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewCustomAction, viewEncounterActionButton, viewPersonDetailsExtended)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate site id db model =
    let
        assembled =
            generateAssembledData site id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> Site -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate site db model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate site db model assembled
    in
    div [ class "page-encounter child-scoreboard" ]
        [ header
        , content
        , viewModal <| acuteIllnessEncounterPopup language assembled model.showAIEncounterPopup TriggerAcuteIllnessEncounter
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
                        Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        True
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| ChildScoreboardParticipantPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> Site -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate site db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate site db assembled model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> Site -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate site db assembled model =
    let
        ( completedActivities, pendingActivities ) =
            List.filter (expectActivity currentDate site assembled) allActivities
                |> List.partition (activityCompleted currentDate site assembled db)

        pendingTabTitle =
            translate language <| Translate.ActivitiesToComplete <| List.length pendingActivities

        completedTabTitle =
            translate language <| Translate.ActivitiesCompleted <| List.length completedActivities

        scorecardTabTitle =
            translate language Translate.ProgressReport

        tabs =
            div [ class "ui tabular menu" ]
                [ tabItem pendingTabTitle
                    (model.selectedTab == Pending)
                    "pending"
                    (SetSelectedTab Pending)
                , tabItem completedTabTitle
                    (model.selectedTab == Completed)
                    "completed"
                    (SetSelectedTab Completed)
                , tabItem scorecardTabTitle
                    (model.selectedTab == Scorecard)
                    "scorecard"
                    (SetActivePage <| UserPage <| ChildScoreboardProgressReportPage assembled.id)
                ]

        viewCard activity =
            activityCard language
                (Translate.ChildScoreboardActivityTitle activity)
                (getActivityIcon activity)
                (SetActivePage <| UserPage <| ChildScoreboardActivityPage assembled.id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Scorecard ->
                    ( [], "" )

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
                , viewEndEncounterButton language assembled allowEndEncounter ShowAIEncounterPopup CloseEncounter
                ]
    in
    [ tabs
    , content
    ]


viewEndEncounterButton : Language -> AssembledData -> Bool -> msg -> (ChildScoreboardEncounterId -> msg) -> Html msg
viewEndEncounterButton language assembled allowEndEncounter showAIEncounterPopupMsg closeEncounterMsg =
    let
        endEncounterMsg =
            let
                childGotDiarrhea =
                    getMeasurementValueFunc assembled.measurements.ncda
                        |> Maybe.map (.signs >> EverySet.member ChildGotDiarrhea)
                        |> Maybe.withDefault False
            in
            if childGotDiarrhea then
                showAIEncounterPopupMsg

            else
                closeEncounterMsg assembled.id
    in
    viewEncounterActionButton language
        Translate.EndEncounter
        "primary"
        allowEndEncounter
        endEncounterMsg


acuteIllnessEncounterPopup : Language -> AssembledData -> Bool -> (AssembledData -> msg) -> Maybe (Html msg)
acuteIllnessEncounterPopup language assembled showAIEncounterPopup triggerAcuteIllnessEncounterMsg =
    if showAIEncounterPopup then
        Just <|
            div [ class "ui active modal danger-signs-popup" ]
                [ div [ class "content" ]
                    [ div [ class "popup-heading-wrapper" ]
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]
                    , div [ class "popup-action" ] [ text <| translate language Translate.NCDADiarrheaPopupMessage ]
                    ]
                , viewCustomAction language (triggerAcuteIllnessEncounterMsg assembled) False Translate.Continue
                ]

    else
        Nothing
