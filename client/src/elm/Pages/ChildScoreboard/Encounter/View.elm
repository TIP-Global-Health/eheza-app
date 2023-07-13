module Pages.ChildScoreboard.Encounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardActivity.Utils exposing (..)
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (NCDASign(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.ChildScoreboard.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEncounterActionButton, viewEndEncounterDialog, viewPersonDetails, viewPersonDetailsExtended, viewReportLink)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
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
    in
    div [ class "page-encounter child-scoreboard" ]
        [ header
        , content
        , viewModal <| acuteIllnessEncounterPopup language assembled model
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
                |> List.partition (activityCompleted currentDate assembled db)

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
                (Translate.ChildScoreboardActivityTitle activity)
                (getActivityIcon activity)
                (SetActivePage <| UserPage <| ChildScoreboardActivityPage assembled.id activity)

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

        endEncounterMsg =
            let
                childGotDiarrhea =
                    getMeasurementValueFunc assembled.measurements.ncda
                        |> Maybe.map (.signs >> EverySet.member ChildGotDiarrhea)
                        |> Maybe.withDefault False
            in
            if childGotDiarrhea == True then
                ShowAIEncounterPopup

            else
                CloseEncounter assembled.id

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEncounterActionButton language
                    Translate.EndEncounter
                    "primary"
                    allowEndEncounter
                    endEncounterMsg
                ]
    in
    [ tabs
    , content
    ]


acuteIllnessEncounterPopup : Language -> AssembledData -> Model -> Maybe (Html Msg)
acuteIllnessEncounterPopup language assembled model =
    if model.showAIEncounterPopup then
        Just <|
            div [ class "ui active modal danger-signs-popup" ]
                [ div [ class "content" ]
                    [ div [ class "popup-heading-wrapper" ]
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]
                    , div [ class "popup-action" ] [ text <| translate language Translate.NCDADiarrheaPopupMessage ]
                    ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| TriggerAcuteIllnessEncounter assembled
                        ]
                        [ text <| translate language Translate.Continue ]
                    ]
                ]

    else
        Nothing
