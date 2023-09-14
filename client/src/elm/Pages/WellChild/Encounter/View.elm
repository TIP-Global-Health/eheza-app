module Pages.WellChild.Encounter.View exposing (allowEndingEcounter, partitionActivities, view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildActivity.Utils exposing (getActivityIcon, getAllActivities)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewPersonDetailsExtended)
import Pages.WellChild.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.WellChild.Encounter.Model exposing (..)
import Pages.WellChild.Encounter.Utils exposing (generateAssembledData)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id isChw db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id isChw db model assembled =
    let
        header =
            viewHeader language isChw assembled

        content =
            viewContent language currentDate zscores id isChw db model assembled
    in
    div [ class "page-encounter well-child" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                assembled.participant.person
                id
                model.warningPopupState
        ]


viewHeader : Language -> Bool -> AssembledData -> Html Msg
viewHeader language isChw assembled =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        isChw
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| WellChildParticipantPage InitiatorParticipantsPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores id isChw db assembled model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> PersonId -> WellChildEncounterId -> Maybe WarningPopupType -> Maybe (Html Msg)
warningPopup language childId encounterId warningPopupState =
    warningPopupState
        |> Maybe.map
            (\popupType ->
                let
                    warningHeading =
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]

                    actions =
                        case popupType of
                            PopupDangerSigns ->
                                div [ class "two ui buttons" ]
                                    [ button
                                        [ class "ui fluid button"
                                        , onClick <| SetWarningPopupState Nothing
                                        ]
                                        [ text <| translate language Translate.Cancel ]
                                    , button
                                        [ class "ui primary fluid button"
                                        , onClick <| TriggerAcuteIllnessEncounter childId encounterId
                                        ]
                                        [ text <| translate language Translate.CloseAndContinue ]
                                    ]

                            PopupECD _ ->
                                button
                                    [ class "ui fluid button"
                                    , onClick <| SetWarningPopupState Nothing
                                    ]
                                    [ text <| translate language Translate.Continue ]
                in
                div [ class "ui active modal danger-signs-popup" ]
                    [ div [ class "content" ]
                        [ div [ class "popup-heading-wrapper" ] warningHeading
                        , div [ class "popup-action" ] [ text <| translate language <| Translate.WellChildEncounterPopup popupType ]
                        ]
                    , div [ class "actions" ]
                        [ actions ]
                    ]
            )


viewMainPageContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate zscores id isChw db assembled model =
    let
        ( completedActivities, pendingActivities ) =
            partitionActivities currentDate zscores isChw db assembled

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
                , tabItem reportsTabTitle (model.selectedTab == Reports) "reports" (SetActivePage (UserPage (WellChildProgressReportPage id)))
                ]

        viewCard activity =
            activityCard language
                (Translate.WellChildActivityTitle activity)
                (getActivityIcon activity)
                (NavigateToActivity id activity)

        ( selectedActivities, emptySectionMessage ) =
            case model.selectedTab of
                Pending ->
                    ( pendingActivities, translate language Translate.NoActivitiesPending )

                Completed ->
                    ( completedActivities, translate language Translate.NoActivitiesCompleted )

                Reports ->
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
            allowEndingEcounter pendingActivities

        endEcounterButtonAttributes =
            if allowEndEncounter then
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


partitionActivities : NominalDate -> ZScore.Model.Model -> Bool -> ModelIndexedDb -> AssembledData -> ( List WellChildActivity, List WellChildActivity )
partitionActivities currentDate zscores isChw db assembled =
    getAllActivities isChw
        |> List.filter (expectActivity currentDate zscores isChw assembled db)
        |> List.partition (activityCompleted currentDate zscores isChw assembled db)


allowEndingEcounter : List WellChildActivity -> Bool
allowEndingEcounter pendingActivities =
    case pendingActivities of
        [] ->
            True

        [ WellChildPhoto ] ->
            True

        _ ->
            False
