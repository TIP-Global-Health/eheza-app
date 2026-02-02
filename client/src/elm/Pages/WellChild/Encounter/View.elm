module Pages.WellChild.Encounter.View exposing (partitionActivities, view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildActivity.Utils exposing (getActivityIcon, getAllActivities)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewCustomAction, viewPersonDetailsExtended, viewSkipNCDADialog)
import Pages.WellChild.Activity.Utils exposing (activityCompleted, expectActivity)
import Pages.WellChild.Encounter.Model exposing (AssembledData, DialogType(..), Model, Msg(..), Tab(..), WarningPopupType(..))
import Pages.WellChild.Encounter.Utils exposing (allowEndingEncounter, generateAssembledData)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features id isChw db model =
    let
        assembled =
            generateAssembledData site id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site features id isChw db model) identity assembled


viewHeaderAndContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate zscores site features id isChw db model assembled =
    let
        header =
            viewHeader language isChw assembled

        content =
            viewContent language currentDate zscores site features id isChw db model assembled

        dialog =
            Maybe.map
                (\state ->
                    case state of
                        DialogWarning popupType ->
                            warningPopup language
                                assembled.participant.person
                                id
                                popupType

                        DialogSkipNCDA ->
                            viewSkipNCDADialog language
                                (SetActivePage <| UserPage <| WellChildActivityPage id WellChildNCDA)
                                (SkipActivity WellChildNCDA)
                )
                model.dialogState
    in
    div [ class "page-encounter well-child" ]
        [ header
        , content
        , viewModal dialog
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
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate zscores site features id isChw db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores site features id isChw db assembled model
    )
        |> div [ class "ui unstackable items" ]


warningPopup : Language -> PersonId -> WellChildEncounterId -> WarningPopupType -> Html Msg
warningPopup language childId encounterId popupType =
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
                            , onClick <| SetDialogState Nothing
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
                        , onClick <| SetDialogState Nothing
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


viewMainPageContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> Model
    -> List (Html Msg)
viewMainPageContent language currentDate zscores site features id isChw db assembled model =
    let
        ( completedActivities, pendingActivities ) =
            partitionActivitiesConsideringSkipped currentDate zscores site features isChw db assembled model.skippedActivities

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
                , tabItem reportsTabTitle False "reports" (SetActivePage (UserPage (WellChildProgressReportPage id)))
                ]

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

        viewCard activity =
            let
                action =
                    case activity of
                        WellChildNCDA ->
                            SetDialogState <| Just DialogSkipNCDA

                        _ ->
                            NavigateToActivity id activity
            in
            activityCard language
                (Translate.WellChildActivityTitle activity)
                (getActivityIcon activity)
                action

        allowEndEncounter =
            allowEndingEncounter currentDate pendingActivities assembled

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewCustomAction language (CloseEncounter id) (not allowEndEncounter) Translate.EndEncounter
                ]
    in
    [ tabs
    , content
    ]


partitionActivities :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> ( List WellChildActivity, List WellChildActivity )
partitionActivities currentDate zscores site features isChw db assembled =
    partitionActivitiesConsideringSkipped currentDate zscores site features isChw db assembled EverySet.empty


partitionActivitiesConsideringSkipped :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> EverySet WellChildActivity
    -> ( List WellChildActivity, List WellChildActivity )
partitionActivitiesConsideringSkipped currentDate zscores site features isChw db assembled skipped =
    List.filter (\activity -> EverySet.member activity skipped |> not) getAllActivities
        |> List.filter (expectActivity currentDate zscores site features isChw assembled db)
        |> List.partition (activityCompleted currentDate zscores site features isChw assembled db)
