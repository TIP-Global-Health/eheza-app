module Pages.Nutrition.Encounter.View exposing (allowEndingEncounter, partitionActivities, view)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionActivity.Utils exposing (allActivities, getActivityIcon)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Nutrition.Activity.Utils exposing (activityCompleted, allMandatoryActivities, expectActivity)
import Pages.Nutrition.Encounter.Model exposing (..)
import Pages.Nutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewConfirmationDialog, viewEndEncounterButton, viewPersonDetails, viewReportLink, viewSkipNCDADialog)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, translate)
import Utils.Html exposing (activityCard, tabItem, viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> NutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site zscores features id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate site zscores features id isChw db model) identity data


viewHeaderAndContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> NutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate site zscores features id isChw db model data =
    let
        header =
            viewHeader language isChw data

        content =
            viewContent language currentDate site zscores features id isChw db model data

        dialog =
            Maybe.map
                (\state ->
                    case state of
                        DialogEndEncounter ->
                            viewConfirmationDialog language
                                Translate.EndEncounterQuestion
                                Translate.OnceYouEndTheEncounter
                                (CloseEncounter id)
                                (SetDialogState Nothing)

                        DialogSkipNCDA ->
                            viewSkipNCDADialog language
                                (SetActivePage <| UserPage <| NutritionActivityPage id NCDA)
                                (SkipActivity NCDA)
                )
                model.dialogState
    in
    div [ class "page-encounter nutrition" ]
        [ header
        , content
        , viewModal dialog
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
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                        isChw
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionParticipantPage InitiatorParticipantsPage data.participant.person
            ]
            [ span [ class "icon-back" ] []
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> NutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate site zscores features id isChw db model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate site zscores features id isChw db data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> NutritionEncounterId
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> Model
    -> List (Html Msg)
viewMainPageContent language currentDate site zscores features id isChw db data model =
    let
        ( completedActivities, pendingActivities ) =
            partitionActivitiesConsideringSkipped currentDate site zscores features isChw db data model.skippedActivities

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
                                NutritionProgressReportPage data.id
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

        viewCard activity =
            let
                action =
                    case activity of
                        NCDA ->
                            SetDialogState <| Just DialogSkipNCDA

                        _ ->
                            SetActivePage <| UserPage <| NutritionActivityPage id activity
            in
            activityCard language
                (Translate.NutritionActivityTitle activity)
                (getActivityIcon activity)
                action

        allowEndEncounter =
            allowEndingEncounter site isChw pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEndEncounterButton language allowEndEncounter (SetDialogState <| Just DialogEndEncounter)
                ]
    in
    [ tabs
    , content
    ]


partitionActivities :
    NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> ( List NutritionActivity, List NutritionActivity )
partitionActivities currentDate site zscores features isChw db assembled =
    partitionActivitiesConsideringSkipped currentDate site zscores features isChw db assembled EverySet.empty


partitionActivitiesConsideringSkipped :
    NominalDate
    -> Site
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> AssembledData
    -> EverySet NutritionActivity
    -> ( List NutritionActivity, List NutritionActivity )
partitionActivitiesConsideringSkipped currentDate site zscores features isChw db assembled skipped =
    List.filter (\activity -> EverySet.member activity skipped |> not) allActivities
        |> List.filter (expectActivity currentDate site zscores features isChw assembled db)
        |> List.partition (activityCompleted currentDate site zscores features isChw assembled db)


allowEndingEncounter : Site -> Bool -> List NutritionActivity -> Bool
allowEndingEncounter site isChw pendingActivities =
    let
        mandatoryActivities =
            allMandatoryActivities site isChw
    in
    List.all
        (\activity ->
            -- Not an activity that is required to make a decision
            -- on next steps, and not the Next Steps activity itself.
            not (List.member activity mandatoryActivities)
                && (activity /= NextSteps)
        )
        pendingActivities
