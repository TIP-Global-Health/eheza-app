module Pages.Nutrition.Encounter.View exposing (allowEndingEcounter, partitionActivities, view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantInitiator(..))
import Backend.Measurement.Model exposing (NutritionMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionActivity.Model exposing (NutritionActivity(..))
import Backend.NutritionActivity.Utils exposing (getActivityIcon, getAllActivities)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Nutrition.Activity.Utils exposing (activityCompleted, allMandatoryActivities, expectActivity)
import Pages.Nutrition.Encounter.Model exposing (..)
import Pages.Nutrition.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails, viewReportLink)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id isChw db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id isChw db model data =
    let
        header =
            viewHeader language isChw data

        content =
            viewContent language currentDate zscores id isChw db model data

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-encounter nutrition" ]
        [ header
        , content
        , viewModal endEncounterDialog
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
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw db model data =
    ((viewPersonDetails language currentDate data.person Nothing |> div [ class "item" ])
        :: viewMainPageContent language currentDate zscores id isChw db data model
    )
        |> div [ class "ui unstackable items" ]


viewMainPageContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> AssembledData -> Model -> List (Html Msg)
viewMainPageContent language currentDate zscores id isChw db data model =
    let
        ( completedActivities, pendingActivities ) =
            partitionActivities currentDate zscores isChw db data

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
            div [ class "card" ]
                [ div
                    [ class "image"
                    , onClick <| SetActivePage <| UserPage <| NutritionActivityPage id activity
                    ]
                    [ span [ class <| "icon-task icon-task-" ++ getActivityIcon activity ] [] ]
                , div [ class "content" ]
                    [ p []
                        [ Translate.NutritionActivityTitle activity
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

        allowEndEcounter =
            allowEndingEcounter isChw pendingActivities

        content =
            div [ class "ui full segment" ]
                [ innerContent
                , viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState
                ]
    in
    [ tabs
    , content
    ]


partitionActivities : NominalDate -> ZScore.Model.Model -> Bool -> ModelIndexedDb -> AssembledData -> ( List NutritionActivity, List NutritionActivity )
partitionActivities currentDate zscores isChw db assembled =
    List.filter (expectActivity currentDate zscores isChw assembled db) getAllActivities
        |> List.partition (activityCompleted currentDate zscores isChw assembled db)


allowEndingEcounter : Bool -> List NutritionActivity -> Bool
allowEndingEcounter isChw pendingActivities =
    let
        mandatoryActivities =
            allMandatoryActivities isChw
    in
    List.all
        (\activity ->
            -- Not an activity that is required to make a decision
            -- on next steps, and not the Next Steps activity itself.
            not (List.member activity mandatoryActivities)
                && (activity /= NextSteps)
        )
        pendingActivities
