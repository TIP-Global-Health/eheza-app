module Pages.NutritionEncounter.View exposing (view, viewChildDetails)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (NutritionMeasurements)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import NutritionActivity.Model exposing (NutritionActivity(..))
import NutritionActivity.Utils exposing (getActivityIcon, getAllActivities)
import Pages.NutritionEncounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (tabItem, thumbnailImage, viewLoading, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 120
    , height = 120
    }


view : Language -> NominalDate -> NutritionEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        encounter =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        measurements =
            Dict.get id db.nutritionMeasurements
                |> Maybe.withDefault NotAsked

        personWithMeasurements =
            RemoteData.map (\a b -> ( a, b )) person
                |> RemoteData.andMap measurements

        header =
            viewWebData language (viewHeader language) identity participant

        content =
            viewWebData language (viewContent language currentDate id model) identity personWithMeasurements
    in
    div [ class "page-nutrition-encounter" ] <|
        [ header
        , content
        ]


viewHeader : Language -> IndividualEncounterParticipant -> Html Msg
viewHeader language participant =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.NutritionEncounter
            ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionParticipantPage participant.person
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> NutritionEncounterId -> Model -> ( Person, NutritionMeasurements ) -> Html Msg
viewContent language currentDate id model ( person, measurements ) =
    (viewChildDetails language currentDate person
        :: viewMainPageContent language currentDate id measurements model
    )
        |> div [ class "ui unstackable items" ]


viewChildDetails : Language -> NominalDate -> Person -> Html msg
viewChildDetails language currentDate child =
    div [ class "item" ]
        [ div [ class "ui image" ]
            [ thumbnailImage "child" child.avatarUrl child.name thumbnailDimensions.height thumbnailDimensions.width ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text child.name ]
            , child.birthDate
                |> Maybe.map
                    (\birthDate ->
                        p [ class "age-wrapper" ]
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ":" ]
                            , span [] [ text <| renderAgeMonthsDays language birthDate currentDate ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
            ]
        ]


viewMainPageContent : Language -> NominalDate -> NutritionEncounterId -> NutritionMeasurements -> Model -> List (Html Msg)
viewMainPageContent language currentDate id measurements model =
    let
        ( completedActivities, pendingActivities ) =
            getAllActivities
                |> List.partition
                    (\activity ->
                        case activity of
                            Height ->
                                isJust measurements.height

                            Muac ->
                                isJust measurements.muac

                            Nutrition ->
                                isJust measurements.nutrition

                            Photo ->
                                isJust measurements.photo

                            Weight ->
                                isJust measurements.weight
                    )

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
