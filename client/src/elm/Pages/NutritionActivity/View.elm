module Pages.NutritionActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import NutritionActivity.Model exposing (NutritionActivity(..))
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionActivity.Utils exposing (nutritionFormWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewCheckBoxMultipleSelectInput, viewCustomLabel, viewLabel)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NutritionEncounterId -> NutritionActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
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

        personId =
            participant
                |> RemoteData.map .person

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
            RemoteData.map (\a b c -> ( a, b, c )) personId
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
    in
    div [ class "page-activity nutrition" ] <|
        [ viewHeader language id activity
        , viewWebData language (viewContent language currentDate id activity model) identity personWithMeasurements
        ]


viewHeader : Language -> NutritionEncounterId -> NutritionActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.NutritionActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| NutritionEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> NutritionEncounterId -> NutritionActivity -> Model -> ( PersonId, Person, NutritionMeasurements ) -> Html Msg
viewContent language currentDate id activity model ( personId, person, measurements ) =
    ((viewPersonDetails language currentDate person |> div [ class "item" ])
        :: viewActivity language currentDate id activity ( personId, measurements ) model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> NutritionEncounterId -> NutritionActivity -> ( PersonId, NutritionMeasurements ) -> Model -> List (Html Msg)
viewActivity language currentDate id activity ( personId, measurements ) model =
    case activity of
        NutritionNutrition ->
            viewNutritionContent language currentDate id ( personId, measurements ) model.nutritionData


viewNutritionContent : Language -> NominalDate -> NutritionEncounterId -> ( PersonId, NutritionMeasurements ) -> NutritionData -> List (Html Msg)
viewNutritionContent language currentDate id ( personId, measurements ) data =
    let
        activity =
            NutritionNutrition

        form =
            measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)
                |> nutritionFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form family-planning" ]
                [ p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , viewLabel language Translate.SelectAllSigns
                , viewCheckBoxMultipleSelectInput language
                    [ Edema, AbdominalDistension, DrySkin ]
                    [ Apathy, PoorAppetite, BrittleHair ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NormalChildNutrition)
                    SetNutritionSign
                    Translate.ChildNutritionSignLabel
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveNutrition personId measurements.nutrition
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
