module Pages.NutritionActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.View exposing (zScoreForHeightOrLength)
import NutritionActivity.Model exposing (NutritionActivity(..))
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionActivity.Utils exposing (..)
import Pages.NutritionEncounter.View exposing (viewChildDetails)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (taskCompleted, viewCheckBoxMultipleSelectInput, viewCustomLabel, viewLabel, viewMeasurementInput, viewPhotoThumbFromPhotoUrl)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.NominalDate exposing (Days(..), diffDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreLengthHeightForAge, zScoreWeightForAge, zScoreWeightForHeight, zScoreWeightForLength)


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id activity db model =
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
    div [ class "page-nutrition-activity" ] <|
        [ viewHeader language id activity
        , viewWebData language (viewContent language currentDate zscores id activity model) identity personWithMeasurements
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


viewContent : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> Model -> ( PersonId, Person, NutritionMeasurements ) -> Html Msg
viewContent language currentDate zscores id activity model ( personId, person, measurements ) =
    (viewChildDetails language currentDate person
        :: viewActivity language currentDate zscores id activity ( personId, person, measurements ) model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> NutritionActivity -> ( PersonId, Person, NutritionMeasurements ) -> Model -> List (Html Msg)
viewActivity language currentDate zscores id activity ( personId, person, measurements ) model =
    case activity of
        Height ->
            viewHeightContent language currentDate zscores ( personId, person, measurements ) model.heightData

        Muac ->
            viewMuacContent language currentDate ( personId, measurements ) model.muacData

        Nutrition ->
            viewNutritionContent language currentDate ( personId, measurements ) model.nutritionData

        Photo ->
            viewPhotoContent language currentDate ( personId, measurements ) model.photoData

        Weight ->
            viewWeightContent language currentDate zscores ( personId, person, measurements ) model.weightData


viewHeightContent : Language -> NominalDate -> ZScore.Model.Model -> ( PersonId, Person, NutritionMeasurements ) -> HeightData -> List (Html Msg)
viewHeightContent language currentDate zscores ( personId, person, measurements ) data =
    let
        activity =
            Height

        form =
            measurements.height
                |> Maybe.map (Tuple.second >> .value)
                |> heightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.height

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreText =
            form.height
                |> Maybe.andThen
                    (\height ->
                        Maybe.andThen
                            (\ageInDays ->
                                zScoreLengthHeightForAge zscores ageInDays person.gender (Centimetres height)
                            )
                            maybeAgeInDays
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form height" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , viewMeasurementInput
                    language
                    form.height
                    SetHeight
                    "height"
                    Translate.CentimeterShorthand
                ]
            , div [ class "ui large header z-score age" ]
                [ text <| translate language Translate.ZScoreHeightForAge
                , span [ class "sub header" ]
                    [ text zScoreText ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveHeight personId measurements.height
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewMuacContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> MuacData -> List (Html Msg)
viewMuacContent language currentDate ( personId, measurements ) data =
    let
        activity =
            Muac

        form =
            measurements.muac
                |> Maybe.map (Tuple.second >> .value)
                |> muacFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.muac
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form muac" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , viewMeasurementInput
                    language
                    form.muac
                    SetMuac
                    "muac"
                    Translate.CentimeterShorthand
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveMuac personId measurements.muac
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewNutritionContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> NutritionData -> List (Html Msg)
viewNutritionContent language currentDate ( personId, measurements ) data =
    let
        activity =
            Nutrition

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
            [ div [ class "ui form nutrition" ]
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


viewPhotoContent : Language -> NominalDate -> ( PersonId, NutritionMeasurements ) -> PhotoData -> List (Html Msg)
viewPhotoContent language currentDate ( personId, measurements ) data =
    let
        photoId =
            Maybe.map Tuple.first measurements.photo

        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case data.form.url of
                Just url ->
                    ( Just url
                    , [ onClick <| SavePhoto personId photoId url ]
                    , False
                    )

                Nothing ->
                    ( Maybe.map (Tuple.second >> .value) measurements.photo
                    , []
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , divKeyed [ class "ui full segment photo" ]
        [ keyedDivKeyed "content"
            [ class "content" ]
            [ p [] [ text <| translate language Translate.PrenatalPhotoHelper ]
                |> keyed "help"
            , keyedDivKeyed "grid"
                [ class "ui grid" ]
                [ Maybe.map viewPhotoThumbFromPhotoUrl displayPhoto
                    |> showMaybe
                    |> List.singleton
                    |> div [ class "eight wide column" ]
                    |> keyed "thumbnail"
                , div
                    [ id "dropzone"
                    , class "eight wide column dropzone"
                    , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
                    ]
                    [ div
                        [ class "dz-message"
                        , attribute "data-dz-message" ""
                        ]
                        [ span
                            []
                            [ text <| translate language Translate.DropzoneDefaultMessage ]
                        ]
                    ]
                    |> keyed "dropzone"
                ]
            ]
        , keyed "button" <|
            div [ class "actions" ]
                [ button
                    ([ classList
                        [ ( "ui fluid primary button", True )
                        , ( "disabled", isDisabled )
                        ]
                     ]
                        ++ saveMsg
                    )
                    [ text <| translate language Translate.Save ]
                ]
        ]
    ]


viewWeightContent : Language -> NominalDate -> ZScore.Model.Model -> ( PersonId, Person, NutritionMeasurements ) -> WeightData -> List (Html Msg)
viewWeightContent language currentDate zscores ( personId, person, measurements ) data =
    let
        activity =
            Weight

        form =
            measurements.weight
                |> Maybe.map (Tuple.second >> .value)
                |> weightFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.weight

        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreForAgeText =
            form.weight
                |> Maybe.andThen
                    (\weight ->
                        Maybe.andThen
                            (\ageInDays ->
                                zScoreWeightForAge zscores ageInDays person.gender (Kilograms weight)
                            )
                            maybeAgeInDays
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        zScoreForHeightText =
            measurements.height
                |> Maybe.map (Tuple.second >> .value)
                |> Maybe.andThen
                    (\(HeightInCm height) ->
                        form.weight
                            |> Maybe.andThen
                                (\weight ->
                                    Maybe.andThen
                                        (\ageInDays ->
                                            zScoreForHeightOrLength zscores ageInDays (Centimetres height) person.gender weight
                                        )
                                        maybeAgeInDays
                                )
                    )
                |> Maybe.map viewZScore
                |> Maybe.withDefault (translate language Translate.NotAvailable)
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form weight" ]
                [ viewLabel language <| Translate.NutritionActivityTitle activity
                , p [] [ text <| translate language <| Translate.NutritionActivityHelper activity ]
                , viewMeasurementInput
                    language
                    form.weight
                    SetWeight
                    "weight"
                    Translate.KilogramShorthand
                ]
            , div [ class "ui large header z-score age" ]
                [ text <| translate language Translate.ZScoreWeightForAge
                , span [ class "sub header" ]
                    [ text zScoreForAgeText ]
                ]
            , div [ class "ui large header z-score height" ]
                [ text <| translate language Translate.ZScoreWeightForHeight
                , span [ class "sub header" ]
                    [ text zScoreForHeightText
                    ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveWeight personId measurements.weight
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]
