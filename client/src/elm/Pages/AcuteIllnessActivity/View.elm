module Pages.AcuteIllnessActivity.View exposing (view)

import AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (acuteIllnessFormWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils exposing (taskCompleted, viewBoolInput, viewCheckBoxMultipleSelectInput, viewCustomLabel, viewLabel)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        encounter =
            Dict.get id db.acuteIllnessEncounters
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
            -- Todo
            Success {}

        -- Dict.get id db.acuteIllnessMeasurements
        --     |> Maybe.withDefault NotAsked
        personWithMeasurements =
            RemoteData.map (\a b c -> ( a, b, c )) personId
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
    in
    div [ class "page-activity acuteIllness" ] <|
        [ viewHeader language id activity
        , viewWebData language (viewContent language currentDate id activity model) identity personWithMeasurements
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.AcuteIllnessActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> Model -> ( PersonId, Person, AcuteIllnessMeasurements ) -> Html Msg
viewContent language currentDate id activity model ( personId, person, measurements ) =
    ((viewPersonDetails language currentDate person |> div [ class "item" ])
        :: viewActivity language currentDate id activity ( personId, measurements ) model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> AcuteIllnessEncounterId -> AcuteIllnessActivity -> ( PersonId, AcuteIllnessMeasurements ) -> Model -> List (Html Msg)
viewActivity language currentDate id activity ( personId, measurements ) model =
    case activity of
        _ ->
            [ div [] [ text "activity here" ] ]



-- viewAcuteIllnessContent : Language -> NominalDate -> AcuteIllnessEncounterId -> ( PersonId, AcuteIllnessMeasurements ) -> AcuteIllnessData -> List (Html Msg)
-- viewAcuteIllnessContent language currentDate id ( personId, measurements ) data =
--     let
--         activity =
--             AcuteIllnessAcuteIllness
--
--         form =
--             measurements.acuteIllness
--                 |> Maybe.map (Tuple.second >> .value)
--                 |> acuteIllnessFormWithDefault data.form
--
--         totalTasks =
--             1
--
--         tasksCompleted =
--             taskCompleted form.signs
--     in
--     [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
--     , div [ class "ui full segment" ]
--         [ div [ class "full content" ]
--             [ div [ class "ui form family-planning" ]
--                 [ p [] [ text <| translate language <| Translate.AcuteIllnessActivityHelper activity ]
--                 , viewLabel language Translate.SelectAllSigns
--                 , viewCheckBoxMultipleSelectInput language
--                     [ Edema, AbdominalDistension, DrySkin ]
--                     [ Apathy, PoorAppetite, BrittleHair ]
--                     (form.signs |> Maybe.withDefault [])
--                     (Just NormalChildAcuteIllness)
--                     SetAcuteIllnessSign
--                     Translate.ChildAcuteIllnessSignLabel
--                 ]
--             ]
--         , div [ class "actions" ]
--             [ button
--                 [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
--                 , onClick <| SaveAcuteIllness personId measurements.acuteIllness
--                 ]
--                 [ text <| translate language Translate.Save ]
--             ]
--         ]
--     ]
