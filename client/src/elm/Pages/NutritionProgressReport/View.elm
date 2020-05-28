module Pages.NutritionProgressReport.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..), MuacInCm(..), NutritionHeight, NutritionWeight, WeightInKg(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.ProgressReport.View exposing (viewFoundChild)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), Length(..), ZScore)
import ZScore.View


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> ModelIndexedDb -> Html Msg
view language currentDate zscores id db =
    generateAssembledData id db
        |> viewWebData language (viewContent language currentDate zscores db) identity


viewContent : Language -> NominalDate -> ZScore.Model.Model -> ModelIndexedDb -> AssembledData -> Html Msg
viewContent language currentDate zscores db data =
    let
        childId =
            data.participant.person

        child =
            data.person

        childMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.withDefault NotAsked

        expectedSessions =
            Dict.get childId db.expectedSessions
                |> Maybe.withDefault NotAsked

        individualChildMeasurements =
            ( currentDate, ( data.id, data.measurements ) )
                :: data.previousMeasurementsWithDates

        maybeRelationship =
            Dict.get data.participant.person db.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.andThen (Dict.values >> List.head)

        mother =
            maybeRelationship
                |> Maybe.andThen
                    (\relationship ->
                        Dict.get relationship.relatedTo db.people
                            |> Maybe.andThen RemoteData.toMaybe
                    )

        relation =
            maybeRelationship
                |> Maybe.map
                    (\relationship ->
                        case relationship.relatedBy of
                            MyParent ->
                                Translate.ChildOf

                            MyCaregiver ->
                                Translate.TakenCareOfBy

                            -- Other 2 options will never occur, as we deal with child here.
                            _ ->
                                Translate.ChildOf
                    )
                |> Maybe.withDefault Translate.ChildOf

        -- We're using nutrition value from the current session here, at
        -- least for now. So, we're ignoring any later sessions, and we're just
        -- leaving it blank if it wasn't entered in this session (rather than looking
        -- back to a previous session when it was entered).
        --
        -- See <https://github.com/Gizra/ihangane/issues/382#issuecomment-353273873>
        currentNutritionSigns =
            data.measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)
                |> Maybe.withDefault EverySet.empty

        defaultLastAssessmentDate =
            currentDate

        goBackAction =
            NutritionEncounterPage data.id
                |> UserPage
                |> App.Model.SetActivePage
    in
    viewWebData language
        (viewFoundChild language currentDate zscores ( childId, child ) individualChildMeasurements mother relation currentNutritionSigns defaultLastAssessmentDate goBackAction)
        identity
        (RemoteData.append expectedSessions childMeasurements)
