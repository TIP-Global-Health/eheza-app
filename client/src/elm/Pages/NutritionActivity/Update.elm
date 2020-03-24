module Pages.NutritionActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (ChildNutritionSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionActivity.Utils exposing (nutritionFormWithDefault, toNutritionValueWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> NutritionEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetNutritionSign sign ->
            let
                form =
                    Dict.get id db.nutritionMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map (.nutrition >> Maybe.map (Tuple.second >> .value) >> nutritionFormWithDefault model.nutritionData.form)
                        |> Maybe.withDefault model.nutritionData.form

                updatedForm =
                    case form.signs of
                        Just signs ->
                            if List.member sign signs then
                                let
                                    updatedSigns =
                                        if List.length signs == 1 then
                                            Nothing

                                        else
                                            signs |> List.filter ((/=) sign) |> Just
                                in
                                { form | signs = updatedSigns }

                            else
                                case sign of
                                    NormalChildNutrition ->
                                        { form | signs = Just [ sign ] }

                                    _ ->
                                        let
                                            updatedSigns =
                                                case signs of
                                                    [ NormalChildNutrition ] ->
                                                        Just [ sign ]

                                                    _ ->
                                                        Just (sign :: signs)
                                        in
                                        { form | signs = updatedSigns }

                        Nothing ->
                            { form | signs = Just [ sign ] }

                updatedData =
                    model.nutritionData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | nutritionData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutrition personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.nutritionData.form
                        |> toNutritionValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveNutrition personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
