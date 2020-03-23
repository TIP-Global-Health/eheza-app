module Pages.NutritionActivity.Update exposing (update)

import App.Model
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (ChildNutritionSign(..))
import Backend.Model
import Backend.NutritionEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.NutritionActivity.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Result exposing (Result)


update : NominalDate -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetNutritionSign sign ->
            let
                form =
                    model.nutritionData.form

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

        SaveNutrition nutritionEncounterId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    []

                -- Todo:
                -- model.nutritionData.form
                --     |> toFamilyPlanningValueWithDefault measurement
                --     |> unwrap
                --         []
                --         (\value ->
                --             [ Backend.NutritionEncounter.Model.SaveFamilyPlanning personId measurementId value
                --                 |> Backend.Model.MsgNutritionEncounter nutritionEncounterId
                --                 |> App.Model.MsgIndexedDb
                --             , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage nutritionEncounterId
                --             ]
                --         )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
