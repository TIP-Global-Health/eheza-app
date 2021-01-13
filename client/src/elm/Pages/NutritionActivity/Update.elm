module Pages.NutritionActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (ChildNutritionSign(..), PhotoUrl(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Utils exposing (toSendToHCValueWithDefault)
import Pages.NutritionActivity.Model exposing (..)
import Pages.NutritionActivity.Utils exposing (..)
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

        SetHeight string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.heightData.form
                                |> (\form ->
                                        { form | height = String.toFloat string, heightDirty = True }
                                   )
                    in
                    model.heightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | heightData = updatedData }
            , Cmd.none
            , []
            )

        SaveHeight personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.heightData.form
                        |> toHeightValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveHeight personId measurementId value
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

        SetMuac string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.muacData.form
                                |> (\form ->
                                        { form | muac = String.toFloat string, muacDirty = True }
                                   )
                    in
                    model.muacData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | muacData = updatedData }
            , Cmd.none
            , []
            )

        SaveMuac personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.muacData.form
                        |> toMuacValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveMuac personId measurementId value
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

        DropZoneComplete result ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.photoData.form
                                |> (\form ->
                                        { form | url = Just (PhotoUrl result.url) }
                                   )
                    in
                    model.photoData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | photoData = updatedData }
            , Cmd.none
            , []
            )

        SavePhoto personId maybePhotoId url ->
            ( { model | photoData = emptyPhotoData }
            , Cmd.none
            , [ Backend.NutritionEncounter.Model.SavePhoto personId maybePhotoId url
                    |> Backend.Model.MsgNutritionEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
              ]
            )

        SetWeight string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.weightData.form
                                |> (\form ->
                                        { form | weight = String.toFloat string, weightDirty = True }
                                   )
                    in
                    model.weightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | weightData = updatedData }
            , Cmd.none
            , []
            )

        SaveWeight personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.weightData.form
                        |> toWeightValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveWeight personId measurementId value
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

        SetReferToHealthCenter value ->
            let
                form =
                    model.sendToHCData.form

                updatedForm =
                    { form | referToHealthCenter = Just value }

                updatedData =
                    model.sendToHCData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | sendToHCData = updatedData }
            , Cmd.none
            , []
            )

        SetHandReferralForm value ->
            let
                form =
                    model.sendToHCData.form

                updatedForm =
                    { form | handReferralForm = Just value }

                updatedData =
                    model.sendToHCData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | sendToHCData = updatedData }
            , Cmd.none
            , []
            )

        SaveSendToHC personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.sendToHCData.form
                        |> toSendToHCValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveSendToHC personId measurementId value
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
