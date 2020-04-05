module Pages.AcuteIllnessActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetActivePageSymptomsTask task ->
            let
                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        ToggleSymptomsGeneralSign sign ->
            let
                form =
                    model.symptomsData.symptomsGeneralForm

                signs =
                    form.signs

                updatedSigns =
                    if Dict.member sign signs then
                        Dict.remove sign signs

                    else
                        Dict.insert sign 1 signs

                updatedForm =
                    { form | signs = updatedSigns }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsGeneralForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        SetSymptomsGeneralSignValue sign string ->
            String.toInt string
                |> Maybe.map
                    (\value ->
                        let
                            form =
                                model.symptomsData.symptomsGeneralForm

                            signs =
                                form.signs

                            updatedForm =
                                { form | signs = Dict.insert sign value form.signs }

                            updatedData =
                                model.symptomsData
                                    |> (\data -> { data | symptomsGeneralForm = updatedForm })
                        in
                        ( { model | symptomsData = updatedData }
                        , Cmd.none
                        , []
                        )
                    )
                |> Maybe.withDefault noChange

        SaveSymptomsGeneral personId saved nextTask ->
            ( model
            , Cmd.none
            , []
            )

        SaveSymptomsRespiratory personId saved nextTask ->
            ( model
            , Cmd.none
            , []
            )

        SaveSymptomsGI personId saved nextTask ->
            ( model
            , Cmd.none
            , []
            )



-- SetAcuteIllnessSign sign ->
--     let
--         form =
--             Dict.get id db.acuteIllnessMeasurements
--                 |> Maybe.withDefault NotAsked
--                 |> RemoteData.toMaybe
--                 |> Maybe.map (.acuteIllness >> Maybe.map (Tuple.second >> .value) >> acuteIllnessFormWithDefault model.acuteIllnessData.form)
--                 |> Maybe.withDefault model.acuteIllnessData.form
--
--         updatedForm =
--             case form.signs of
--                 Just signs ->
--                     if List.member sign signs then
--                         let
--                             updatedSigns =
--                                 if List.length signs == 1 then
--                                     Nothing
--
--                                 else
--                                     signs |> List.filter ((/=) sign) |> Just
--                         in
--                         { form | signs = updatedSigns }
--
--                     else
--                         case sign of
--                             NormalChildAcuteIllness ->
--                                 { form | signs = Just [ sign ] }
--
--                             _ ->
--                                 let
--                                     updatedSigns =
--                                         case signs of
--                                             [ NormalChildAcuteIllness ] ->
--                                                 Just [ sign ]
--
--                                             _ ->
--                                                 Just (sign :: signs)
--                                 in
--                                 { form | signs = updatedSigns }
--
--                 Nothing ->
--                     { form | signs = Just [ sign ] }
--
--         updatedData =
--             model.acuteIllnessData
--                 |> (\data -> { data | form = updatedForm })
--     in
--     ( { model | acuteIllnessData = updatedData }
--     , Cmd.none
--     , []
--     )
--
-- SaveAcuteIllness personId saved ->
--     let
--         measurementId =
--             Maybe.map Tuple.first saved
--
--         measurement =
--             Maybe.map (Tuple.second >> .value) saved
--
--         appMsgs =
--             model.acuteIllnessData.form
--                 |> toAcuteIllnessValueWithDefault measurement
--                 |> unwrap
--                     []
--                     (\value ->
--                         [ Backend.AcuteIllnessEncounter.Model.SaveAcuteIllness personId measurementId value
--                             |> Backend.Model.MsgAcuteIllnessEncounter id
--                             |> App.Model.MsgIndexedDb
--                         , App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
--                         ]
--                     )
--     in
--     ( model
--     , Cmd.none
--     , appMsgs
--     )
