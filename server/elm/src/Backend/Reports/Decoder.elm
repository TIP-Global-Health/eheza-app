module Backend.Reports.Decoder exposing (decodeReportsData)

import AssocList as Dict
import Backend.Decoder exposing (decodeSite)
import Backend.Reports.Model exposing (..)
import Backend.Reports.Utils exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Maybe.Extra exposing (isNothing)


decodeReportsData : Decoder ReportsData
decodeReportsData =
    succeed ReportsData
        |> required "site" decodeSite
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list decodePatientData)


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "global" ->
                        succeed EntityGlobal

                    "province" ->
                        succeed EntityProvince

                    "district" ->
                        succeed EntityDistrict

                    "sector" ->
                        succeed EntitySector

                    "cell" ->
                        succeed EntityCell

                    "village" ->
                        succeed EntityVillage

                    "health-center" ->
                        succeed EntityHealthCenter

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )


decodePatientData : Decoder PatientData
decodePatientData =
    succeed PatientData
        |> required "created" decodeYYYYMMDD
        |> required "birth_date" decodeYYYYMMDD
