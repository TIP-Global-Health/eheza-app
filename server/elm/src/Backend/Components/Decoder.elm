module Backend.Components.Decoder exposing (decodeMenuData, decodeReportParams, decodeSelectedEntity, decodeSyncResponse)

import Backend.Components.Model exposing (HealthCenterData, MenuData, MenuScope(..), ReportParams, SelectedEntity(..), SyncResponse)
import Backend.Decoder exposing (decodeSite)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, fail, field, list, maybe, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


decodeHealthCenterData : Decoder HealthCenterData
decodeHealthCenterData =
    succeed HealthCenterData
        |> required "id" decodeInt
        |> required "name" string


decodeMenuData : Decoder MenuData
decodeMenuData =
    succeed MenuData
        |> required "site" decodeSite
        |> required "health_centers" (list decodeHealthCenterData)
        |> optional "scope" (maybe decodeMenuScope) Nothing


decodeMenuScope : Decoder MenuScope
decodeMenuScope =
    string
        |> andThen
            (\scope ->
                case scope of
                    "full" ->
                        succeed ScopeFull

                    "health_centers" ->
                        succeed ScopeHealthCenters

                    _ ->
                        fail <| scope ++ " is unknown MenuScope type"
            )


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


decodeReportParams : Decoder ReportParams
decodeReportParams =
    succeed ReportParams
        |> optional "province" (nullable string) Nothing
        |> optional "district" (nullable string) Nothing
        |> optional "sector" (nullable string) Nothing
        |> optional "cell" (nullable string) Nothing
        |> optional "village" (nullable string) Nothing
        |> optional "health_center" (nullable decodeInt) Nothing


decodeSyncResponse : Decoder record -> Decoder (SyncResponse record)
decodeSyncResponse decodeRecord =
    field "data"
        (succeed SyncResponse
            |> required "batch" (list decodeRecord)
            |> required "total_remaining" decodeInt
            |> required "last" decodeInt
        )
