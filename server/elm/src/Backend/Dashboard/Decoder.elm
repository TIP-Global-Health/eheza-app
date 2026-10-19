module Backend.Dashboard.Decoder exposing (decodeDashboardData, decodeSyncResponse)

import Backend.Components.Decoder exposing (decodeMenuData)
import Backend.Components.Model exposing (SyncResponse)
import Backend.Dashboard.Model
    exposing
        ( ChildRecord
        , DashboardData
        , DeliveryLocation(..)
        , PregnancyRecord
        , PrenatalIndicator(..)
        , PrenatalVisit
        )
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, field, list, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)


decodeDashboardData : Decoder DashboardData
decodeDashboardData =
    succeed DashboardData
        |> optional "first_cohort_date" (nullable decodeYYYYMMDD) Nothing
        |> custom decodeMenuData
        |> hardcoded []
        |> hardcoded Nothing


decodeSyncResponse : Decoder (SyncResponse PregnancyRecord)
decodeSyncResponse =
    Backend.Components.Decoder.decodeSyncResponse decodePregnancyRecord


decodePregnancyRecord : Decoder PregnancyRecord
decodePregnancyRecord =
    let
        risks =
            field "risks" (list string)
    in
    succeed PregnancyRecord
        |> required "hc" decodeInt
        |> required "edd" (nullable decodeYYYYMMDD)
        |> required "dc" (nullable decodeYYYYMMDD)
        |> required "ol" (map (Maybe.andThen deliveryLocationFromCode) (nullable string))
        |> required "visits" (list decodePrenatalVisit)
        |> required "us" (list decodeYYYYMMDD)
        |> custom (map (not << List.isEmpty) risks)
        |> custom (map (List.member "pe") risks)
        |> required "child" (nullable decodeChildRecord)


deliveryLocationFromCode : String -> Maybe DeliveryLocation
deliveryLocationFromCode code =
    case code of
        "f" ->
            Just FacilityDelivery

        "h" ->
            Just HomeDelivery

        _ ->
            Nothing


decodePrenatalVisit : Decoder PrenatalVisit
decodePrenatalVisit =
    succeed PrenatalVisit
        |> required "d" decodeYYYYMMDD
        |> required "t" (map (\encounterType -> not (List.member encounterType [ "nurse-postpartum", "chw-postpartum" ])) string)
        |> required "ind" (map (List.filterMap prenatalIndicatorFromCode) (list string))


prenatalIndicatorFromCode : String -> Maybe PrenatalIndicator
prenatalIndicatorFromCode code =
    case code of
        "d" ->
            Just IndicatorAspirin

        "e" ->
            Just IndicatorCalcium

        _ ->
            Nothing


decodeChildRecord : Decoder ChildRecord
decodeChildRecord =
    succeed ChildRecord
        |> required "visits" (list (field "d" decodeYYYYMMDD))
