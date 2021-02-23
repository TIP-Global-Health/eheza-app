module Backend.PmtctParticipant.Decoder exposing (decodePmtctParticipant)

import Backend.PmtctParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Restful.Endpoint exposing (decodeEntityUuid)


decodePmtctParticipant : Decoder PmtctParticipant
decodePmtctParticipant =
    succeed PmtctParticipant
        |> required "person" decodeEntityUuid
        |> required "adult" decodeEntityUuid
        |> required "adult_activities" decodeAdultActivities
        |> requiredAt [ "expected", "value" ] decodeYYYYMMDD
        |> optionalAt [ "expected", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "clinic" decodeEntityUuid
        |> required "deleted" bool


decodeAdultActivities : Decoder AdultActivities
decodeAdultActivities =
    andThen
        (\s ->
            case s of
                "caregiver" ->
                    succeed CaregiverActivities

                "mother" ->
                    succeed MotherActivities

                _ ->
                    fail <| s ++ " is not a recognized AdultActivities"
        )
        string
