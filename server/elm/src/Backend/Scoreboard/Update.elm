module Backend.Scoreboard.Update exposing (update)

import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Scoreboard.Utils exposing (generateECDMilestonesStatusByMonth)
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (decodeValue)
import Result


update : NominalDate -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate msg model =
    case msg of
        SetData value ->
            let
                data =
                    decodeValue (decodeScoreboardData currentDate) value

                dataWithECDMilestonesStatus =
                    Result.map
                        (\scoreboardData ->
                            let
                                updatedRecords =
                                    List.map
                                        (\record ->
                                            let
                                                -- Structure that holds monthly status of ECD milestones,
                                                -- when child is between 0 and 24 months old.
                                                ecdMilestonesStatusByMonth =
                                                    generateECDMilestonesStatusByMonth currentDate record.birthDate record.ncda.universalIntervention.row5.encountersData

                                                updatedUniversalInterventionData =
                                                    record.ncda.universalIntervention.row5
                                                        |> (\universalInterventionData -> { universalInterventionData | ecdMilestonesStatusByMonth = ecdMilestonesStatusByMonth })

                                                updatedUniversalIntervention =
                                                    record.ncda.universalIntervention
                                                        |> (\universalIntervention -> { universalIntervention | row5 = updatedUniversalInterventionData })

                                                updatedNCDA =
                                                    record.ncda |> (\ncda -> { ncda | universalIntervention = updatedUniversalIntervention })
                                            in
                                            { record | ncda = updatedNCDA }
                                        )
                                        scoreboardData.records
                            in
                            { scoreboardData | records = updatedRecords }
                        )
                        data

                modelUpdated =
                    { model | scoreboardData = Just dataWithECDMilestonesStatus }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
