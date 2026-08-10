module Pages.ChildScoreboard.Encounter.Utils exposing (childGotDiarrhea, generateAssembledData)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (NCDASign(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Measurement.Utils
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import RemoteData exposing (WebData)
import SyncManager.Model exposing (Site)


{-| Diarrhea recorded on the NCDA questionnaire sends the child to an acute
illness encounter. Both places an encounter can be ended ask this, so that
ending one from the progress report refers the child as ending it from the
encounter page does.
-}
childGotDiarrhea : AssembledData -> Bool
childGotDiarrhea assembled =
    getMeasurementValueFunc assembled.measurements.ncda
        |> Maybe.map (.signs >> EverySet.member ChildGotDiarrhea)
        |> Maybe.withDefault False


generateAssembledData : Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData =
    Measurement.Utils.generateAssembledDataForChildScoreboard
