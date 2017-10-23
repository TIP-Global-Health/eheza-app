module Backend.Measurement.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDict


{-| Picks out a particular mother's `MotherEdits`. If not found, makes
an empty one.
-}
getMotherEdits : MotherId -> MeasurementEdits -> MotherEdits
getMotherEdits motherId measurements =
    EveryDict.get motherId measurements.mothers
        |> Maybe.withDefault emptyMotherEdits


{-| Picks out a particular child's `ChildEdits`. If not found, makes
an empty one.
-}
getChildEdits : ChildId -> MeasurementEdits -> ChildEdits
getChildEdits childId measurements =
    EveryDict.get childId measurements.children
        |> Maybe.withDefault emptyChildEdits


{-| Given a MUAC in cm, classify according to the measurement tool shown
at <https://github.com/Gizra/ihangane/issues/282>
-}
muacIndication : MuacInCm -> MuacIndication
muacIndication (MuacInCm value) =
    if value <= 11.5 then
        MuacRed
    else if value <= 12.5 then
        MuacYellow
    else
        MuacGreen
