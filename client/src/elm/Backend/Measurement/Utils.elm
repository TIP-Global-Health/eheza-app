module Backend.Measurement.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDict


{-| Picks out a particular mother's `EditableMeasurements`. If not found, makes
an empty one.
-}
getEditableMotherMeasurements : MotherId -> EditableMeasurements -> EditableMotherMeasurements
getEditableMotherMeasurements motherId measurements =
    EveryDict.get motherId measurements.mothers
        |> Maybe.withDefault emptyEditableMotherMeasurements


{-| Picks out a particular child's `EditableMeasurements`. If not found, makes
an empty one.
-}
getEditableChildMeasurements : ChildId -> EditableMeasurements -> EditableChildMeasurements
getEditableChildMeasurements childId measurements =
    EveryDict.get childId measurements.children
        |> Maybe.withDefault emptyEditableChildMeasurements


{-| Given a MUAC in cm, classify according to the measurement tool shown
at <https://github.com/Gizra/ihangane/issues/282>
-}
muacIndication : MuacValue -> MuacIndication
muacIndication (MuacValue value) =
    if value <= 11.5 then
        MuacRed
    else if value <= 12.5 then
        MuacYellow
    else
        MuacGreen
