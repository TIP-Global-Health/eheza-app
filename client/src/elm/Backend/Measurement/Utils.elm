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
