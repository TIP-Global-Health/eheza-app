module Measurement.Utils exposing (..)

import Activity.Utils exposing (expectCounselingActivity)
import Backend.Counseling.Model exposing (CounselingTiming(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValue, mapMeasurementData)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildMeasurementData, getMotherMeasurementData)
import EveryDict
import EverySet
import Measurement.Model exposing (..)


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { minVal = 0.5
    , maxVal = 100
    }


getInputConstraintsMuac : FloatInputConstraints
getInputConstraintsMuac =
    { minVal = 0.5
    , maxVal = 40
    }


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { minVal = 0.5
    , maxVal = 60
    }


{-| Initialize (or reset) a form with the given data.
-}
fromChildMeasurementData : MeasurementData ChildMeasurements ChildEdits -> ModelChild
fromChildMeasurementData data =
    -- TODO: Clearly there is some kind of pattern below, but we won't try to abstract that
    -- just yet. Ultimately, this is the kind of thing which `RestfulData` would organize.
    { height =
        data
            |> mapMeasurementData .height .height
            |> currentValue
            |> Maybe.map (.value >> (\(HeightInCm cm) -> toString cm))
            |> Maybe.withDefault ""
    , muac =
        data
            |> mapMeasurementData .muac .muac
            |> currentValue
            |> Maybe.map (.value >> (\(MuacInCm cm) -> toString cm))
            |> Maybe.withDefault ""
    , nutritionSigns =
        data
            |> mapMeasurementData .nutrition .nutrition
            |> currentValue
            |> Maybe.map .value
            |> Maybe.withDefault EverySet.empty
    , counseling =
        data
            |> mapMeasurementData .counselingSession .counseling
            |> currentValue
            |> Maybe.map .value
    , photo =
        data
            |> mapMeasurementData .photo .photo
            |> currentValue
            |> Maybe.map .value
    , weight =
        data
            |> mapMeasurementData .weight .weight
            |> currentValue
            |> Maybe.map (.value >> (\(WeightInKg kg) -> toString kg))
            |> Maybe.withDefault ""
    }


{-| Initialize (or reset) a form with the given data.
-}
fromMotherMeasurementData : MeasurementData MotherMeasurements MotherEdits -> ModelMother
fromMotherMeasurementData data =
    { familyPlanningSigns =
        data
            |> mapMeasurementData .familyPlanning .familyPlanning
            |> currentValue
            |> Maybe.map .value
            |> Maybe.withDefault EverySet.empty
    }


getMotherForm : MotherId -> EditableSession -> ModelMother
getMotherForm motherId session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromMotherMeasurementData` would get calculated every time
    case EveryDict.get motherId session.motherForms of
        Just motherForm ->
            motherForm

        Nothing ->
            getMotherMeasurementData motherId session
                |> fromMotherMeasurementData


getChildForm : ChildId -> EditableSession -> ModelChild
getChildForm childId session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromChildMeasurementData` would get calculated every time
    case EveryDict.get childId session.childForms of
        Just childForm ->
            childForm

        Nothing ->
            getChildMeasurementData childId session
                |> fromChildMeasurementData
                |> (\form ->
                        -- We need some special logic for the counseling
                        -- session, to fill in the correct kind of session.
                        -- This seems to be the best place to do that, though
                        -- that may need some more thinking at some point.
                        case form.counseling of
                            Just _ ->
                                form

                            Nothing ->
                                { form
                                    | counseling =
                                        expectCounselingActivity session childId
                                            |> Maybe.map
                                                (\timing ->
                                                    ( timing, EverySet.empty )
                                                )
                                }
                   )
