module Measurement.Utils exposing (fromChildMeasurementData, fromMotherMeasurementData, getChildForm, getInputConstraintsHeight, getInputConstraintsMuac, getInputConstraintsWeight, getMotherForm)

import Activity.Utils exposing (expectCounselingActivity, expectParticipantConsent)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValue, currentValues, mapMeasurementData)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildMeasurementData, getMotherMeasurementData)
import EverySet
import LocalData
import Measurement.Model exposing (..)
import Pages.Session.Model


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
fromChildMeasurementData : MeasurementData ChildMeasurements -> ModelChild
fromChildMeasurementData data =
    -- TODO: Clearly there is some kind of pattern below, but we won't try to abstract that
    -- just yet. Ultimately, this is the kind of thing which `RestfulData` would organize.
    { height =
        data
            |> mapMeasurementData .height
            |> currentValue
            |> Maybe.map (.value >> (\(HeightInCm cm) -> String.fromFloat cm))
            |> Maybe.withDefault ""
    , muac =
        data
            |> mapMeasurementData .muac
            |> currentValue
            |> Maybe.map (.value >> (\(MuacInCm cm) -> String.fromFloat cm))
            |> Maybe.withDefault ""
    , nutritionSigns =
        data
            |> mapMeasurementData .nutrition
            |> currentValue
            |> Maybe.map .value
            |> Maybe.withDefault EverySet.empty
    , counseling =
        data
            |> mapMeasurementData .counselingSession
            |> currentValue
            |> Maybe.map .value
    , photo =
        data
            |> mapMeasurementData .photo
            |> currentValue
            |> Maybe.map .value
    , weight =
        data
            |> mapMeasurementData .weight
            |> currentValue
            |> Maybe.map (.value >> (\(WeightInKg kg) -> String.fromFloat kg))
            |> Maybe.withDefault ""
    }


{-| Initialize (or reset) a form with the given data.
-}
fromMotherMeasurementData : MeasurementData MotherMeasurements -> ModelMother
fromMotherMeasurementData data =
    let
        -- We show the UI as completed for all current consents
        progress =
            data
                |> mapMeasurementData .consent
                |> currentValues
                |> List.map (Tuple.second >> .value >> .formId)
                |> List.map (\formId -> ( formId, completedParticipantFormProgress ))
                |> Dict.fromList
    in
    { familyPlanningSigns =
        data
            |> mapMeasurementData .familyPlanning
            |> currentValue
            |> Maybe.map .value
            |> Maybe.withDefault EverySet.empty
    , participantConsent =
        { expected = Dict.empty
        , view = Nothing
        , progress = progress
        }
    }


getMotherForm : PersonId -> Pages.Session.Model.Model -> EditableSession -> ModelMother
getMotherForm motherId pages session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromMotherMeasurementData` would get calculated every time
    case Dict.get motherId pages.motherForms of
        Just motherForm ->
            motherForm

        Nothing ->
            getMotherMeasurementData motherId session
                |> LocalData.unwrap
                    emptyModelMother
                    (fromMotherMeasurementData
                        >> (\form ->
                                { form
                                    | participantConsent =
                                        { expected = expectParticipantConsent session.offlineSession motherId
                                        , view = Nothing
                                        , progress = form.participantConsent.progress
                                        }
                                }
                           )
                    )


getChildForm : PersonId -> Pages.Session.Model.Model -> EditableSession -> ModelChild
getChildForm childId pages session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromChildMeasurementData` would get calculated every time
    case Dict.get childId pages.childForms of
        Just childForm ->
            childForm

        Nothing ->
            getChildMeasurementData childId session
                |> LocalData.unwrap
                    emptyModelChild
                    (fromChildMeasurementData
                        >> (\form ->
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
                    )
