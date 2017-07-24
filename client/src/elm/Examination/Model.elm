module Examination.Model exposing (..)

import EveryDictList exposing (EveryDictList)
import Measurement.Model exposing (ChildMeasurements, MotherMeasurements)
import RemoteData exposing (WebData)


type ExaminationId
    = Examination Int


type ExaminationStorage
    = New (WebData ExaminationId)
    | Existing (WebData ExaminationId)


type alias EveryDictListExaminationsChild =
    EveryDictList ExaminationStorage ChildMeasurements


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationStorage MotherMeasurements
