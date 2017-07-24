module Examination.Model exposing (..)

import EveryDictList exposing (EveryDictList)
import Measurement.Model exposing (MeasurementsType)
import RemoteData exposing (WebData)


type ExaminationId
    = Examination Int


type ExaminationStorage
    = New (WebData ExaminationId)
    | Existing ExaminationId


type alias EveryDictListExaminations =
    EveryDictList ExaminationStorage (WebData MeasurementsType)
