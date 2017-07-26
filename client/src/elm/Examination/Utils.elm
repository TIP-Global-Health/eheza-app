module Examination.Utils exposing (getLastExaminationFromChild)

import Child.Model exposing (Child)
import Examination.Model exposing (ExaminationChild)


getLastExaminationFromChild : Child -> Maybe ExaminationChild
getLastExaminationFromChild child =
    Just
        { height = Just 50.0
        , muac = Just 13.0
        , photo = Nothing
        , weight = Just 4.0
        }
