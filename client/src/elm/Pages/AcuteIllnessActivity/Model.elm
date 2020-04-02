module Pages.AcuteIllnessActivity.Model exposing
    ( AcuteIllnessData
    , AcuteIllnessForm
    , AcuteIllnessValue
    , Model
    , Msg(..)
    , emptyModel
    )

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page



-- | SetAcuteIllnessSign ChildAcuteIllnessSign
-- | SaveAcuteIllness PersonId (Maybe ( AcuteIllnessAcuteIllnessId, AcuteIllnessAcuteIllness ))


type alias Model =
    { -- acuteIllnessData : AcuteIllnessData
    }


emptyModel : Model
emptyModel =
    { -- acuteIllnessData = emptyAcuteIllnessData
    }


type alias AcuteIllnessData =
    { -- form : AcuteIllnessForm
    }


emptyAcuteIllnessData : AcuteIllnessData
emptyAcuteIllnessData =
    { -- form = AcuteIllnessForm Nothing
    }


type alias AcuteIllnessForm =
    { -- signs : Maybe (List ChildAcuteIllnessSign)
    }


type alias AcuteIllnessValue =
    { -- signs : Maybe (List ChildAcuteIllnessSign)
    }
