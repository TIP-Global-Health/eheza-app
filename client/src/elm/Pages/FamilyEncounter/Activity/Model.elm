module Pages.FamilyEncounter.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = NoOp
    | SetActivePage Page
      -- TODO: Add activity-specific messages in #1665


type alias Model =
    { fbfMotherData : FBFMotherData
    , fbfChildData : FBFChildData
    }


emptyModel : Model
emptyModel =
    { fbfMotherData = emptyFBFMotherData
    , fbfChildData = emptyFBFChildData
    }


type alias FBFMotherData =
    { placeholder : String
    }


emptyFBFMotherData : FBFMotherData
emptyFBFMotherData =
    { placeholder = ""
    }


type alias FBFChildData =
    { placeholder : String
    }


emptyFBFChildData : FBFChildData
emptyFBFChildData =
    { placeholder = ""
    }
