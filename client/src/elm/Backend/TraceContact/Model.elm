module Backend.TraceContact.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { saveTraceContact : WebData ()
    }


emptyModel : Model
emptyModel =
    { saveTraceContact = NotAsked
    }


type Msg
    = EditTraceContact ContactTraceItem
    | HandleSavedTraceContact (WebData ())
