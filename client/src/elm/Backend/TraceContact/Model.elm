module Backend.TraceContact.Model exposing (Model, Msg(..), emptyModel)

import Backend.Measurement.Model exposing (..)
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
