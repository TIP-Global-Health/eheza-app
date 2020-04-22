module Backend.NutritionEncounter.Model exposing (Model, Msg(..), NutritionEncounter, emptyModel)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias NutritionEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeNutritionEncounter : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeNutritionEncounter = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    }


type Msg
    = CloseNutritionEncounter
    | HandleClosedNutritionEncounter (WebData ())
    | SaveNutrition PersonId (Maybe NutritionNutritionId) (EverySet ChildNutritionSign)
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe NutritionPhotoId) PhotoUrl
    | HandleSavedPhoto (WebData ())
