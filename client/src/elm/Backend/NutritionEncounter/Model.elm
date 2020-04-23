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
    , saveHeight : WebData ()
    , saveMuac : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    , saveWeight : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeNutritionEncounter = NotAsked
    , saveHeight = NotAsked
    , saveMuac = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    , saveWeight = NotAsked
    }


type Msg
    = CloseNutritionEncounter
    | HandleClosedNutritionEncounter (WebData ())
    | SaveHeight PersonId (Maybe NutritionHeightId) HeightInCm
    | HandleSavedHeight (WebData ())
    | SaveMuac PersonId (Maybe NutritionMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveNutrition PersonId (Maybe NutritionNutritionId) (EverySet ChildNutritionSign)
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe NutritionPhotoId) PhotoUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe NutritionWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
