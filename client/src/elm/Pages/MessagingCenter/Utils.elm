module Pages.MessagingCenter.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
import Backend.Model exposing (ModelIndexedDb)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.MessagingCenter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)


zzz =
    24
