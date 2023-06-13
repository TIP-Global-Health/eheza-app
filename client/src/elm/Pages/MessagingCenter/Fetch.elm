module Pages.MessagingCenter.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.MessagingCenter.Model exposing (..)
import Pages.MessagingCenter.Utils exposing (..)
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> NurseId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate nurseId db =
    [ FetchResilienceMessagesForNurse nurseId
    ]
