module Pages.PinCode.Fetch exposing (fetch)

import Backend.Entities exposing (NurseId)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : Maybe NurseId -> List MsgIndexedDb
fetch _ =
    [ FetchHealthCenters
    , FetchClinics
    , FetchVillages
    ]
