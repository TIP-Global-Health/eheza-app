module Pages.PinCode.Fetch exposing (fetch)

import Backend.Entities exposing (HealthCenterId)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : Maybe HealthCenterId -> List MsgIndexedDb
fetch healthCenterId =
    [ FetchHealthCenters
    , FetchClinics
    , FetchVillages
    ]
