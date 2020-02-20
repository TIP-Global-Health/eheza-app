module Pages.PinCode.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))


fetch : List MsgIndexedDb
fetch =
    [ FetchHealthCenters
    , FetchVillages
    ]
