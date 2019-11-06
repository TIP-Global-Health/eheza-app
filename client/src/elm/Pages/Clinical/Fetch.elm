module Pages.Clinical.Fetch exposing (fetch)

import Backend.Model exposing (MsgIndexedDb(..))


fetch : List MsgIndexedDb
fetch =
    [ FetchSyncData
    , FetchHealthCenters
    ]
