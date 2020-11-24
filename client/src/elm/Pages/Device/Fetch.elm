module Pages.Device.Fetch exposing (fetch)

import Backend.Model


fetch : List Backend.Model.MsgIndexedDb
fetch =
    [ Backend.Model.FetchHealthCenters
    ]
