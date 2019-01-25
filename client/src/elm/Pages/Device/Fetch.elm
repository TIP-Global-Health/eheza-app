module Pages.Device.Fetch exposing (fetch)

import Backend.Model
import Utils.WebData exposing (whenNotAsked)


fetch : Backend.Model.ModelIndexedDb -> List Backend.Model.MsgIndexedDb
fetch backend =
    List.filterMap identity
        [ whenNotAsked Backend.Model.FetchHealthCenters backend.healthCenters
        , whenNotAsked Backend.Model.FetchSyncData backend.syncData
        ]
