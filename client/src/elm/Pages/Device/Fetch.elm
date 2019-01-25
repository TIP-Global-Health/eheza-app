module Pages.Device.Fetch exposing (fetch)

import Backend.Model
import Utils.WebData exposing (whenNotAsked)


fetch : Backend.Model.ModelIndexedDb -> List Backend.Model.MsgIndexedDb
fetch backend =
    let
        fetchHealthCenters =
            whenNotAsked Backend.Model.FetchHealthCenters backend.healthCenters
    in
    List.filterMap identity
        [ fetchHealthCenters ]
