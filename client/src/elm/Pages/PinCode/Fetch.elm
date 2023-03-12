module Pages.PinCode.Fetch exposing (fetch)

import Backend.Entities exposing (NurseId)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : Maybe NurseId -> List MsgIndexedDb
fetch nurseId =
    [ FetchHealthCenters
    , FetchClinics
    , FetchVillages
    ]
        ++ -- Need to fetch resilience messages to be able to
           -- display number of unread messages on Wellbeing activity.
           (Maybe.map (FetchResilienceMessagesForNurse >> List.singleton) nurseId
                |> Maybe.withDefault []
           )
