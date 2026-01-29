module Pages.EducationSession.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))


fetch : EducationSessionId -> Maybe VillageId -> List MsgIndexedDb
fetch id mVillageId =
    Maybe.map
        (\villageId ->
            [ FetchVillages
            , FetchEducationSession id
            , FetchPeopleInVillage villageId
            ]
        )
        mVillageId
        |> Maybe.withDefault []
