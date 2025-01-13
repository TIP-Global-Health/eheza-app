module Pages.EducationSession.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Pages.EducationSession.Model exposing (Model)


fetch : EducationSessionId -> Maybe VillageId -> ModelIndexedDb -> Model -> List MsgIndexedDb
fetch id mVillageId db model =
    Maybe.map
        (\villageId ->
            [ FetchVillages
            , FetchEducationSession id
            , FetchPeopleInVillage villageId
            ]
        )
        mVillageId
        |> Maybe.withDefault []
