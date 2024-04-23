module Pages.EducationSession.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EverySet exposing (EverySet)
import Pages.EducationSession.Model exposing (Model)
import RemoteData


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
