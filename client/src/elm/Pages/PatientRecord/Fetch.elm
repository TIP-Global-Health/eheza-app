module Pages.PatientRecord.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Person.Utils exposing (isPersonAnAdult)
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllnessParticipant.Fetch
import RemoteData exposing (RemoteData)


fetch : NominalDate -> PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate personId db =
    let
        msgsByAge =
            Dict.get personId db.people
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (\person ->
                        if isPersonAnAdult currentDate person == Just False then
                            Backend.NutritionEncounter.Fetch.fetch personId db

                        else
                            []
                    )
                |> Maybe.withDefault []
    in
    (FetchPerson personId
        :: Pages.AcuteIllnessParticipant.Fetch.fetch personId db
    )
        ++ msgsByAge
