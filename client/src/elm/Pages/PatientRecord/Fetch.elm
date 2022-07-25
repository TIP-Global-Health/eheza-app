module Pages.PatientRecord.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.NutritionEncounter.Fetch
import Backend.Person.Utils exposing (isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.AcuteIllness.Participant.Fetch
import Pages.Prenatal.Participant.Fetch
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
                            let
                                fetchChildrenMsgs =
                                    Dict.get personId db.relationshipsByPerson
                                        |> Maybe.andThen RemoteData.toMaybe
                                        |> Maybe.map
                                            (Dict.values
                                                >> List.filter (.relatedBy >> (==) MyChild)
                                                >> List.map (.relatedTo >> FetchPerson)
                                            )
                                        |> Maybe.withDefault []
                            in
                            FetchMotherMeasurements personId
                                :: fetchChildrenMsgs
                                ++ Pages.Prenatal.Participant.Fetch.fetch personId db
                    )
                |> Maybe.withDefault []
    in
    [ FetchPerson personId
    , FetchRelationshipsForPerson personId
    ]
        ++ Pages.AcuteIllness.Participant.Fetch.fetch personId db
        ++ msgsByAge
