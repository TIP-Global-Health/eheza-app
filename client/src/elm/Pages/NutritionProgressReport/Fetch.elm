module Pages.NutritionProgressReport.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb)
import Pages.NutritionEncounter.Fetch
import Pages.Person.Fetch exposing (fetchFamilyMembers)
import RemoteData exposing (RemoteData(..))


fetch : NutritionEncounterId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        encounter =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked

        fetchCmds =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )
                |> RemoteData.map
                    (\participant ->
                        [ Backend.Model.FetchRelationshipsForPerson participant.person

                        -- These 2 commands load data for Group context.
                        , Backend.Model.FetchChildMeasurements participant.person
                        , Backend.Model.FetchExpectedSessions participant.person
                        ]
                            ++ fetchFamilyMembers participant.person db
                    )
                |> RemoteData.withDefault []
    in
    Pages.NutritionEncounter.Fetch.fetch id db ++ fetchCmds
