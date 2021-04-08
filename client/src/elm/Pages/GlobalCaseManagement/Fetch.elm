module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (generateNutritionFollowUps)
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId db =
    let
        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        nutritionFollowUps =
            followUps
                |> Maybe.map (generateNutritionFollowUps currentDate healthCenterId)
                |> Maybe.withDefault Dict.empty

        people =
            Dict.keys nutritionFollowUps

        fetchIndividualParticipantsMsgs =
            List.map FetchIndividualEncounterParticipantsForPerson people

        fetchHomeVisitEncountersMsgs =
            people
                |> List.map
                    (\personId ->
                        resolveIndividualParticipantsForPerson personId HomeVisitEncounter db
                            |> List.map FetchHomeVisitEncountersForParticipant
                    )
                |> List.concat
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    , FetchFollowUpParticipants people
    ]
        ++ fetchIndividualParticipantsMsgs
        ++ fetchHomeVisitEncountersMsgs
