module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> HealthCenterId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId db =
    let
        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        --
        --  Nutrition follows ups calculations.
        --
        nutritionFollowUps =
            followUps
                |> Maybe.map (generateNutritionFollowUps db)
                |> Maybe.withDefault Dict.empty

        peopleForNutrition =
            Dict.keys nutritionFollowUps

        fetchIndividualParticipantsMsgs =
            List.map FetchIndividualEncounterParticipantsForPerson peopleForNutrition

        fetchHomeVisitEncountersMsgs =
            peopleForNutrition
                |> List.map
                    (\personId ->
                        resolveIndividualParticipantsForPerson personId HomeVisitEncounter db
                            |> List.map FetchHomeVisitEncountersForParticipant
                    )
                |> List.concat

        --
        --  Acute illness follows ups calculations.
        --
        acuteIllnessEncounters =
            followUps
                |> Maybe.map generateAcuteIllnessEncounters
                |> Maybe.withDefault EverySet.empty

        acuteIllnessParticipants =
            generateAcuteIllnessParticipants acuteIllnessEncounters db

        fetchAcuteIllnessEncountersMsgs =
            EverySet.toList acuteIllnessEncounters
                |> List.map FetchAcuteIllnessEncounter

        fetchAcuteIllnessParticipantsMsgs =
            EverySet.toList acuteIllnessParticipants
                |> List.map FetchIndividualEncounterParticipant

        fetchAcuteIllnessEncountersForParticipantMsgs =
            EverySet.toList acuteIllnessParticipants
                |> List.map FetchAcuteIllnessEncountersForParticipant

        acuteIllnessFollowUps =
            followUps
                |> Maybe.map (generateAcuteIllnessFollowUps db)
                |> Maybe.withDefault Dict.empty

        peopleForAccuteIllness =
            Dict.keys acuteIllnessFollowUps
                |> List.map Tuple.second

        --
        --  Prenatal follows ups calculations.
        --
        prenatalEncounters =
            followUps
                |> Maybe.map generatePrenatalEncounters
                |> Maybe.withDefault EverySet.empty

        prenatalParticipants =
            generatePrenatalParticipants prenatalEncounters db

        fetchPrenatalEncountersMsgs =
            EverySet.toList prenatalEncounters
                |> List.map FetchPrenatalEncounter

        fetchPrenatalParticipantsMsgs =
            EverySet.toList prenatalParticipants
                |> List.map FetchIndividualEncounterParticipant

        fetchPrenatalEncountersForParticipantMsgs =
            EverySet.toList prenatalParticipants
                |> List.map FetchPrenatalEncountersForParticipant

        prenatalFollowUps =
            followUps
                |> Maybe.map (generatePrenatalFollowUps db)
                |> Maybe.withDefault Dict.empty

        peopleForPrenatal =
            Dict.keys prenatalFollowUps
                |> List.map Tuple.second

        --
        --  Trace Contacts calculations.
        --
        traceReporters =
            Maybe.map (.traceContacts >> Dict.values >> List.map .participantId)
                followUps
                |> Maybe.withDefault []

        --
        -- People for all types of follow ups.
        --
        people =
            peopleForNutrition
                ++ peopleForAccuteIllness
                ++ peopleForPrenatal
                ++ traceReporters
                |> EverySet.fromList
                |> EverySet.toList
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    , FetchFollowUpParticipants people
    ]
        ++ fetchIndividualParticipantsMsgs
        ++ fetchHomeVisitEncountersMsgs
        ++ fetchAcuteIllnessEncountersMsgs
        ++ fetchAcuteIllnessParticipantsMsgs
        ++ fetchAcuteIllnessEncountersForParticipantMsgs
        ++ fetchPrenatalEncountersMsgs
        ++ fetchPrenatalParticipantsMsgs
        ++ fetchPrenatalEncountersForParticipantMsgs
