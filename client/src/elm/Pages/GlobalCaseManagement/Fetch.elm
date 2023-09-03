module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Backend.Village.Model exposing (Village)
import Backend.Village.Utils exposing (getVillageById, isVillageResident)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Utils
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> HealthCenterId -> Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId villageId db =
    let
        fetchForAuthorityMsgs =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (\followUps ->
                        Maybe.andThen
                            (getVillageById db
                                >> Maybe.map (\village -> fetchForCHWAtVillage currentDate village db followUps)
                            )
                            villageId
                            |> Maybe.withDefault (fetchForNurseAtHealthCenter currentDate db followUps)
                    )
                |> Maybe.withDefault []
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    ]
        ++ fetchForAuthorityMsgs


fetchForCHWAtVillage : NominalDate -> Village -> ModelIndexedDb -> FollowUpMeasurements -> List MsgIndexedDb
fetchForCHWAtVillage currentDate village db followUps =
    let
        ( peopleForNutrition, peopleForAccuteIllness, peopleForPrenatal ) =
            resolveUniquePatientsFromFollowUps currentDate followUps

        -- We need to fetch all people in follow ups, to determine if person
        -- is resident of village or not.
        -- Then, we'll fetch participants and encounters data only for
        -- those that are residents.
        peopleForFetch =
            peopleForNutrition
                ++ peopleForAccuteIllness
                ++ peopleForPrenatal
                |> Pages.Utils.unique

        residentsForNutrition =
            filterResidents db village peopleForNutrition

        followUpsForResidents =
            generateFollowUpsForResidents currentDate village db followUps ( peopleForNutrition, peopleForAccuteIllness, peopleForPrenatal )

        --
        --  Nutrition follows ups calculations.
        --
        fetchIndividualParticipantsMsg =
            FetchIndividualEncounterParticipantsForPeople residentsForNutrition

        fetchHomeVisitEncountersMsg =
            List.map (\personId -> resolveIndividualParticipantsForPerson personId HomeVisitEncounter db)
                residentsForNutrition
                |> List.concat
                |> FetchHomeVisitEncountersForParticipants

        --
        --  Acute illness follows ups calculations.
        --
        acuteIllnessEncounters =
            generateAcuteIllnessEncounters followUpsForResidents

        acuteIllnessParticipants =
            generateAcuteIllnessParticipants acuteIllnessEncounters db

        fetchAcuteIllnessEncountersMsg =
            EverySet.toList acuteIllnessEncounters
                |> FetchAcuteIllnessEncounters

        fetchAcuteIllnessParticipantsMsg =
            EverySet.toList acuteIllnessParticipants
                |> FetchIndividualEncounterParticipants

        fetchAcuteIllnessEncountersForParticipantMsg =
            EverySet.toList acuteIllnessParticipants
                |> FetchAcuteIllnessEncountersForParticipants

        --
        --  Prenatal follows ups calculations.
        --
        prenatalEncounters =
            generatePrenatalEncounters followUpsForResidents

        prenatalParticipants =
            generatePrenatalParticipants prenatalEncounters db

        fetchPrenatalEncountersMsg =
            EverySet.toList prenatalEncounters
                |> FetchPrenatalEncounters

        fetchPrenatalParticipantsMsg =
            EverySet.toList prenatalParticipants
                |> FetchIndividualEncounterParticipants

        fetchPrenatalEncountersForParticipantMsg =
            EverySet.toList prenatalParticipants
                |> FetchPrenatalEncountersForParticipants
    in
    [ FetchFollowUpParticipants peopleForFetch
    , fetchAcuteIllnessEncountersMsg
    , fetchPrenatalEncountersMsg
    , fetchAcuteIllnessParticipantsMsg
    , fetchPrenatalParticipantsMsg
    , fetchHomeVisitEncountersMsg
    , fetchAcuteIllnessEncountersForParticipantMsg
    , fetchPrenatalEncountersForParticipantMsg
    , fetchIndividualParticipantsMsg
    ]


fetchForNurseAtHealthCenter : NominalDate -> ModelIndexedDb -> FollowUpMeasurements -> List MsgIndexedDb
fetchForNurseAtHealthCenter currentDate db followUps =
    let
        --
        --  Trace Contacts calculations.
        --
        traceReporters =
            Dict.values followUps.traceContacts
                |> List.map .participantId

        --
        --  Prenatal labs results calculations.
        --
        peopleForPrenatalLabsResults =
            Dict.values followUps.prenatalLabs
                |> List.map .participantId

        --
        --  NCD labs results calculations.
        --
        peopleForNCDLabsResults =
            Dict.values followUps.ncdLabs
                |> List.map .participantId

        --
        -- People for all types of follow ups.
        --
        people =
            traceReporters
                ++ peopleForPrenatalLabsResults
                ++ peopleForNCDLabsResults
                |> Pages.Utils.unique
    in
    [ FetchFollowUpParticipants people ]
