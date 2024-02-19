module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Backend.Village.Model exposing (Village)
import Backend.Village.Utils exposing (getVillageById)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Utils
import RemoteData


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
        followUpPatients =
            resolveUniquePatientsFromFollowUps currentDate followUps

        -- We need to fetch all people in follow ups, to determine if person
        -- is resident of village or not.
        -- Then, we'll fetch participants and encounters data only for
        -- those that are residents.
        peopleForFetch =
            followUpPatients.nutrition
                ++ followUpPatients.acuteIllness
                ++ followUpPatients.prenatal
                ++ followUpPatients.immunization
                ++ followUpPatients.tuberculosis
                |> Pages.Utils.unique

        residentsForNutrition =
            filterResidents db village followUpPatients.nutrition

        residentsForImmunization =
            filterResidents db village followUpPatients.immunization

        followUpsForResidents =
            generateFollowUpsForResidents currentDate village db followUps followUpPatients

        fetchIndividualParticipantsMsg =
            FetchIndividualEncounterParticipantsForPeople (residentsForNutrition ++ residentsForImmunization)

        --
        --  Nutrition follows ups calculations.
        --
        fetchHomeVisitEncountersMsg =
            List.concatMap (\personId -> resolveIndividualParticipantsForPerson personId HomeVisitEncounter db)
                residentsForNutrition
                |> FetchHomeVisitEncountersForParticipants

        --
        --  Immunization follows ups calculations.
        --
        fetchWellChildEncountersMsg =
            List.concatMap (\personId -> resolveIndividualParticipantsForPerson personId WellChildEncounter db)
                residentsForImmunization
                |> FetchWellChildEncountersForParticipants

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

        --
        --  Tuberculosis follows ups calculations.
        --
        tuberculosisEncounters =
            generateTuberculosisEncounters followUpsForResidents

        tuberculosisParticipants =
            generateTuberculosisParticipants tuberculosisEncounters db

        fetchTuberculosisEncountersMsg =
            EverySet.toList tuberculosisEncounters
                |> FetchTuberculosisEncounters

        fetchTuberculosisParticipantsMsg =
            EverySet.toList tuberculosisParticipants
                |> FetchIndividualEncounterParticipants

        fetchTuberculosisEncountersForParticipantMsg =
            EverySet.toList tuberculosisParticipants
                |> FetchTuberculosisEncountersForParticipants
    in
    [ FetchFollowUpParticipants peopleForFetch
    , fetchAcuteIllnessEncountersMsg
    , fetchPrenatalEncountersMsg
    , fetchTuberculosisEncountersMsg
    , fetchAcuteIllnessParticipantsMsg
    , fetchPrenatalParticipantsMsg
    , fetchTuberculosisParticipantsMsg
    , fetchHomeVisitEncountersMsg
    , fetchAcuteIllnessEncountersForParticipantMsg
    , fetchPrenatalEncountersForParticipantMsg
    , fetchTuberculosisEncountersForParticipantMsg
    , fetchIndividualParticipantsMsg
    , fetchWellChildEncountersMsg
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
