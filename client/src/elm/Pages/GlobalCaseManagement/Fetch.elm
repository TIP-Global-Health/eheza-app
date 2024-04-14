module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (FollowUpMeasurements)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Backend.Village.Utils exposing (resolveVillageResidents)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Utils exposing (..)
import Pages.Utils
import RemoteData


fetch : NominalDate -> HealthCenterId -> Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId mVillageId db =
    let
        fetchForAuthorityMsgs =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (\followUps ->
                        Maybe.map
                            (\villageId ->
                                fetchForCHWAtVillage currentDate villageId db followUps
                            )
                            mVillageId
                            |> Maybe.withDefault (fetchForNurseAtHealthCenter currentDate db followUps)
                    )
                |> Maybe.withDefault []
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    ]
        ++ fetchForAuthorityMsgs


fetchForCHWAtVillage : NominalDate -> VillageId -> ModelIndexedDb -> FollowUpMeasurements -> List MsgIndexedDb
fetchForCHWAtVillage currentDate villageId db allFollowUps =
    let
        followUps =
            filterFollowUpsOfResidents villageResidents allFollowUps

        villageResidents =
            resolveVillageResidents villageId db

        followUpPatients =
            resolveUniquePatientsFromFollowUps currentDate followUps

        residentsForNutrition =
            followUpPatients.nutrition

        residentsForImmunization =
            followUpPatients.immunization

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
            generateAcuteIllnessEncounters followUps

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
            generatePrenatalEncounters followUps

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
            generateTuberculosisEncounters followUps

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

        --
        --  HIV follows ups calculations.
        --
        hivEncounters =
            generateHIVEncounters followUps

        hivParticipants =
            generateHIVParticipants hivEncounters db

        fetchHIVEncountersMsg =
            EverySet.toList hivEncounters
                |> FetchHIVEncounters

        fetchHIVParticipantsMsg =
            EverySet.toList hivParticipants
                |> FetchIndividualEncounterParticipants

        fetchHIVEncountersForParticipantMsg =
            EverySet.toList hivParticipants
                |> FetchHIVEncountersForParticipants
    in
    [ FetchPeopleInVillage villageId
    , fetchAcuteIllnessEncountersMsg
    , fetchPrenatalEncountersMsg
    , fetchTuberculosisEncountersMsg
    , fetchWellChildEncountersMsg
    , fetchAcuteIllnessParticipantsMsg
    , fetchPrenatalParticipantsMsg
    , fetchTuberculosisParticipantsMsg
    , fetchHomeVisitEncountersMsg
    , fetchAcuteIllnessEncountersForParticipantMsg
    , fetchPrenatalEncountersForParticipantMsg
    , fetchTuberculosisEncountersForParticipantMsg
    , fetchHIVEncountersForParticipantMsg
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
