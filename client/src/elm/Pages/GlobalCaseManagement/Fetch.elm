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
import Restful.Endpoint exposing (fromEntityUuid)


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
            peopleFromPositiveResultHIVFollowUps
                ++ residentsForNutrition
                ++ residentsForImmunization
                |> Pages.Utils.unique
                |> FetchIndividualEncounterParticipantsForPeople

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
        -- There are regular HIV follow ups, and 'dummy' HIV
        -- follow ups created for positive HIV test results.
        ( positiveResultHIVFollowUps, hivFollowUps ) =
            Dict.values followUps.hiv
                |> List.partition
                    (\followUp ->
                        Maybe.map
                            (\encounterId ->
                                fromEntityUuid encounterId == "dummy"
                            )
                            followUp.encounterId
                            |> Maybe.withDefault False
                    )

        hivEncounters =
            List.filterMap .encounterId hivFollowUps
                |> EverySet.fromList

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

        -- Used to fetch HIV patrticipants for patients (above),
        -- and their HIV encounters.
        -- We need the HIV encounters to determine if to show follow up
        -- entry or not (positive result entry is shown unless there was
        -- HIV encounter after result was recorded).
        peopleFromPositiveResultHIVFollowUps =
            List.map .participantId positiveResultHIVFollowUps

        fetchHIVEncountersForPositiveResultFollowUpsParticipantMsg =
            List.concatMap
                (\personId ->
                    resolveIndividualParticipantsForPerson personId HIVEncounter db
                )
                peopleFromPositiveResultHIVFollowUps
                |> FetchHIVEncountersForParticipants
    in
    [ FetchPeopleInVillage villageId
    , fetchAcuteIllnessEncountersMsg
    , fetchPrenatalEncountersMsg
    , fetchTuberculosisEncountersMsg
    , fetchWellChildEncountersMsg
    , fetchHIVEncountersMsg
    , fetchAcuteIllnessParticipantsMsg
    , fetchPrenatalParticipantsMsg
    , fetchTuberculosisParticipantsMsg
    , fetchHomeVisitEncountersMsg
    , fetchHIVParticipantsMsg
    , fetchAcuteIllnessEncountersForParticipantMsg
    , fetchPrenatalEncountersForParticipantMsg
    , fetchTuberculosisEncountersForParticipantMsg
    , fetchHIVEncountersForParticipantMsg
    , fetchIndividualParticipantsMsg
    , fetchHIVEncountersForPositiveResultFollowUpsParticipantMsg
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

        -- Fetching data of Prenatal encounters (where diagnoses
        -- are stored), since we need to know if entry action
        -- should forward to next steps right away, as there's
        -- an urgent diagnosis to handle.
        fetchPrenatalEncountersMsg =
            Dict.values followUps.prenatalLabs
                |> List.filterMap .encounterId
                |> FetchPrenatalEncounters
    in
    [ FetchFollowUpParticipants people
    , fetchPrenatalEncountersMsg
    ]
