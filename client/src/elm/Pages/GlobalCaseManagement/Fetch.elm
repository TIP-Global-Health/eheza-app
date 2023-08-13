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
        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    ]
        ++ (Maybe.andThen
                (getVillageById db
                    >> Maybe.map
                        (\village ->
                            fetchForVillage currentDate village db followUps
                        )
                )
                villageId
                |> Maybe.withDefault (fetchForHealthCenter currentDate db followUps)
           )


fetchForVillage : NominalDate -> Village -> ModelIndexedDb -> Maybe FollowUpMeasurements -> List MsgIndexedDb
fetchForVillage currentDate village db followUps =
    let
        nutritionFollowUps =
            Maybe.map (generateNutritionFollowUps currentDate) followUps
                |> Maybe.withDefault Dict.empty

        peopleForNutrition =
            Dict.keys nutritionFollowUps
                |> Pages.Utils.unique

        peopleForAccuteIllness =
            Maybe.map
                (.acuteIllness
                    >> Dict.values
                    >> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps currentDate)
                    >> List.map .participantId
                    >> Pages.Utils.unique
                )
                followUps
                |> Maybe.withDefault []

        peopleForPrenatal =
            Maybe.map
                (.prenatal
                    >> Dict.values
                    >> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps currentDate)
                    >> List.map .participantId
                    >> Pages.Utils.unique
                )
                followUps
                |> Maybe.withDefault []

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

        residentsForAccuteIllness =
            filterResidents db village peopleForAccuteIllness

        residentsForPrenatal =
            filterResidents db village peopleForPrenatal

        followUpsForResidents =
            Maybe.map
                (\followUps_ ->
                    let
                        acuteIllnessFollowUps =
                            Dict.filter
                                (\_ followUp ->
                                    List.member followUp.participantId residentsForAccuteIllness
                                )
                                followUps_.acuteIllness

                        prenatalFollowUps =
                            Dict.filter
                                (\_ followUp ->
                                    List.member followUp.participantId residentsForPrenatal
                                )
                                followUps_.prenatal
                    in
                    { followUps_ | acuteIllness = acuteIllnessFollowUps, prenatal = prenatalFollowUps }
                )
                followUps

        --
        --  Nutrition follows ups calculations.
        --
        fetchIndividualParticipantsMsgs =
            List.map FetchIndividualEncounterParticipantsForPerson residentsForNutrition

        fetchHomeVisitEncountersMsgs =
            List.map
                (\personId ->
                    resolveIndividualParticipantsForPerson personId HomeVisitEncounter db
                        |> List.map FetchHomeVisitEncountersForParticipant
                )
                residentsForNutrition
                |> List.concat

        --
        --  Acute illness follows ups calculations.
        --
        acuteIllnessEncounters =
            Maybe.map generateAcuteIllnessEncounters followUpsForResidents
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

        --
        --  Prenatal follows ups calculations.
        --
        prenatalEncounters =
            Maybe.map generatePrenatalEncounters followUpsForResidents
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
    in
    FetchFollowUpParticipants peopleForFetch
        :: fetchIndividualParticipantsMsgs
        ++ fetchHomeVisitEncountersMsgs
        ++ fetchAcuteIllnessEncountersMsgs
        ++ fetchAcuteIllnessParticipantsMsgs
        ++ fetchAcuteIllnessEncountersForParticipantMsgs
        ++ fetchPrenatalEncountersMsgs
        ++ fetchPrenatalParticipantsMsgs
        ++ fetchPrenatalEncountersForParticipantMsgs


fetchForHealthCenter : NominalDate -> ModelIndexedDb -> Maybe FollowUpMeasurements -> List MsgIndexedDb
fetchForHealthCenter currentDate db followUps =
    let
        --
        --  Trace Contacts calculations.
        --
        traceReporters =
            Maybe.map (.traceContacts >> Dict.values >> List.map .participantId)
                followUps
                |> Maybe.withDefault []

        --
        --  Prenatal labs results calculations.
        --
        peopleForPrenatalLabsResults =
            Maybe.map (.prenatalLabs >> Dict.values >> List.map .participantId)
                followUps
                |> Maybe.withDefault []

        --
        --  NCD labs results calculations.
        --
        peopleForNCDLabsResults =
            Maybe.map (.ncdLabs >> Dict.values >> List.map .participantId)
                followUps
                |> Maybe.withDefault []

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
