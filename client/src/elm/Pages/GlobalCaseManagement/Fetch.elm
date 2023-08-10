module Pages.GlobalCaseManagement.Fetch exposing (fetch)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Backend.Village.Utils exposing (getVillageById, isVillageResident)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (..)
import RemoteData exposing (RemoteData(..))


fetch : NominalDate -> HealthCenterId -> Maybe VillageId -> ModelIndexedDb -> List MsgIndexedDb
fetch currentDate healthCenterId villageId db =
    let
        followUps =
            Dict.get healthCenterId db.followUpMeasurements
                |> Maybe.andThen RemoteData.toMaybe

        nutritionFollowUps =
            Maybe.map (generateNutritionFollowUps currentDate) followUps
                |> Maybe.withDefault Dict.empty

        peopleForNutrition =
            Dict.keys nutritionFollowUps

        acuteIllnessFollowUps =
            Maybe.map (generateAcuteIllnessFollowUps currentDate db) followUps
                |> Maybe.withDefault Dict.empty

        peopleForAccuteIllness =
            Dict.keys acuteIllnessFollowUps
                |> List.map Tuple.second

        prenatalFollowUps =
            Maybe.map (generatePrenatalFollowUps currentDate db) followUps
                |> Maybe.withDefault Dict.empty

        peopleForPrenatal =
            Dict.keys prenatalFollowUps
                |> List.map Tuple.second

        people =
            peopleForNutrition
                ++ peopleForAccuteIllness
                ++ peopleForPrenatal
                |> EverySet.fromList
                |> EverySet.toList
    in
    [ FetchVillages
    , FetchHealthCenters
    , FetchFollowUpMeasurements healthCenterId
    , FetchFollowUpParticipants people
    ]
        ++ (Maybe.andThen
                (getVillageById db
                    >> Maybe.map
                        (\village ->
                            fetchForVillage currentDate village db followUps peopleForNutrition peopleForAccuteIllness peopleForPrenatal
                        )
                )
                villageId
                |> Maybe.withDefault (fetchForHealthCenter currentDate db followUps peopleForNutrition peopleForAccuteIllness peopleForPrenatal)
           )


fetchForVillage currentDate village db followUps peopleForNutrition peopleForAccuteIllness peopleForPrenatal =
    let
        _ =
            Debug.log "peopleForNutrition" (List.length peopleForNutrition)

        _ =
            Debug.log "residentsForNutrition" (List.length residentsForNutrition)

        _ =
            Debug.log "peopleForAccuteIllness" (List.length peopleForAccuteIllness)

        _ =
            Debug.log "residentsForAccuteIllness" (List.length residentsForAccuteIllness)

        _ =
            Debug.log "peopleForPrenatal" (List.length peopleForPrenatal)

        _ =
            Debug.log "residentsForPrenatal" (List.length residentsForPrenatal)

        residentsForNutrition =
            filterResidents peopleForNutrition

        residentsForAccuteIllness =
            filterResidents peopleForAccuteIllness

        residentsForPrenatal =
            filterResidents peopleForPrenatal

        filterResidents =
            List.filter
                (\personId ->
                    Dict.get personId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (\person ->
                                isVillageResident person village
                            )
                        |> Maybe.withDefault False
                )

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
    fetchIndividualParticipantsMsgs
        ++ fetchHomeVisitEncountersMsgs
        ++ fetchAcuteIllnessEncountersMsgs
        ++ fetchAcuteIllnessParticipantsMsgs
        ++ fetchAcuteIllnessEncountersForParticipantMsgs
        ++ fetchPrenatalEncountersMsgs
        ++ fetchPrenatalParticipantsMsgs
        ++ fetchPrenatalEncountersForParticipantMsgs


fetchForHealthCenter currentDate db followUps peopleForNutrition peopleForAccuteIllness peopleForPrenatal =
    let
        --
        --  Nutrition follows ups calculations.
        --
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
                |> EverySet.fromList
                |> EverySet.toList
    in
    FetchFollowUpParticipants people
        :: fetchIndividualParticipantsMsgs
        ++ fetchHomeVisitEncountersMsgs
        ++ fetchAcuteIllnessEncountersMsgs
        ++ fetchAcuteIllnessParticipantsMsgs
        ++ fetchAcuteIllnessEncountersForParticipantMsgs
        ++ fetchPrenatalEncountersMsgs
        ++ fetchPrenatalParticipantsMsgs
        ++ fetchPrenatalEncountersForParticipantMsgs
