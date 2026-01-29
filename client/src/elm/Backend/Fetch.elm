module Backend.Fetch exposing (forget, shouldFetch)

import AssocList as Dict
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import LocalData exposing (isNotNeeded)
import RemoteData exposing (RemoteData(..), isNotAsked, isSuccess)
import Time


{-| Given a `MsgIndexedDb`, do we need to fetch the data it would fetch?
We only answer `True` if the data is `NotAsked`. So, we don't automatically
re-fetch errors.

Note that the data need not literally be a `RemoteData`, but that will be
common. The answer does need to flip to `False` when a request is in progress,
or we will enter an infinite loop.

-}
shouldFetch : Time.Posix -> ModelIndexedDb -> MsgIndexedDb -> Bool
shouldFetch currentTime model msg =
    let
        hasNoSuccessValues dict =
            Dict.values dict
                |> List.filter (\v -> RemoteData.isLoading v || RemoteData.isNotAsked v)
                |> List.isEmpty
                |> not
    in
    case msg of
        FetchChildMeasurements childId ->
            Dict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchComputedDashboard healthCenterId ->
            -- Do not fetch, if last attempt was less than 20 seconds ago.
            if Time.posixToMillis currentTime - Time.posixToMillis model.computedDashboardLastFetched < 20000 then
                False

            else
                Dict.member healthCenterId model.computedDashboards
                    |> not

        FetchChildrenMeasurements ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.childMeasurements

        FetchClinics ->
            isNotAsked model.clinics

        FetchEditableSession id _ ->
            -- This one is a bit special because it is synthetic ...  what
            -- we're asking for here is not the fetch itself, but a certain
            -- organization of the fetched data. We want to re-run the
            -- organization in every case unless we have a success here.
            -- Which means, once we have a success, it's important to
            -- invalidate or modify our successful data if underlying data
            -- changes. (That is, our `handleRevisions` needs to keep the
            -- editable sessions in mind).
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> isSuccess
                |> not

        FetchEditableSessionCheckedIn id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.checkedIn >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEditableSessionMeasurements id ->
            let
                -- Make sure we don't still have measurements being lazy loaded. If we do, allow rebuilding the
                -- `EditableSession`.
                hasMothersMeasurementsNotSuccess =
                    hasNoSuccessValues model.motherMeasurements

                hasChildrenMeasurementsNotSuccess =
                    hasNoSuccessValues model.childMeasurements

                measurementsNotSuccess =
                    Dict.get id model.editableSessions
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map (.offlineSession >> .measurements >> isNotNeeded)
                        |> RemoteData.withDefault False
            in
            measurementsNotSuccess
                || hasMothersMeasurementsNotSuccess
                || hasChildrenMeasurementsNotSuccess

        FetchEditableSessionSummaryByActivity id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.summaryByActivity >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEditableSessionSummaryByParticipant id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.summaryByParticipant >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEveryCounselingSchedule ->
            isNotAsked model.everyCounselingSchedule

        FetchExpectedParticipants sessionId ->
            Dict.get sessionId model.expectedParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchExpectedSessions childId ->
            Dict.get childId model.expectedSessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHealthCenters ->
            isNotAsked model.healthCenters

        FetchVillages ->
            isNotAsked model.villages

        FetchMotherMeasurements motherId ->
            Dict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchMothersMeasurements ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.motherMeasurements

        FetchParticipantForms ->
            isNotAsked model.participantForms

        FetchPeopleByName search ->
            Dict.get (String.trim search) model.personSearchesByName
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPeopleByNationalId search ->
            Dict.get (String.trim search) model.personSearchesByNationalId
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPeopleInVillage id ->
            Dict.get id model.peopleInVillage
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPerson id ->
            Dict.get id model.people
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPeople ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.people

        FetchParticipantsForPerson id ->
            Dict.get id model.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncounter id ->
            Dict.get id model.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncounters ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.prenatalEncounters)) ids

        FetchPrenatalEncountersForParticipant id ->
            Dict.get id model.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.prenatalEncountersByParticipant)) ids

        FetchPrenatalMeasurements id ->
            Dict.get id model.prenatalMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNutritionEncounter id ->
            Dict.get id model.nutritionEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNutritionEncountersForParticipant id ->
            Dict.get id model.nutritionEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNutritionMeasurements id ->
            Dict.get id model.nutritionMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchAcuteIllnessEncounter id ->
            Dict.get id model.acuteIllnessEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchAcuteIllnessEncounters ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.acuteIllnessEncounters)) ids

        FetchAcuteIllnessEncountersForParticipant id ->
            Dict.get id model.acuteIllnessEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchAcuteIllnessEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.acuteIllnessEncountersByParticipant)) ids

        FetchAcuteIllnessMeasurements id ->
            Dict.get id model.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchFollowUpMeasurements id ->
            Dict.get id model.followUpMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchFollowUpParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.people)) ids

        FetchHomeVisitEncounter id ->
            Dict.get id model.homeVisitEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHomeVisitEncountersForParticipant id ->
            Dict.get id model.homeVisitEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHomeVisitEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.homeVisitEncountersByParticipant)) ids

        FetchHomeVisitMeasurements id ->
            Dict.get id model.homeVisitMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchWellChildEncounter id ->
            Dict.get id model.wellChildEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchWellChildEncountersForParticipant id ->
            Dict.get id model.wellChildEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchWellChildEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.wellChildEncountersByParticipant)) ids

        FetchWellChildMeasurements id ->
            Dict.get id model.wellChildMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNCDEncounter id ->
            Dict.get id model.ncdEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNCDEncountersForParticipant id ->
            Dict.get id model.ncdEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchNCDEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.ncdEncountersByParticipant)) ids

        FetchNCDMeasurements id ->
            Dict.get id model.ncdMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildScoreboardEncounter id ->
            Dict.get id model.childScoreboardEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildScoreboardEncountersForParticipant id ->
            Dict.get id model.childScoreboardEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildScoreboardMeasurements id ->
            Dict.get id model.childScoreboardMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchTuberculosisEncounter id ->
            Dict.get id model.tuberculosisEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchTuberculosisEncounters ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.tuberculosisEncounters)) ids

        FetchTuberculosisEncountersForParticipant id ->
            Dict.get id model.tuberculosisEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchTuberculosisEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.tuberculosisEncountersByParticipant)) ids

        FetchTuberculosisMeasurements id ->
            Dict.get id model.tuberculosisMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHIVEncounter id ->
            Dict.get id model.hivEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHIVEncounters ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.hivEncounters)) ids

        FetchHIVEncountersForParticipant id ->
            Dict.get id model.hivEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHIVEncountersForParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.hivEncountersByParticipant)) ids

        FetchHIVMeasurements id ->
            Dict.get id model.hivMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchEducationSession id ->
            Dict.get id model.educationSessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchEducationSessionsForPerson id ->
            Dict.get id model.educationSessionsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchStockManagementMeasurements id ->
            Dict.get id model.stockManagementMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchStockManagementData id ->
            Dict.get id model.stockManagementData
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchIndividualEncounterParticipant id ->
            Dict.get id model.individualParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchIndividualEncounterParticipants ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.individualParticipants)) ids

        FetchIndividualEncounterParticipantsForPerson id ->
            Dict.get id model.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchIndividualEncounterParticipantsForPeople ids ->
            if List.isEmpty ids then
                False

            else
                List.any (\id -> not (Dict.member id model.individualParticipantsByPerson)) ids

        FetchRelationshipsForPerson id ->
            Dict.get id model.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSession sessionId ->
            Dict.get sessionId model.sessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSessionsByClinic clinicId ->
            Dict.get clinicId model.sessionsByClinic
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchTraceContact id ->
            Dict.get id model.traceContacts
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPregnancyByNewborn id ->
            Dict.get id model.pregnancyByNewborn
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchResilienceSurveysForNurse id ->
            Dict.get id model.resilienceSurveysByNurse
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        -- For other messages, we answer false.
        _ ->
            False


{-| Given a Msg that would fetch some data, forget that data.
-}
forget : MsgIndexedDb -> ModelIndexedDb -> ModelIndexedDb
forget msg model =
    case msg of
        FetchChildMeasurements childId ->
            { model | childMeasurements = Dict.remove childId model.childMeasurements }

        FetchClinics ->
            { model | clinics = NotAsked }

        FetchEditableSession id _ ->
            { model | editableSessions = Dict.remove id model.editableSessions }

        FetchEveryCounselingSchedule ->
            { model | everyCounselingSchedule = NotAsked }

        FetchExpectedParticipants sessionId ->
            { model | expectedParticipants = Dict.remove sessionId model.expectedParticipants }

        FetchExpectedSessions childId ->
            { model | expectedSessions = Dict.remove childId model.expectedSessions }

        FetchHealthCenters ->
            { model | healthCenters = NotAsked }

        FetchVillages ->
            { model | villages = NotAsked }

        FetchMotherMeasurements motherId ->
            { model | motherMeasurements = Dict.remove motherId model.motherMeasurements }

        FetchParticipantForms ->
            { model | participantForms = NotAsked }

        FetchPeopleByName search ->
            { model | personSearchesByName = Dict.remove (String.trim search) model.personSearchesByName }

        FetchPeopleByNationalId search ->
            { model | personSearchesByNationalId = Dict.remove (String.trim search) model.personSearchesByNationalId }

        FetchPeopleInVillage id ->
            { model | peopleInVillage = Dict.remove id model.peopleInVillage }

        FetchPerson id ->
            { model | people = Dict.remove id model.people }

        FetchTraceContact id ->
            { model | traceContacts = Dict.remove id model.traceContacts }

        FetchPregnancyByNewborn id ->
            { model | pregnancyByNewborn = Dict.remove id model.pregnancyByNewborn }

        FetchIndividualEncounterParticipantsForPerson id ->
            { model | individualParticipantsByPerson = Dict.remove id model.individualParticipantsByPerson }

        FetchParticipantsForPerson id ->
            { model | participantsByPerson = Dict.remove id model.participantsByPerson }

        FetchPrenatalEncounter id ->
            { model | prenatalEncounters = Dict.remove id model.prenatalEncounters }

        FetchPrenatalEncountersForParticipant id ->
            { model | prenatalEncountersByParticipant = Dict.remove id model.prenatalEncountersByParticipant }

        FetchPrenatalMeasurements id ->
            { model | prenatalMeasurements = Dict.remove id model.prenatalMeasurements }

        FetchNutritionEncounter id ->
            { model | nutritionEncounters = Dict.remove id model.nutritionEncounters }

        FetchNutritionEncountersForParticipant id ->
            { model | nutritionEncountersByParticipant = Dict.remove id model.nutritionEncountersByParticipant }

        FetchNutritionMeasurements id ->
            { model | nutritionMeasurements = Dict.remove id model.nutritionMeasurements }

        FetchAcuteIllnessEncounter id ->
            { model | acuteIllnessEncounters = Dict.remove id model.acuteIllnessEncounters }

        FetchAcuteIllnessEncountersForParticipant id ->
            { model | acuteIllnessEncountersByParticipant = Dict.remove id model.acuteIllnessEncountersByParticipant }

        FetchAcuteIllnessMeasurements id ->
            { model | acuteIllnessMeasurements = Dict.remove id model.acuteIllnessMeasurements }

        FetchFollowUpMeasurements id ->
            { model | followUpMeasurements = Dict.remove id model.followUpMeasurements }

        FetchHomeVisitEncounter id ->
            { model | homeVisitEncounters = Dict.remove id model.homeVisitEncounters }

        FetchHomeVisitEncountersForParticipant id ->
            { model | homeVisitEncountersByParticipant = Dict.remove id model.homeVisitEncountersByParticipant }

        FetchHomeVisitMeasurements id ->
            { model | homeVisitMeasurements = Dict.remove id model.homeVisitMeasurements }

        FetchWellChildEncounter id ->
            { model | wellChildEncounters = Dict.remove id model.wellChildEncounters }

        FetchWellChildEncountersForParticipant id ->
            { model | wellChildEncountersByParticipant = Dict.remove id model.wellChildEncountersByParticipant }

        FetchWellChildMeasurements id ->
            { model | wellChildMeasurements = Dict.remove id model.wellChildMeasurements }

        FetchNCDEncounter id ->
            { model | ncdEncounters = Dict.remove id model.ncdEncounters }

        FetchNCDEncountersForParticipant id ->
            { model | ncdEncountersByParticipant = Dict.remove id model.ncdEncountersByParticipant }

        FetchNCDMeasurements id ->
            { model | ncdMeasurements = Dict.remove id model.ncdMeasurements }

        FetchChildScoreboardEncounter id ->
            { model | childScoreboardEncounters = Dict.remove id model.childScoreboardEncounters }

        FetchChildScoreboardEncountersForParticipant id ->
            { model | childScoreboardEncountersByParticipant = Dict.remove id model.childScoreboardEncountersByParticipant }

        FetchChildScoreboardMeasurements id ->
            { model | childScoreboardMeasurements = Dict.remove id model.childScoreboardMeasurements }

        FetchTuberculosisEncounter id ->
            { model | tuberculosisEncounters = Dict.remove id model.tuberculosisEncounters }

        FetchTuberculosisEncountersForParticipant id ->
            { model | tuberculosisEncountersByParticipant = Dict.remove id model.tuberculosisEncountersByParticipant }

        FetchTuberculosisMeasurements id ->
            { model | tuberculosisMeasurements = Dict.remove id model.tuberculosisMeasurements }

        FetchHIVEncounter id ->
            { model | hivEncounters = Dict.remove id model.hivEncounters }

        FetchHIVEncountersForParticipant id ->
            { model | hivEncountersByParticipant = Dict.remove id model.hivEncountersByParticipant }

        FetchHIVMeasurements id ->
            { model | hivMeasurements = Dict.remove id model.hivMeasurements }

        FetchEducationSession id ->
            { model | educationSessions = Dict.remove id model.educationSessions }

        FetchEducationSessionsForPerson id ->
            { model | educationSessionsByPerson = Dict.remove id model.educationSessionsByPerson }

        FetchStockManagementMeasurements id ->
            { model | stockManagementMeasurements = Dict.remove id model.stockManagementMeasurements }

        FetchStockManagementData id ->
            { model | stockManagementData = Dict.remove id model.stockManagementData }

        FetchIndividualEncounterParticipant id ->
            { model | individualParticipants = Dict.remove id model.individualParticipants }

        FetchRelationshipsForPerson id ->
            { model | relationshipsByPerson = Dict.remove id model.relationshipsByPerson }

        FetchSession sessionId ->
            { model | sessions = Dict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = Dict.remove clinicId model.sessionsByClinic }

        FetchResilienceSurveysForNurse id ->
            { model | resilienceSurveysByNurse = Dict.remove id model.resilienceSurveysByNurse }

        -- For other messages, we do nothing.
        _ ->
            model
