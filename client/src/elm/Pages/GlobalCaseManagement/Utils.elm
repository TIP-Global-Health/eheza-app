module Pages.GlobalCaseManagement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model
    exposing
        ( FollowUpMeasurements
        , FollowUpOption(..)
        , FollowUpValue
        , LaboratoryTest(..)
        , LabsResultsValue
        , NCDLabsResults
        , PrenatalFollowUpValue
        , PrenatalLabsResults
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Village.Model exposing (Village)
import Backend.Village.Utils exposing (isVillageResident)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.GlobalCaseManagement.Model exposing (..)
import Pages.Utils
import RemoteData exposing (RemoteData(..), WebData)


chwFilters : List CaseManagementFilter
chwFilters =
    [ FilterAcuteIllness, FilterAntenatal, FilterNutrition ]


nurseFilters : List CaseManagementFilter
nurseFilters =
    [ FilterContactsTrace, FilterPrenatalLabs, FilterNCDLabs ]


generateNutritionFollowUps : NominalDate -> FollowUpMeasurements -> Dict PersonId NutritionFollowUpItem
generateNutritionFollowUps limitDate followUps =
    let
        nutritionIndividual =
            Dict.values followUps.nutritionIndividual
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)

        nutritionGroup =
            Dict.values followUps.nutritionGroup
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)

        wellChild =
            Dict.values followUps.wellChild
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)

        generateFollowUpItems followUpsList accumDict =
            followUpsList
                |> List.foldl
                    (\candidate accum ->
                        let
                            candidateItem =
                                NutritionFollowUpItem candidate.dateMeasured "" candidate.value
                        in
                        Dict.get candidate.participantId accum
                            |> Maybe.map
                                (\memberItem ->
                                    let
                                        candidateDueDate =
                                            caclulateFollowUpDueDate candidateItem.dateMeasured candidateItem.value.options

                                        memberDueDate =
                                            caclulateFollowUpDueDate memberItem.dateMeasured memberItem.value.options
                                    in
                                    if Date.compare candidateDueDate memberDueDate == LT then
                                        Dict.insert candidate.participantId candidateItem accum

                                    else
                                        accum
                                )
                            |> Maybe.withDefault (Dict.insert candidate.participantId candidateItem accum)
                    )
                    accumDict
    in
    generateFollowUpItems nutritionIndividual Dict.empty
        |> generateFollowUpItems nutritionGroup
        |> generateFollowUpItems wellChild


generateAcuteIllnessFollowUps : NominalDate -> ModelIndexedDb -> FollowUpMeasurements -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
generateAcuteIllnessFollowUps limitDate db followUps =
    let
        encountersData =
            generateAcuteIllnessEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.acuteIllnessEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, ( encounter.participant, encounter.sequenceNumber ) ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.acuteIllness
        |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
        |> List.foldl
            (\item accum ->
                let
                    encounterData =
                        item.encounterId
                            |> Maybe.andThen
                                (\encounterId -> Dict.get encounterId encountersData)
                in
                encounterData
                    |> Maybe.map
                        (\( participantId, encounterSequenceNumber ) ->
                            let
                                personId =
                                    item.participantId

                                newItem =
                                    AcuteIllnessFollowUpItem item.dateMeasured "" item.encounterId encounterSequenceNumber item.value.options
                            in
                            Dict.get ( participantId, personId ) accum
                                |> Maybe.map
                                    (\member ->
                                        if compareAcuteIllnessFollowUpItems newItem member == GT then
                                            Dict.insert ( participantId, personId ) newItem accum

                                        else
                                            accum
                                    )
                                |> Maybe.withDefault
                                    (Dict.insert ( participantId, personId ) newItem accum)
                        )
                    |> Maybe.withDefault accum
            )
            Dict.empty


generatePrenatalFollowUps : NominalDate -> ModelIndexedDb -> FollowUpMeasurements -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
generatePrenatalFollowUps limitDate db followUps =
    let
        encountersData =
            generatePrenatalEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.prenatalEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, encounter.participant ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.prenatal
        |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
        |> List.foldl
            (\item accum ->
                let
                    encounterData =
                        item.encounterId
                            |> Maybe.andThen
                                (\encounterId -> Dict.get encounterId encountersData)
                in
                encounterData
                    |> Maybe.map
                        (\participantId ->
                            let
                                personId =
                                    item.participantId

                                newItem =
                                    PrenatalFollowUpItem item.dateMeasured "" item.encounterId item.value
                            in
                            Dict.get ( participantId, personId ) accum
                                |> Maybe.map
                                    (\member ->
                                        if Date.compare newItem.dateMeasured member.dateMeasured == GT then
                                            Dict.insert ( participantId, personId ) newItem accum

                                        else
                                            accum
                                    )
                                |> Maybe.withDefault
                                    (Dict.insert ( participantId, personId ) newItem accum)
                        )
                    |> Maybe.withDefault accum
            )
            Dict.empty


filterResolvedFollowUps : NominalDate -> Maybe NominalDate -> Bool
filterResolvedFollowUps limitDate resolutionDate =
    Maybe.map
        (\date ->
            -- Resolution date was on limit date, or before that.
            not <| Date.compare limitDate date == LT
        )
        resolutionDate
        |> -- Do not filter follow up if resolution date is not set.
           Maybe.withDefault True


fillPersonName : (k -> PersonId) -> ModelIndexedDb -> Dict k { v | personName : String } -> Dict k { v | personName : String }
fillPersonName keyToPersonIdFunc db =
    Dict.toList
        >> List.filterMap
            (\( k, v ) ->
                Dict.get (keyToPersonIdFunc k) db.people
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map (\person -> ( k, { v | personName = person.name } ))
            )
        >> Dict.fromList


generateAcuteIllnessEncounters : FollowUpMeasurements -> EverySet AcuteIllnessEncounterId
generateAcuteIllnessEncounters followUps =
    generateEncountersIdsFromMeasurements .acuteIllness followUps


generatePrenatalEncounters : FollowUpMeasurements -> EverySet PrenatalEncounterId
generatePrenatalEncounters followUps =
    generateEncountersIdsFromMeasurements .prenatal followUps


generateEncountersIdsFromMeasurements :
    (FollowUpMeasurements -> Dict measurementId { a | encounterId : Maybe encounterId })
    -> FollowUpMeasurements
    -> EverySet encounterId
generateEncountersIdsFromMeasurements getMeasurementsFunc followUps =
    getMeasurementsFunc followUps
        |> Dict.values
        |> List.filterMap .encounterId
        |> EverySet.fromList


generateAcuteIllnessParticipants : EverySet AcuteIllnessEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generateAcuteIllnessParticipants encounters db =
    generateParticipantsIdsByEncounters .acuteIllnessEncounters encounters db


generatePrenatalParticipants : EverySet PrenatalEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generatePrenatalParticipants encounters db =
    generateParticipantsIdsByEncounters .prenatalEncounters encounters db


generateParticipantsIdsByEncounters :
    (ModelIndexedDb -> Dict encounterId (WebData { a | participant : IndividualEncounterParticipantId }))
    -> EverySet encounterId
    -> ModelIndexedDb
    -> EverySet IndividualEncounterParticipantId
generateParticipantsIdsByEncounters getEncountersFunc encounters db =
    encounters
        |> EverySet.toList
        |> List.filterMap
            (\encounterId ->
                getEncountersFunc db
                    |> Dict.get encounterId
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map .participant
            )
        |> EverySet.fromList


followUpDueOptionByDate : NominalDate -> NominalDate -> EverySet FollowUpOption -> FollowUpDueOption
followUpDueOptionByDate currentDate dateMeasured options =
    let
        dueDate =
            EverySet.toList options
                |> List.head
                |> Maybe.map (calculateDueDate dateMeasured)
                |> Maybe.withDefault dateMeasured

        diff =
            diffDays currentDate dueDate
    in
    if diff < 0 then
        OverDue

    else if diff == 0 then
        DueToday

    else if diff < 7 then
        DueThisWeek

    else if diff < 31 then
        DueThisMonth

    else
        DueNextMonth


caclulateFollowUpDueDate : NominalDate -> EverySet FollowUpOption -> NominalDate
caclulateFollowUpDueDate dateMeasured options =
    EverySet.toList options
        |> List.head
        |> Maybe.map (calculateDueDate dateMeasured)
        |> Maybe.withDefault dateMeasured


calculateDueDate : NominalDate -> FollowUpOption -> NominalDate
calculateDueDate dateMeasured option =
    case option of
        OneDay ->
            Date.add Days 1 dateMeasured

        ThreeDays ->
            Date.add Days 3 dateMeasured

        OneWeek ->
            Date.add Days 7 dateMeasured

        TwoWeeks ->
            Date.add Days 14 dateMeasured

        OneMonth ->
            Date.add Months 1 dateMeasured

        TwoMonths ->
            Date.add Months 2 dateMeasured

        ThreeMonths ->
            Date.add Months 3 dateMeasured


compareAcuteIllnessFollowUpItems : AcuteIllnessFollowUpItem -> AcuteIllnessFollowUpItem -> Order
compareAcuteIllnessFollowUpItems item1 item2 =
    let
        byDate =
            Date.compare item1.dateMeasured item2.dateMeasured
    in
    if byDate == EQ then
        compare item1.encounterSequenceNumber item2.encounterSequenceNumber

    else
        byDate


prenatalLabsResultsTestData : NominalDate -> PrenatalLabsResults -> ( List LaboratoryTest, List LaboratoryTest )
prenatalLabsResultsTestData currentDate results =
    labsResultsTestData currentDate results.dateMeasured results.value


ncdLabsResultsTestData : NominalDate -> NCDLabsResults -> ( List LaboratoryTest, List LaboratoryTest )
ncdLabsResultsTestData currentDate results =
    labsResultsTestData currentDate results.dateMeasured results.value


labsResultsTestData : NominalDate -> NominalDate -> LabsResultsValue -> ( List LaboratoryTest, List LaboratoryTest )
labsResultsTestData currentDate dateMeasured value =
    if Date.compare currentDate dateMeasured == EQ then
        ( EverySet.toList value.performedTests, EverySet.toList value.completedTests )

    else
        ( EverySet.remove TestVitalsRecheck value.performedTests |> EverySet.toList
        , EverySet.remove TestVitalsRecheck value.completedTests |> EverySet.toList
        )


generateFollowUpsForResidents :
    NominalDate
    -> Village
    -> ModelIndexedDb
    -> FollowUpMeasurements
    -> ( List PersonId, List PersonId, List PersonId )
    -> FollowUpMeasurements
generateFollowUpsForResidents currentDate village db followUps ( peopleForNutrition, peopleForAccuteIllness, peopleForPrenatal ) =
    let
        residentsForNutrition =
            filterResidents db village peopleForNutrition

        residentsForAccuteIllness =
            filterResidents db village peopleForAccuteIllness

        residentsForPrenatal =
            filterResidents db village peopleForPrenatal

        nutritionGroup =
            Dict.filter
                (\_ followUp ->
                    List.member followUp.participantId residentsForNutrition
                )
                followUps.nutritionGroup

        nutritionIndividual =
            Dict.filter
                (\_ followUp ->
                    List.member followUp.participantId residentsForNutrition
                )
                followUps.nutritionIndividual

        wellChild =
            Dict.filter
                (\_ followUp ->
                    List.member followUp.participantId residentsForNutrition
                )
                followUps.wellChild

        acuteIllness =
            Dict.filter
                (\_ followUp ->
                    List.member followUp.participantId residentsForAccuteIllness
                )
                followUps.acuteIllness

        prenatal =
            Dict.filter
                (\_ followUp ->
                    List.member followUp.participantId residentsForPrenatal
                )
                followUps.prenatal
    in
    { followUps
        | nutritionGroup = nutritionGroup
        , nutritionIndividual = nutritionIndividual
        , wellChild = wellChild
        , acuteIllness = acuteIllness
        , prenatal = prenatal
    }


resolveUniquePatientsFromFollowUps : NominalDate -> FollowUpMeasurements -> ( List PersonId, List PersonId, List PersonId )
resolveUniquePatientsFromFollowUps limitDate followUps =
    let
        peopleForNutritionGroup =
            uniquePatientsFromFollowUps .nutritionGroup

        peopleForNutritionIndividual =
            uniquePatientsFromFollowUps .nutritionIndividual

        peopleForWellChild =
            uniquePatientsFromFollowUps .wellChild

        uniquePatientsFromFollowUps mappingFunc =
            mappingFunc followUps
                |> Dict.values
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
                |> List.map .participantId
                |> Pages.Utils.unique
    in
    ( peopleForNutritionGroup
        ++ peopleForNutritionIndividual
        ++ peopleForWellChild
        |> Pages.Utils.unique
    , uniquePatientsFromFollowUps .acuteIllness
    , uniquePatientsFromFollowUps .prenatal
    )


filterResidents : ModelIndexedDb -> Village -> List PersonId -> List PersonId
filterResidents db village =
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
