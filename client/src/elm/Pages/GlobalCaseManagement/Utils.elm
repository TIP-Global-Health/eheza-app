module Pages.GlobalCaseManagement.Utils exposing (caclulateFollowUpDueDate, calculateDueDate, chwFilters, compareAcuteIllnessFollowUpItems, fillPersonName, filterByParticipant, filterDictFollowUpsSetToNotNeeded, filterFollowUpsOfResidents, filterListFollowUpsSetToNotNeeded, filterResolvedFollowUps, followUpDueOptionByDate, generateAcuteIllnessEncounters, generateAcuteIllnessFollowUps, generateAcuteIllnessParticipants, generateEncountersIdsFromMeasurements, generateHIVEncounters, generateHIVFollowUps, generateHIVParticipants, generateImmunizationFollowUps, generateNutritionFollowUps, generateParticipantsIdsByEncounters, generatePrenatalEncounters, generatePrenatalFollowUps, generatePrenatalParticipants, generateTuberculosisEncounters, generateTuberculosisFollowUps, generateTuberculosisParticipants, labTechFilters, labsResultsTestData, nurseFilters, resolveUniquePatientsFromFollowUps)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( FollowUpMeasurements
        , FollowUpOption(..)
        , FollowUpValue
        , LaboratoryTest(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Utils exposing (hivManagementEnabled, tuberculosisManagementEnabled)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Pages.GlobalCaseManagement.Model exposing (AcuteIllnessFollowUpItem, CaseManagementFilter(..), FollowUpDueOption(..), FollowUpPatients, HIVFollowUpItem, ImmunizationFollowUpItem, NutritionFollowUpItem, PrenatalFollowUpItem, TuberculosisFollowUpItem)
import Pages.Utils
import RemoteData exposing (WebData)
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model exposing (SiteFeature)


chwFilters : EverySet SiteFeature -> List CaseManagementFilter
chwFilters features =
    [ FilterAcuteIllness
    , FilterAntenatal
    , FilterNutrition
    , FilterImmunization
    ]
        ++ (if tuberculosisManagementEnabled features then
                [ FilterTuberculosis ]

            else
                []
           )
        ++ (if hivManagementEnabled features then
                [ FilterHIV ]

            else
                []
           )


nurseFilters : List CaseManagementFilter
nurseFilters =
    [ FilterContactsTrace, FilterPrenatalLabs, FilterNCDLabs ]


labTechFilters : List CaseManagementFilter
labTechFilters =
    [ FilterPrenatalLabs ]


generateNutritionFollowUps : NominalDate -> FollowUpMeasurements -> Dict PersonId NutritionFollowUpItem
generateNutritionFollowUps limitDate followUps =
    let
        nutritionIndividual =
            Dict.values followUps.nutritionIndividual
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
                |> filterListFollowUpsSetToNotNeeded

        nutritionGroup =
            Dict.values followUps.nutritionGroup
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
                |> filterListFollowUpsSetToNotNeeded

        wellChild =
            Dict.values followUps.wellChild
                |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
                |> filterListFollowUpsSetToNotNeeded

        generateFollowUpItems followUpsList accumDict =
            List.foldl
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
                followUpsList
    in
    generateFollowUpItems nutritionIndividual Dict.empty
        |> generateFollowUpItems nutritionGroup
        |> generateFollowUpItems wellChild


generateAcuteIllnessFollowUps :
    NominalDate
    -> ModelIndexedDb
    -> FollowUpMeasurements
    -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
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
                                    AcuteIllnessFollowUpItem item.dateMeasured "" item.encounterId encounterSequenceNumber item.value
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
        |> filterDictFollowUpsSetToNotNeeded


generatePrenatalFollowUps :
    NominalDate
    -> ModelIndexedDb
    -> FollowUpMeasurements
    -> Dict ( IndividualEncounterParticipantId, PersonId ) PrenatalFollowUpItem
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
        |> filterDictFollowUpsSetToNotNeeded


generateImmunizationFollowUps : NominalDate -> FollowUpMeasurements -> Dict PersonId ImmunizationFollowUpItem
generateImmunizationFollowUps limitDate followUps =
    Dict.values followUps.nextVisit
        |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
        |> List.filterMap
            (\followUp ->
                Maybe.andThen
                    (\dueDate ->
                        if not <| Date.compare limitDate dueDate == LT then
                            Just ( followUp.dateMeasured, followUp.participantId, dueDate )

                        else
                            Nothing
                    )
                    followUp.value.asapImmunisationDate
            )
        |> List.foldl
            (\( dateMeasured, participantId, dueDate ) accum ->
                let
                    candidateItem =
                        ImmunizationFollowUpItem dateMeasured dueDate ""
                in
                Dict.get participantId accum
                    |> Maybe.map
                        (\memberItem ->
                            if Date.compare candidateItem.dateMeasured memberItem.dateMeasured == GT then
                                Dict.insert participantId candidateItem accum

                            else
                                accum
                        )
                    |> Maybe.withDefault (Dict.insert participantId candidateItem accum)
            )
            Dict.empty


{-| We have 2 sources for case management entries of Tuberculosis pane.
One is Acute illness follow up, in case Tuberculosis suspect is diagnosed.
Other is the follow ups from Tuberculosis management encounter.
So, in order to combine these 2, we'll have to include AI follows for person,
as part of Tuberculosis follow ups.
Currently, all Tuberculosis encounters are derived from single participant (as
there's no option to 'end' Tuberculosis illness), so this solution is valid.
In case we'll have multiple Tuberculosis illnesses in future, this wil need to
be revised.
There's also an option that patient did not attend Tuberculosis encounter, and
there's no participant. This is why we return second dictionary derived from
AI follow ups. It's enrty will have a different action of starting first
Tuberculosis encounter, which will also create the participant.
-}
generateTuberculosisFollowUps :
    NominalDate
    -> ModelIndexedDb
    -> FollowUpMeasurements
    -> Dict ( IndividualEncounterParticipantId, PersonId ) AcuteIllnessFollowUpItem
    ->
        ( Dict ( IndividualEncounterParticipantId, PersonId ) TuberculosisFollowUpItem
        , Dict PersonId TuberculosisFollowUpItem
        )
generateTuberculosisFollowUps limitDate db followUps followUpsFromAcuteIllness =
    let
        -- As theoretically, there can be multiple illnesses where
        -- Tuberculosis suspect is diagnosed, we resolve the most recent
        -- follow up per patient.
        acuteIllnessItemsByPerson =
            -- Filter out resolved follow ups.
            Dict.filter
                (\_ item ->
                    item.value |> .resolutionDate |> filterResolvedFollowUps limitDate
                )
                followUpsFromAcuteIllness
                -- Genrated dict with most recent follow up per patient.
                |> Dict.foldl
                    (\( _, personId ) item accum ->
                        Dict.get personId accum
                            |> Maybe.map
                                (\current ->
                                    if Date.compare current.dateMeasured item.dateMeasured == LT then
                                        Dict.insert personId item accum

                                    else
                                        accum
                                )
                            |> Maybe.withDefault (Dict.insert personId item accum)
                    )
                    Dict.empty
                -- Translate Acute Illness follow ups items into
                -- Tuberculosis follow ups items, so we can merge them with
                -- 'generic' Tuberculosis follow ups items.
                |> Dict.map
                    (\_ item ->
                        TuberculosisFollowUpItem item.dateMeasured
                            item.personName
                            Nothing
                            (FollowUpValue item.value.options item.value.resolutionDate)
                    )

        encountersData =
            generateTuberculosisEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.tuberculosisEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, encounter.participant ))
                    )
                |> Dict.fromList

        itemsFromTuberculosis =
            Dict.values followUps.tuberculosis
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
                                            TuberculosisFollowUpItem item.dateMeasured "" item.encounterId item.value
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
    in
    Dict.foldl
        (\( participantId, personId ) item ( accum, acuteIllnessDict ) ->
            Dict.get personId acuteIllnessDict
                |> Maybe.map
                    (\itemFromAcuteIllness ->
                        ( -- In case acute illness item is more recent that the one we have
                          -- from Tuberculosis encounter, replace it.
                          if Date.compare item.dateMeasured itemFromAcuteIllness.dateMeasured == LT then
                            Dict.insert ( participantId, personId )
                                -- When replacing, we assign encounter ID, as acute illness
                                -- does not have it set.
                                { itemFromAcuteIllness | encounterId = item.encounterId }
                                accum

                          else
                            accum
                        , -- Item for person was found in 'generic' Tuberculosis dict,
                          -- No matter if it's used or dropped, we remove it from Acute Illness dict,
                          Dict.remove personId acuteIllnessDict
                        )
                    )
                |> Maybe.withDefault
                    ( Dict.insert ( participantId, personId ) item accum
                    , acuteIllnessDict
                    )
        )
        ( Dict.empty, acuteIllnessItemsByPerson )
        itemsFromTuberculosis
        |> Tuple.mapBoth filterDictFollowUpsSetToNotNeeded filterDictFollowUpsSetToNotNeeded


generateHIVFollowUps :
    NominalDate
    -> ModelIndexedDb
    -> FollowUpMeasurements
    -> Dict ( Maybe IndividualEncounterParticipantId, PersonId ) HIVFollowUpItem
generateHIVFollowUps limitDate db followUps =
    let
        encountersData =
            generateHIVEncounters followUps
                |> EverySet.toList
                |> List.filterMap
                    (\encounterId ->
                        Dict.get encounterId db.hivEncounters
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map (\encounter -> ( encounterId, encounter.participant ))
                    )
                |> Dict.fromList
    in
    Dict.values followUps.hiv
        |> List.filter (.value >> .resolutionDate >> filterResolvedFollowUps limitDate)
        |> List.foldl
            (\item accum ->
                let
                    personId =
                        item.participantId

                    encounterIdAsString =
                        Maybe.map fromEntityUuid item.encounterId
                in
                if encounterIdAsString == Just "dummy" then
                    let
                        newItem =
                            HIVFollowUpItem item.dateMeasured "" Nothing item.value
                    in
                    Dict.insert ( Nothing, personId ) newItem accum

                else
                    let
                        encounterData =
                            Maybe.andThen
                                (\encounterId -> Dict.get encounterId encountersData)
                                item.encounterId
                    in
                    encounterData
                        |> Maybe.map
                            (\participantId ->
                                let
                                    newItem =
                                        HIVFollowUpItem item.dateMeasured "" item.encounterId item.value
                                in
                                Dict.get ( Just participantId, personId ) accum
                                    |> Maybe.map
                                        (\member ->
                                            if Date.compare newItem.dateMeasured member.dateMeasured == GT then
                                                Dict.insert ( Just participantId, personId ) newItem accum

                                            else
                                                accum
                                        )
                                    |> Maybe.withDefault
                                        (Dict.insert ( Just participantId, personId ) newItem accum)
                            )
                        |> Maybe.withDefault accum
            )
            Dict.empty
        |> filterDictFollowUpsSetToNotNeeded


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
        >> List.sortBy (Tuple.second >> .personName)
        >> Dict.fromList


generateAcuteIllnessEncounters : FollowUpMeasurements -> EverySet AcuteIllnessEncounterId
generateAcuteIllnessEncounters followUps =
    generateEncountersIdsFromMeasurements .acuteIllness followUps


generatePrenatalEncounters : FollowUpMeasurements -> EverySet PrenatalEncounterId
generatePrenatalEncounters followUps =
    generateEncountersIdsFromMeasurements .prenatal followUps


generateTuberculosisEncounters : FollowUpMeasurements -> EverySet TuberculosisEncounterId
generateTuberculosisEncounters followUps =
    generateEncountersIdsFromMeasurements .tuberculosis followUps


generateHIVEncounters : FollowUpMeasurements -> EverySet HIVEncounterId
generateHIVEncounters followUps =
    generateEncountersIdsFromMeasurements .hiv followUps


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


generateTuberculosisParticipants : EverySet TuberculosisEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generateTuberculosisParticipants encounters db =
    generateParticipantsIdsByEncounters .tuberculosisEncounters encounters db


generateHIVParticipants : EverySet HIVEncounterId -> ModelIndexedDb -> EverySet IndividualEncounterParticipantId
generateHIVParticipants encounters db =
    generateParticipantsIdsByEncounters .hivEncounters encounters db


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

        -- When this option is selected, follow up not required
        -- so we set due date far into future.
        FollowUpNotNeeded ->
            Date.add Years 20 dateMeasured


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


labsResultsTestData :
    NominalDate
    ->
        { r
            | dateMeasured : NominalDate
            , value : { v | performedTests : EverySet LaboratoryTest, completedTests : EverySet LaboratoryTest }
        }
    -> ( EverySet LaboratoryTest, EverySet LaboratoryTest )
labsResultsTestData currentDate results =
    if Date.compare currentDate results.dateMeasured == EQ then
        ( results.value.performedTests, results.value.completedTests )

    else
        -- Vitals recheck needs to happen on same day it was scheduled.
        -- If it's not the case, we remove the test from the list.
        ( EverySet.remove TestVitalsRecheck results.value.performedTests
        , EverySet.remove TestVitalsRecheck results.value.completedTests
        )


resolveUniquePatientsFromFollowUps : NominalDate -> FollowUpMeasurements -> FollowUpPatients
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
    { nutrition =
        peopleForNutritionGroup
            ++ peopleForNutritionIndividual
            ++ peopleForWellChild
            |> Pages.Utils.unique
    , immunization = uniquePatientsFromFollowUps .nextVisit
    }


filterFollowUpsOfResidents : List PersonId -> FollowUpMeasurements -> FollowUpMeasurements
filterFollowUpsOfResidents residents followUps =
    { followUps
        | nutritionGroup = filterByParticipant residents followUps.nutritionGroup
        , nutritionIndividual = filterByParticipant residents followUps.nutritionIndividual
        , wellChild = filterByParticipant residents followUps.wellChild
        , acuteIllness = filterByParticipant residents followUps.acuteIllness
        , prenatal = filterByParticipant residents followUps.prenatal
        , nextVisit = filterByParticipant residents followUps.nextVisit
    }


filterByParticipant : List PersonId -> Dict id { a | participantId : PersonId } -> Dict id { a | participantId : PersonId }
filterByParticipant residents =
    Dict.filter (\_ followUp -> List.member followUp.participantId residents)


filterDictFollowUpsSetToNotNeeded :
    Dict a { c | value : { b | options : EverySet FollowUpOption } }
    -> Dict a { c | value : { b | options : EverySet FollowUpOption } }
filterDictFollowUpsSetToNotNeeded =
    Dict.filter
        (\_ item ->
            not <| EverySet.member FollowUpNotNeeded item.value.options
        )


filterListFollowUpsSetToNotNeeded :
    List { c | value : { b | options : EverySet FollowUpOption } }
    -> List { c | value : { b | options : EverySet FollowUpOption } }
filterListFollowUpsSetToNotNeeded =
    List.filter
        (\item ->
            not <| EverySet.member FollowUpNotNeeded item.value.options
        )
