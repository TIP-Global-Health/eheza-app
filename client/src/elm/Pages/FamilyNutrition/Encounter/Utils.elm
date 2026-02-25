module Pages.FamilyNutrition.Encounter.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity(..))
import Backend.FamilyNutritionActivity.Utils exposing (allActivities)
import Backend.FamilyNutritionEncounter.Utils exposing (getFamilyNutritionEncountersForParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


generateAssembledData : FamilyNutritionEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.familyNutritionEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.familyNutritionMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.familyParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            RemoteData.toMaybe encounter
                |> Maybe.map
                    (\encounter_ ->
                        generatePreviousMeasurements (Just id) encounter_.participant db
                    )
                |> Maybe.withDefault []

        children =
            RemoteData.andThen
                (\encounter_ ->
                    participant
                        |> RemoteData.andThen
                            (\participant_ ->
                                Dict.get participant_.person db.relationshipsByPerson
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> Maybe.map
                                        (Dict.values
                                            >> List.filter (.relatedBy >> (==) MyChild)
                                            >> List.filterMap
                                                (\rel ->
                                                    Dict.get rel.relatedTo db.people
                                                        |> Maybe.andThen RemoteData.toMaybe
                                                        |> Maybe.map (\child -> ( rel.relatedTo, child ))
                                                )
                                            >> List.filter
                                                (\( _, child ) ->
                                                    isPersonAnAdult encounter_.startDate child
                                                        |> Maybe.map not
                                                        |> Maybe.withDefault True
                                                )
                                        )
                                    |> Maybe.withDefault []
                                    |> Success
                            )
                )
                encounter
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)
        |> RemoteData.andMap children


nextFamilyMemberWithPendingActivities :
    NominalDate
    -> FamilyMember
    -> List ( PersonId, Person )
    -> FamilyNutritionMeasurements
    -> Maybe FamilyMember
nextFamilyMemberWithPendingActivities currentDate current children measurements =
    let
        allMembers =
            FamilyMemberMother
                :: List.map (\( childId, _ ) -> FamilyMemberChild childId) children

        -- Get members after current in cycle order (excluding current),
        -- wrapping around to include members before current.
        membersAfterCurrent =
            case current of
                FamilyMemberMother ->
                    List.drop 1 allMembers

                FamilyMemberChild currentId ->
                    let
                        splitAtCurrent remaining before =
                            case remaining of
                                [] ->
                                    before

                                (FamilyMemberChild cid) :: rest ->
                                    if cid == currentId then
                                        rest ++ List.reverse before

                                    else
                                        splitAtCurrent rest (FamilyMemberChild cid :: before)

                                member :: rest ->
                                    splitAtCurrent rest (member :: before)
                    in
                    splitAtCurrent allMembers []

        hasPendingActivities member =
            activitiesForFamilyMember currentDate member children
                |> List.all (activityCompleted member measurements)
                |> not
    in
    List.filter hasPendingActivities membersAfterCurrent
        |> List.head


generatePreviousMeasurements :
    Maybe FamilyNutritionEncounterId
    -> FamilyEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( FamilyNutritionEncounterId, FamilyNutritionMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    getFamilyNutritionEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    Dict.get encounterId db.familyNutritionMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (\data ->
                                ( encounter.startDate, ( encounterId, data ) )
                            )
            )
        |> List.sortWith sortTuplesByDateDesc


activitiesForFamilyMember : NominalDate -> FamilyMember -> List ( PersonId, Person ) -> List FamilyNutritionActivity
activitiesForFamilyMember currentDate familyMember children =
    case familyMember of
        FamilyMemberMother ->
            -- Photo is only for children, not for mother.
            List.filter ((/=) FamilyNutritionPhoto) allActivities

        FamilyMemberChild childId ->
            let
                childIsAbove6Months =
                    List.filter (\( id, _ ) -> id == childId) children
                        |> List.head
                        |> Maybe.andThen (\( _, person ) -> ageInMonths currentDate person)
                        |> Maybe.map (\months -> months >= 6)
                        |> Maybe.withDefault False
            in
            if childIsAbove6Months then
                allActivities

            else
                List.filter ((/=) FamilyNutritionMuac) allActivities


activityCompleted : FamilyMember -> FamilyNutritionMeasurements -> FamilyNutritionActivity -> Bool
activityCompleted familyMember measurements activity =
    case ( familyMember, activity ) of
        ( FamilyMemberMother, FamilyNutritionAheza ) ->
            measurements.ahezaMother /= Nothing

        ( FamilyMemberMother, FamilyNutritionMuac ) ->
            measurements.muacMother /= Nothing

        -- Photo is not applicable for mother, but handle the case gracefully.
        ( FamilyMemberMother, FamilyNutritionPhoto ) ->
            True

        ( FamilyMemberChild childId, FamilyNutritionAheza ) ->
            Dict.member childId measurements.ahezaChild

        ( FamilyMemberChild childId, FamilyNutritionMuac ) ->
            Dict.member childId measurements.muacChild

        ( FamilyMemberChild childId, FamilyNutritionPhoto ) ->
            Dict.member childId measurements.photo
