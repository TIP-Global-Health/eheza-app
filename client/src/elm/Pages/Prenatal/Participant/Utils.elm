module Pages.Prenatal.Participant.Utils exposing (antenatalPregnanciesNewestFirst, isPregnancyActive, pregnancyConcludedInTodaysPostpartumVisit, resolveShownPregnancy)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra
import Maybe.Extra exposing (isJust)
import Pages.Prenatal.Participant.Model exposing (ShownPregnancy)
import Restful.Endpoint exposing (fromEntityUuid)
import Utils.NominalDate exposing (sortEncounterTuplesDesc)


{-| A person's antenatal pregnancies, newest first by registration date.
Pregnancies registered on the same day are ordered by their id, so every
device lists them alike.
-}
antenatalPregnanciesNewestFirst : Dict IndividualEncounterParticipantId IndividualEncounterParticipant -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
antenatalPregnanciesNewestFirst participants =
    Dict.toList participants
        |> List.filter (\( _, participant ) -> participant.encounterType == AntenatalEncounter)
        |> List.sortBy (Tuple.first >> fromEntityUuid)
        |> List.sortWith sortEncounterTuplesDesc


{-| The pregnancy the participant page shows: the newest active one. Without
one, a pregnancy concluded in this role's postpartum visit today is kept, so
the visit reopens.
-}
resolveShownPregnancy :
    NominalDate
    -> Bool
    -> (IndividualEncounterParticipantId -> List ( PrenatalEncounterId, PrenatalEncounter ))
    -> Dict IndividualEncounterParticipantId IndividualEncounterParticipant
    -> Maybe ShownPregnancy
resolveShownPregnancy currentDate isChw encountersFor participants =
    let
        pregnancies =
            antenatalPregnanciesNewestFirst participants

        shown keptForPostpartumVisit sessionId =
            { sessionId = sessionId
            , encounters = encountersFor sessionId
            , keptForPostpartumVisit = keptForPostpartumVisit
            }
    in
    case List.Extra.find (Tuple.second >> isPregnancyActive currentDate) pregnancies of
        Just ( sessionId, _ ) ->
            Just (shown False sessionId)

        Nothing ->
            List.Extra.findMap
                (\( sessionId, session ) ->
                    let
                        pregnancy =
                            shown True sessionId
                    in
                    if pregnancyConcludedInTodaysPostpartumVisit currentDate isChw session (List.map Tuple.second pregnancy.encounters) then
                        Just pregnancy

                    else
                        Nothing
                )
                pregnancies


{-| Pregnancy is considered 'active' if it does not have its end date and
outcome set, and its EDD date is less than 3 month overdue.
When EDD date was never recorded, we estimate it from the date on which the
pregnancy was registered. That date is not the start of the pregnancy, so for
a woman registered late the estimate runs long. This is deliberate: keeping a
concluded pregnancy active a while longer is harmless, while expiring one too
early would allow a second pregnancy to be opened for a woman who is still
pregnant.
-}
isPregnancyActive : NominalDate -> IndividualEncounterParticipant -> Bool
isPregnancyActive currentDate session =
    if isJust session.endDate && isJust session.outcome then
        False

    else
        let
            eddDate =
                Maybe.withDefault (lmpToEDDDate session.startDate) session.eddDate
        in
        diffDays eddDate currentDate < 92


{-| Outcome recorded today, with this role's postpartum visit still open or ended
today. The page keeps the pregnancy, so that visit reopens instead of a second
pregnancy starting.
-}
pregnancyConcludedInTodaysPostpartumVisit : NominalDate -> Bool -> IndividualEncounterParticipant -> List PrenatalEncounter -> Bool
pregnancyConcludedInTodaysPostpartumVisit currentDate isChw session encounters =
    let
        postpartumEncounterType =
            if isChw then
                ChwPostpartumEncounter

            else
                NursePostpartumEncounter
    in
    (session.endDate == Just currentDate)
        && isJust session.outcome
        && List.any
            (\encounter ->
                (encounter.encounterType == postpartumEncounterType)
                    && List.member encounter.endDate [ Nothing, Just currentDate ]
            )
            encounters
