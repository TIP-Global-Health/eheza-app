module Pages.Prenatal.Participant.Utils exposing (isPregnancyActive, pregnancyConcludedInTodaysPostpartumVisit, resolveChwVisitButtons, resolveShownPregnancy)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date
import Gizra.NominalDate exposing (NominalDate, diffDays)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Prenatal.Participant.Model exposing (ChwVisitButtons, ShownPregnancy)
import Restful.Endpoint exposing (fromEntityUuid)


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
        -- Newest registration first. Of those registered the same day, the one
        -- with more visits comes first, then the lower id, so load order never decides.
        pregnancies =
            Dict.toList participants
                |> List.filter (\( _, participant ) -> participant.encounterType == AntenatalEncounter)
                |> List.map
                    (\( sessionId, session ) ->
                        ( session
                        , { sessionId = sessionId
                          , encounters = encountersFor sessionId
                          , keptForPostpartumVisit = False
                          }
                        )
                    )
                |> List.sortBy
                    (\( session, pregnancy ) ->
                        ( negate (Date.toRataDie session.startDate), negate (List.length pregnancy.encounters), fromEntityUuid pregnancy.sessionId )
                    )
    in
    case List.Extra.find (Tuple.first >> isPregnancyActive currentDate) pregnancies of
        Just ( _, pregnancy ) ->
            Just pregnancy

        Nothing ->
            List.Extra.find
                (\( session, pregnancy ) ->
                    pregnancyConcludedInTodaysPostpartumVisit currentDate isChw session (List.map Tuple.second pregnancy.encounters)
                )
                pregnancies
                |> Maybe.map (\( _, pregnancy ) -> { pregnancy | keptForPostpartumVisit = True })


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


{-| State of the three CHW visit buttons, from the pregnancy's CHW visits,
newest first.
-}
resolveChwVisitButtons : NominalDate -> List ( PrenatalEncounterId, PrenatalEncounter ) -> ChwVisitButtons
resolveChwVisitButtons currentDate encounters =
    let
        ( visitToResume, lastEncounterType, encounterWasCompletedToday ) =
            List.head encounters
                |> Maybe.map
                    (\( encounterId, encounter ) ->
                        let
                            -- An open visit is resumed on the day it started. An open
                            -- postpartum visit is resumed on any day, as the nurse's is.
                            activeEncounterId =
                                if isJust encounter.endDate then
                                    Nothing

                                else if encounter.startDate == currentDate || encounter.encounterType == ChwPostpartumEncounter then
                                    Just encounterId

                                else
                                    Nothing
                        in
                        ( activeEncounterId, Just encounter.encounterType, encounter.endDate == Just currentDate )
                    )
                |> Maybe.withDefault ( Nothing, Nothing, False )

        -- Button for certain type is active when:
        -- 1. There's an active encounter, and it's type matches button encounter type.
        -- 2. No active encounter, and last completed encounter was of previous type.
        --   For example, second encounter is active, if last completed encounter was first encounter.
        encounterTypeButtonActive encounterType =
            if isJust visitToResume then
                lastEncounterType == Just encounterType

            else
                case encounterType of
                    ChwFirstEncounter ->
                        isNothing lastEncounterType

                    ChwSecondEncounter ->
                        lastEncounterType == Just ChwFirstEncounter && not encounterWasCompletedToday

                    ChwThirdPlusEncounter ->
                        -- There can be multiple 'ChwThird' encounters.
                        (lastEncounterType == Just ChwSecondEncounter || lastEncounterType == Just ChwThirdPlusEncounter)
                            && not encounterWasCompletedToday

                    ChwPostpartumEncounter ->
                        case lastEncounterType of
                            Just lastEncounterType_ ->
                                -- Last encounter is not a postpartum encounter,
                                -- and there's no encounter that was completed today.
                                (lastEncounterType_ /= ChwPostpartumEncounter)
                                    && not encounterWasCompletedToday

                            Nothing ->
                                -- When there're no encounters, we allow to
                                -- create postpartum encounter.
                                True

                    -- We should never get here, as we deal only with CHW encounters.
                    NurseEncounter ->
                        False

                    -- We should never get here, as we deal only with CHW encounters.
                    NursePostpartumEncounter ->
                        False

        -- Used for subsequent encounter button, which is responsible
        -- to start / navigate to second or third encounters.
        subsequentVisitType =
            case lastEncounterType of
                Just ChwFirstEncounter ->
                    ChwSecondEncounter

                Just ChwSecondEncounter ->
                    if isJust visitToResume then
                        ChwSecondEncounter

                    else
                        ChwThirdPlusEncounter

                _ ->
                    ChwThirdPlusEncounter
    in
    { visitToResume = visitToResume
    , firstVisitEnabled = encounterTypeButtonActive ChwFirstEncounter
    , subsequentVisitType = subsequentVisitType
    , subsequentVisitEnabled = encounterTypeButtonActive subsequentVisitType
    , postpartumVisitEnabled = encounterTypeButtonActive ChwPostpartumEncounter
    }
