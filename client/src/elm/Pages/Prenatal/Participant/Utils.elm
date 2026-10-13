module Pages.Prenatal.Participant.Utils exposing (isPregnancyActive, pregnancyConcludedInTodaysPostpartumVisit)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust)


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


{-| Outcome recorded today, with a postpartum visit of the viewer's role started
today. The participant page keeps this pregnancy, so that visit reopens instead
of starting a second pregnancy.
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
                    && (encounter.startDate == currentDate)
            )
            encounters
