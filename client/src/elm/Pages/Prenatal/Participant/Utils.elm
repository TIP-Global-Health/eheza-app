module Pages.Prenatal.Participant.Utils exposing (isPregnancyActive)

import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust)


{-| Pregnancy is considered 'active' if it does not have its end date and
outcome set, and its EDD date is less than 3 month overdue.
When EDD date was never recorded, we estimate it from the date on which
the pregnancy was registered.
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
