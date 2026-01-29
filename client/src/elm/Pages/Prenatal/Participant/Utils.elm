module Pages.Prenatal.Participant.Utils exposing (isPregnancyActive)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust)


{-| Preganancy is considered 'active' if it does not have it's end
date and outcome set, or it's EDD date is not set, or, EDD date is less than
3 month overdue.
-}
isPregnancyActive : NominalDate -> IndividualEncounterParticipant -> Bool
isPregnancyActive currentDate session =
    if isJust session.endDate && isJust session.outcome then
        False

    else
        session.eddDate
            |> Maybe.map
                (\eddDate -> diffDays eddDate currentDate < 92)
            |> Maybe.withDefault True
