module Pages.PrenatalParticipant.Utils exposing (isPregnancyActive)

import Backend.Entities exposing (..)
import Backend.PrenatalParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate, diffDays)
import Maybe.Extra exposing (isJust)


{-| Preganancy is considered 'active' if it does not have it's end
date set, and it's EDD date is not set, or, EDD date is less than
3 month overdue.
-}
isPregnancyActive : NominalDate -> PrenatalParticipant -> Bool
isPregnancyActive currentDate session =
    if isJust session.endDate then
        False

    else
        session.eddDate
            |> Maybe.map
                (\eddDate -> diffDays eddDate currentDate < 92)
            |> Maybe.withDefault True
