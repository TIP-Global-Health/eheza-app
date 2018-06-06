module Pages.Admin.Fetch exposing (fetch)

import Backend.Model
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.WebData exposing (whenNotAsked)


fetch : NominalDate -> Backend.Model.ModelBackend -> List Backend.Model.MsgBackend
fetch currentDate backend =
    let
        fetchClinics =
            whenNotAsked Backend.Model.FetchClinics backend.clinics

        fetchSessions =
            case backend.futureSessions of
                NotAsked ->
                    Just <| Backend.Model.FetchFutureSessions currentDate

                Loading ->
                    Nothing

                Failure _ ->
                    Nothing

                Success ( queryDate, _ ) ->
                    -- We remember the date we queried about, so that
                    -- if we are now on a different date, we know to
                    -- redo the query.
                    if queryDate == currentDate then
                        Nothing
                    else
                        Just <| Backend.Model.FetchFutureSessions currentDate
    in
        List.filterMap identity
            [ fetchClinics
            , fetchSessions
            ]
