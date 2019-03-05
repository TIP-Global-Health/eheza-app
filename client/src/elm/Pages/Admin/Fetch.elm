module Pages.Admin.Fetch exposing (fetch)

import Backend.Model
import Form
import Gizra.NominalDate exposing (NominalDate)
import Pages.Admin.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Utils.WebData exposing (whenNotAsked)


fetch : NominalDate -> Backend.Model.ModelBackend -> Model -> List Msg
fetch currentDate backend model =
    []



{- TODO: reimplement
   let
       fetchClinics =
           whenNotAsked (MsgBackend <| Backend.Model.FetchClinics) backend.clinics

       fetchSessions =
           case backend.futureSessions of
               NotAsked ->
                   Just <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate

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
                       Just <| MsgBackend <| Backend.Model.FetchFutureSessions currentDate

       -- We reset our form if our form status was submitted, and we got a successful result.
       resetPostSession =
           case backend.postSessionRequest of
               Success _ ->
                   model.createSession
                       |> Maybe.andThen
                           (\form ->
                               if Form.isSubmitted form then
                                   Just ResetCreateSessionForm

                               else
                                   Nothing
                           )

               _ ->
                   Nothing
   in
   List.filterMap identity
       [ fetchClinics
       , fetchSessions
       , resetPostSession
       ]
-}
