module Pages.Patients.Update exposing (update, urlFragment)

import Activity.Encoder exposing (encodeActivityType)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Pages.Patients.Model exposing (Model, Msg(..))
import Patient.Model exposing (PatientTypeFilter(..), PatientsDict)
import User.Model exposing (..)


{-| What should we show in the part of the URL we will be asked to decode?
-}
urlFragment : Model -> String
urlFragment model =
    model.activityTypeFilter
        |> List.map encodeActivityType
        |> String.join "&"
        |> (\fragment ->
                if fragment == "" then
                    ""
                else
                    "activites=" ++ fragment
           )


update : BackendUrl -> String -> User -> Msg -> PatientsDict -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg patients model =
    case msg of
        SetActivityTypeFilter activityType isChecked ->
            let
                activityTypeFilterUpdated =
                    if isChecked then
                        activityType :: model.activityTypeFilter
                    else
                        List.filter ((/=) activityType) model.activityTypeFilter
            in
                ( { model | activityTypeFilter = activityTypeFilterUpdated }
                , Cmd.none
                , Nothing
                )

        SetActivityTypeFilters activityTypeFilters ->
            ( { model | activityTypeFilter = activityTypeFilters }
            , Cmd.none
            , Nothing
            )

        SetPatientTypeFilter patientTypeFilterString ->
            let
                patientTypeFilter =
                    if patientTypeFilterString == "All" then
                        All
                    else if patientTypeFilterString == "Children" then
                        Children
                    else if patientTypeFilterString == "Mothers" then
                        Mothers
                    else
                        model.patientTypeFilter
            in
                ( { model | patientTypeFilter = patientTypeFilter }
                , Cmd.none
                , Nothing
                )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            , Nothing
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            , Nothing
            )
