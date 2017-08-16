module Pages.Participants.Update exposing (update, urlFragment)

import Activity.Encoder exposing (encodeActivityType)
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Pages.Participants.Model exposing (Model, Msg(..))
import Participant.Model exposing (ParticipantTypeFilter(..), ParticipantsDict)
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


update : BackendUrl -> String -> User -> Msg -> ParticipantsDict -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg participants model =
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

        SetParticipantTypeFilter participantTypeFilterString ->
            let
                participantTypeFilter =
                    if participantTypeFilterString == "All" then
                        All
                    else if participantTypeFilterString == "Children" then
                        Children
                    else if participantTypeFilterString == "Mothers" then
                        Mothers
                    else
                        model.participantTypeFilter
            in
                ( { model | participantTypeFilter = participantTypeFilter }
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
