module Pages.OpenSessions.View exposing (view)

{-| The purpose of this page is to show a list of the sessions which
are available for data-entry.

Soon, it will have buttons to download the data needed to
do the data-entry for a session, even if you end up being offline.
For now, we just show the list.

-}

import Html exposing (..)
import App.Model exposing (Msg(..))
import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (ClinicId, SessionId)
import Backend.Model
import Backend.Session.Model exposing (Session)
import Date exposing (Date)
import Drupal.Restful exposing (EntityDictList)
import EveryDictList
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import StorageKey exposing (StorageKey(..))
import Translate as Trans exposing (Language(..), TranslationId, translate)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| In principle, one could just supply the "big" model, but it's nice to avoid
that, where possible ... we only need the Clinics and the open sessions.

We need both because the sessions just have a reference to the ID of the
clinic. The assumption here is that it's reasonable to fetch all the clinics,
but not reasonable to fetch all the sessions (including past sessions).

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> Date -> WebData (EntityDictList ClinicId Clinic) -> WebData ( NominalDate, EntityDictList SessionId Session ) -> Html Msg
view language currentDate clinicData sessionData =
    div
        [ class "ui full segment" ]
        [ viewWrapper language currentDate clinicData sessionData
        ]


viewWrapper : Language -> Date -> WebData (EntityDictList ClinicId Clinic) -> WebData ( NominalDate, EntityDictList SessionId Session ) -> Html Msg
viewWrapper language currentDate clinicData sessionData =
    -- Note that we should get through to the `viewSucessfully` case, since we'll
    -- automatically trigger the necessary fetches. Of course, I suppose an error
    -- is always possible ...
    --
    -- Clearly, the pattern below is some kind of variant on `RemoteData.map` ...
    -- perhaps there is a `RemoteData.view` function lurking here.
    case clinicData of
        NotAsked ->
            -- since it will be automatic
            spinner

        Loading ->
            spinner

        Failure err ->
            div []
                [ viewError language err
                , div
                    [ class "ui button"
                    , onClick (MsgBackend Backend.Model.FetchClinics)
                    ]
                    [ text <| translate language Trans.Retry ]
                ]

        Success clinics ->
            case sessionData of
                NotAsked ->
                    -- again, will be automatic
                    spinner

                Loading ->
                    spinner

                Failure err ->
                    div []
                        [ viewError language err
                        , div
                            [ class "ui button"
                            , Gizra.NominalDate.fromLocalDateTime currentDate
                                |> Backend.Model.FetchSessionsOpenOn
                                |> MsgBackend
                                |> onClick
                            ]
                            [ text <| translate language Trans.Retry ]
                        ]

                Success sessions ->
                    viewWithData language clinics sessions


{-| This is the "inner" view function ... we get here if all the data was actually available.
-}
viewWithData : Language -> EntityDictList ClinicId Clinic -> ( NominalDate, EntityDictList SessionId Session ) -> Html Msg
viewWithData language clinics ( sessionDate, sessions ) =
    -- Start with something simplistic. We'll add a `download` button soon.
    -- TODO: Run text snippets through the "translate" mechanism.
    div []
        [ h4 []
            [ text "Available sessions for: "
            , text <| Gizra.NominalDate.formatYYYYMMDD sessionDate
            ]
        , sessions
            |> EveryDictList.toList
            |> List.map (viewSession clinics)
            |> ul []
        ]


viewSession : EntityDictList ClinicId Clinic -> ( StorageKey SessionId, Session ) -> Html Msg
viewSession clinics ( sessionId, session ) =
    let
        clinicName =
            EveryDictList.get (Existing session.clinicId) clinics
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown clinic"

        downloadLink =
            case sessionId of
                Existing id ->
                    span
                        [ Backend.Model.FetchOfflineSession id
                            |> MsgBackend
                            |> onClick
                        ]
                        [ text "Download" ]

                New ->
                    emptyNode
    in
        li []
            [ text clinicName
            , text " "
            , text (formatYYYYMMDD session.scheduledDate.start)
            , text " - "
            , text (formatYYYYMMDD session.scheduledDate.end)
            , text " "
            , downloadLink
            ]
