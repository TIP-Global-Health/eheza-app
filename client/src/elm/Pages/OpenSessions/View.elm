module Pages.OpenSessions.View exposing (view, fetch)

{-| The purpose of this page is to show a list of the sessions which
are available for data-entry.

Soon, it will have buttons to download the data needed to
do the data-entry for a session, even if you end up being offline.
For now, we just show the list.

-}

import Html exposing (..)
import App.Model exposing (Msg(..))
import Clinic.Model exposing (ClinicId, Clinic)
import Date exposing (Date)
import Drupal.Restful exposing (EntityDictList)
import EveryDictList
import Gizra.NominalDate exposing (formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Session.Model exposing (SessionId, Session)
import StorageKey exposing (StorageKey(..))
import Translate as Trans exposing (Language(..), TranslationId, translate)
import Utils.Html exposing (spinner)
import Utils.WebData exposing (viewError)


{-| The `fetch` function is an innovation in how to manage the "lazy" loading
of needed data. It is an alternative to the `...Manager` modules that we have
sometimes used.

The weakness of the `...Manager` modules is that they don't clearly enough
distinguish between the job of marshalling data and providing the UI. By
putting the fundmental data from the backend in the `Pages` hierarchy, it can
become awkward to maintain a single source of truth. Ideally, we would keep all
contact with the backend out of the UI code, and manage the data more
"centrally." (In fact, we should probably have a `Data` module prefix something
like our `Pages` prefix). Then, each view can be provided with whatever data
it needs, from the centrally managed store.

Now, that would be straight-forward if we always had all our data. But we don't
-- we load it lazily from the backend as needed. And, it is the `Manager` modules
(and the view layer in general) that actually knows what data it needs to have
loaded. That is, it is the `view` functions that know what data they actually
need to have loaded. So, that was a strength of the `Manager` modules ... they
kept together the view and the knowledge of what data the view needed.

But, of course, when you're actually in the `view` function, you can't actually
trigger something that will fetch data. That is fundamentally something that
has to happen in the `update` function. However, logically, the data you need
to fetch is not something that is determined by the `msg` you received. So,
the normal "switching" and dispatching on the `msg` isn't what's called for.
Instead, what one would want is to switch on the `activePage` -- that is,
to follow the same hierarchy as the `view` function, but instead of asking for
some HTML, ask whether any data needs to be loaded.

So, the idea for the `fetch` function is that it would ordinarily take the
same parameters as `view`, and it would sort of follow the `view` function,
by assembling a List of messages which need to be sent in order to load data.
Of course, it's important not to load the data over and over again, so this
needs to be tied up with a kind of `WebData` wrapper for the data, so we know
not to trigger a fetch when one is in progress. ONe would also not want to
triggr a fetch in case of error (at least, not without a delay), since you
wouldn't want to just automatically retry errors constantly.

-}
fetch : Date -> WebData (EntityDictList ClinicId Clinic) -> WebData ( NominalDate, EntityDictList SessionId Session ) -> List Msg
fetch currentDate clinics sessions =
    -- So, to recap, this is called by the parent's `update` function, to see whether
    -- any data needs to be fetched. We can return messages that will fetch needed
    -- data. The function is located here because it is kind of a companion to the
    -- `view` function ... it needs to stay in sync with the view function, so that
    -- the things the `view` function needs are in fact loaded. So, the signature for
    -- the two functions would generally be related.
    let
        nominalDate =
            Gizra.NominalDate.fromLocalDateTime currentDate

        fetchSessions =
            case sessions of
                NotAsked ->
                    Just (FetchSessionsOpenOn nominalDate)

                Loading ->
                    Nothing

                Failure _ ->
                    Nothing

                Success ( sessionDate, _ ) ->
                    if sessionDate == nominalDate then
                        Nothing
                    else
                        Just (FetchSessionsOpenOn nominalDate)

        fetchClinics =
            case clinics of
                NotAsked ->
                    Just FetchClinics

                _ ->
                    Nothing
    in
        List.filterMap identity
            [ fetchSessions
            , fetchClinics
            ]


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
                    , onClick FetchClinics
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
                            , onClick <|
                                FetchSessionsOpenOn <|
                                    Gizra.NominalDate.fromLocalDateTime currentDate
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
    in
        li []
            [ text clinicName
            , text " "
            , text (formatYYYYMMDD session.scheduledDate.start)
            , text " - "
            , text (formatYYYYMMDD session.scheduledDate.end)
            ]
