module Pages.OpenSessions.View exposing (view)

{-| The purpose of this page is to show a list of the sessions which
are available for data-entry.

Soon, it will have buttons to download the data needed to
do the data-entry for a session, even if you end up being offline.
For now, we just show the list.

-}

import Html exposing (..)
import App.Model exposing (Msg)
import Clinic.Model exposing (ClinicId, Clinic)
import Date exposing (Date)
import Drupal.Restful exposing (EntityDictList)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)
import Session.Model exposing (SessionId, Session)
import Translate as Trans exposing (Language(..), TranslationId, translate)


{-| In principle, one could just supply the "big" model, but it's nice to avoid
that, where possible ... we only need the Clinics and the open sessions.

We need both because the sessions just have a reference to the ID of the
clinic. The assumption here is that it's reasonable to fetch all the clinics,
but not reasonable to fetch all the sessions (including past sessions).

We need the current date so that we can show something in the UI when we don't
have sessions for the correct date. In fact, this should automatically trigger
a re-fetch, but it is good methodology to cover all the caess in the UI.

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

-}
view : Language -> Date -> WebData (EntityDictList ClinicId Clinic) -> WebData ( NominalDate, EntityDictList SessionId Session ) -> Html Msg
view language currentDate clinicData sessionData =
    div [] [ text "Open Sessions" ]
