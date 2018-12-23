module Pages.PatientRegistration.View exposing (view)

{-| The purpose of this page is
-}

import App.Model exposing (Msg(..))
import Backend.Model exposing (ModelBackend, ModelCached, MsgBackend(..))
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import RemoteData exposing (RemoteData(..), WebData)
import Time.Date exposing (delta)
import Translate exposing (Language(..), TranslationId, translate)
import User.Model exposing (User)


{-| To make things simpler, we just supply the whole state of the backend ... the view
can pick out what it wants. (Sometimes it would be a better idea to pass more
specific things, rather than the whole state of the backend).

For now, at least, we don't really need our own `Msg` type, so we're just using
the big one.

If `selectedClinic` is Just, we'll show a page for that clinic. If not, we'll
show a list of clinics.

-}
view : Language -> NominalDate -> User -> ModelBackend -> ModelCached -> Html Msg
view language currentDate user backend cache =
    div [] [ text "hello" ]
