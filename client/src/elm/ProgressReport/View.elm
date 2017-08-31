module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Pages.Participant.Model exposing (Msg)
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import EveryDict
import Examination.Model exposing (ExaminationChild)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput, onWithOptions)
import Maybe.Extra exposing (isJust)
import RemoteData exposing (RemoteData(..))
import Translate as Trans exposing (Language(..), TranslationId, translate)
import User.Model exposing (..)


viewProgressReport : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Html Msg
viewProgressReport backendUrl accessToken user language ( childId, child ) =
    div [ class "ui full segment progress-report" ]
        [ text <| toString child.examinations ]
