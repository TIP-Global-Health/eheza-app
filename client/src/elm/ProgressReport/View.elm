module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Pages.Participant.Model exposing (Msg)
import Child.Model exposing (Child, ChildId)
import Config.Model exposing (BackendUrl)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Translate as Trans exposing (Language(..), TranslationId, translate)
import User.Model exposing (..)


viewProgressReport : BackendUrl -> String -> User -> Language -> ( ChildId, Child ) -> Html Msg
viewProgressReport backendUrl accessToken user language ( childId, child ) =
    div [ class "ui full segment progress-report" ]
        [ text <| toString child.examinations ]
