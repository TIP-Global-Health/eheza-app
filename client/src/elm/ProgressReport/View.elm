module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Pages.Participant.Model exposing (Msg)
import Translate as Trans exposing (Language(..), TranslationId, translate)


viewProgressReport : Language -> ChildId -> EditableSession -> Html Msg
viewProgressReport language childId session =
    div [ class "ui full segment progress-report" ]
        [ text "The progress report will go here." ]
