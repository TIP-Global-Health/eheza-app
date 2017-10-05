module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Pages.Participant.Model exposing (Msg)
import Backend.Child.Model exposing (Child)
import Backend.Entities exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Translate as Trans exposing (Language(..), TranslationId, translate)


viewProgressReport : Language -> ( ChildId, Child ) -> Html Msg
viewProgressReport language ( childId, child ) =
    div [ class "ui full segment progress-report" ]
        [ text <| toString child.examinations ]
