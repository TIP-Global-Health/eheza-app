module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Translate as Trans exposing (Language(..), TranslationId, translate)
import ZScore.Model
import ZScore.View


viewProgressReport : Language -> ZScore.Model.Model -> ChildId -> EditableSession -> Html any
viewProgressReport language zscores childId session =
    div [ class "ui full segment progress-report" ]
        [ ZScore.View.viewHeightForAgeBoys zscores
        ]
