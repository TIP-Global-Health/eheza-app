module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Html exposing (..)
import Html.Attributes exposing (..)
import Translate exposing (Language(..), translate)
import ZScore.Model
import ZScore.View


viewProgressReport : Language -> ZScore.Model.Model -> ( ChildId, Child ) -> EditableSession -> Html any
viewProgressReport language zscores ( childId, child ) session =
    let
        ( heightForAge, weightForAge, weightForHeight ) =
            case child.gender of
                Male ->
                    ( ZScore.View.viewHeightForAgeBoys
                    , ZScore.View.viewWeightForAgeBoys
                    , ZScore.View.viewWeightForHeightBoys
                    )

                Female ->
                    ( ZScore.View.viewHeightForAgeGirls
                    , ZScore.View.viewWeightForAgeGirls
                    , ZScore.View.viewWeightForHeightGirls
                    )
    in
        div [ class "ui full segment progress-report" ]
            [ heightForAge zscores []
            , weightForAge zscores
            , weightForHeight zscores
            ]
