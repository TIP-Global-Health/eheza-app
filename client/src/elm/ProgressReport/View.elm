module ProgressReport.View
    exposing
        ( viewProgressReport
        )

import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Height, Weight, HeightInCm(..), WeightInKg(..))
import Backend.Measurement.Utils exposing (mapMeasurementData, currentValueWithId)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildHistoricalMeasurements, getChildMeasurementData)
import Html exposing (..)
import Html.Attributes exposing (..)
import Translate exposing (Language(..), translate)
import Utils.NominalDate exposing (Days(..), diffDays)
import ZScore.Model exposing (Centimetres(..), Kilograms(..))
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

        historical =
            getChildHistoricalMeasurements childId session.offlineSession

        current =
            getChildMeasurementData childId session

        -- This includes any edits that have been saved locally, but not as-you=type
        -- in the UI before you hit "Save" or "Update".
        currentHeight =
            current
                |> mapMeasurementData .height .height
                |> currentValueWithId

        currentWeight =
            current
                |> mapMeasurementData .weight .weight
                |> currentValueWithId

        heightValues =
            case currentHeight of
                Nothing ->
                    -- No current value, so just use historical
                    List.map Tuple.second historical.heights

                Just ( Nothing, currentValue ) ->
                    -- We have a new current value, so use it
                    currentValue :: List.map Tuple.second historical.heights

                Just ( Just currentId, currentValue ) ->
                    -- We've edited an old value, so use the edited version
                    -- and leave out the old one.
                    historical.heights
                        |> List.filter (\( id, _ ) -> id /= currentId)
                        |> List.map Tuple.second
                        |> List.append [ currentValue ]

        weightValues =
            case currentWeight of
                Nothing ->
                    -- No current value, so just use historical
                    List.map Tuple.second historical.weights

                Just ( Nothing, currentValue ) ->
                    -- We have a new current value, so use it
                    currentValue :: List.map Tuple.second historical.weights

                Just ( Just currentId, currentValue ) ->
                    -- We've edited an old value, so use the edited version
                    -- and leave out the old one.
                    historical.weights
                        |> List.filter (\( id, _ ) -> id /= currentId)
                        |> List.map Tuple.second
                        |> List.append [ currentValue ]

        heightForAgeData =
            List.map (chartHeightForAge child) heightValues

        weightForAgeData =
            List.map (chartWeightForAge child) weightValues
    in
        div [ class "ui full segment progress-report" ]
            [ ZScore.View.viewMarkers
            , heightForAge zscores heightForAgeData
            , weightForAge zscores weightForAgeData
            , weightForHeight zscores
            ]


chartHeightForAge : Child -> Height -> ( Days, Centimetres )
chartHeightForAge child height =
    ( diffDays child.birthDate height.dateMeasured
      -- I suppose one could avoid this little transformation
      -- by unifiying the two tags.
    , case height.value of
        HeightInCm cm ->
            Centimetres cm
    )


chartWeightForAge : Child -> Weight -> ( Days, Kilograms )
chartWeightForAge child weight =
    ( diffDays child.birthDate weight.dateMeasured
      -- I suppose one could avoid this little transformation
      -- by unifiying the two tags.
    , case weight.value of
        WeightInKg cm ->
            Kilograms cm
    )
