module Pages.Nutrition.Activity.Test exposing (all)

import Backend.Model exposing (emptyModelIndexedDb)
import EverySet
import Expect
import Measurement.Model exposing (AnthropometricMeasurement(..), emptyHeightForm, emptyMuacForm, emptyWeightForm)
import Pages.Nutrition.Activity.Model exposing (Msg(..), emptyModel)
import Pages.Nutrition.Activity.Update exposing (update)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Pages.Nutrition.Activity"
        [ measurementOutOfRangeTests
        ]


{-| What the Save action does with a measurement that is outside the range it
can take.

The measurement is named on a popup and nothing is saved. The form is left as
it is, so the measurement can be entered again.

-}
measurementOutOfRangeTests : Test
measurementOutOfRangeTests =
    let
        modelWith height muac weight =
            { emptyModel
                | heightData = { form = { emptyHeightForm | height = height } }
                , muacData = { form = { emptyMuacForm | muac = muac } }
                , weightData = { form = { emptyWeightForm | weight = weight } }
            }

        preSave model msg =
            let
                ( updatedModel, _, appMsgs ) =
                    update SiteRwanda (toEntityUuid "encounter") emptyModelIndexedDb msg model
            in
            -- What the popup names, and whether anything was saved.
            ( updatedModel.measurementOutOfRangePopupState, not <| List.isEmpty appMsgs )

        person =
            toEntityUuid "person"
    in
    describe "the save gate"
        [ test "a height outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith (Just 1050) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing)
                    |> Expect.equal ( [ MeasurementHeight ], False )
        , test "a weight outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing Nothing (Just 850))
                    (PreSaveWeight EverySet.empty person Nothing)
                    |> Expect.equal ( [ MeasurementWeight ], False )
        , test "a MUAC outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing (Just 125) Nothing)
                    (PreSaveMuac person Nothing)
                    |> Expect.equal ( [ MeasurementMuac ], False )
        , test "a MUAC that is millimetres at Burundi is within range there" <|
            \_ ->
                let
                    ( updatedModel, _, _ ) =
                        update SiteBurundi
                            (toEntityUuid "encounter")
                            emptyModelIndexedDb
                            (PreSaveMuac person Nothing)
                            (modelWith Nothing (Just 12.5) Nothing)
                in
                updatedModel.measurementOutOfRangePopupState
                    |> Expect.equal []
        , test "a height within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith (Just 105) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing)
                    |> Expect.equal ( [], True )
        , test "a weight within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith Nothing Nothing (Just 12))
                    (PreSaveWeight EverySet.empty person Nothing)
                    |> Expect.equal ( [], True )
        ]
