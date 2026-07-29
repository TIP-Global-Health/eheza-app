module Pages.FamilyNutrition.Encounter.Test exposing (all)

import Backend.Model exposing (emptyModelIndexedDb)
import Expect
import Measurement.Model exposing (AnthropometricMeasurement(..), emptyMuacForm)
import Pages.FamilyNutrition.Encounter.Model exposing (Msg(..), emptyModel)
import Pages.FamilyNutrition.Encounter.Update exposing (update)
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Pages.FamilyNutrition.Encounter"
        [ preSaveMuacTest
        ]


{-| This encounter shows no range above the input, so a MUAC outside it was
refused by a Save button that would not answer and said nothing at all. The
measurement is named on a warning instead, which is the only place the range is
said here.

The same warning is asked for whether the MUAC is the mother's or a child's.

-}
preSaveMuacTest : Test
preSaveMuacTest =
    let
        person =
            toEntityUuid "person"

        preSave site muac msg =
            let
                data =
                    emptyModel.muacData

                model =
                    { emptyModel | muacData = { data | form = { emptyMuacForm | muac = muac } } }

                ( updatedModel, _, appMsgs ) =
                    update site (toEntityUuid "encounter") emptyModelIndexedDb msg model
            in
            -- What the warning names, and whether anything was saved.
            ( updatedModel.measurementOutOfRangePopupState, not <| List.isEmpty appMsgs )
    in
    describe "the MUAC save gate"
        [ test "the mother's: a plausible 12.5 cm names nothing and saves" <|
            \_ ->
                preSave SiteRwanda (Just 12.5) (PreSaveMuacMother person Nothing)
                    |> Expect.equal ( [], True )
        , test "the mother's: 125, a mm value typed into a cm field, is named and saves nothing" <|
            \_ ->
                preSave SiteRwanda (Just 125) (PreSaveMuacMother person Nothing)
                    |> Expect.equal ( [ MeasurementMuac ], False )
        , test "a child's is asked the same way" <|
            \_ ->
                preSave SiteRwanda (Just 125) (PreSaveMuacChild person Nothing)
                    |> Expect.equal ( [ MeasurementMuac ], False )
        , test "Burundi: a form holding 12.5 cm is 125 mm there, so it names nothing and saves" <|
            \_ ->
                preSave SiteBurundi (Just 12.5) (PreSaveMuacMother person Nothing)
                    |> Expect.equal ( [], True )
        , test "Burundi: a form holding 1.25 cm is 12.5 mm there, below the range" <|
            \_ ->
                preSave SiteBurundi (Just 1.25) (PreSaveMuacMother person Nothing)
                    |> Expect.equal ( [ MeasurementMuac ], False )
        , test "closing the warning names nothing, and saves nothing on the way" <|
            \_ ->
                preSave SiteRwanda (Just 125) (SetMeasurementOutOfRangePopupState [])
                    |> Expect.equal ( [], False )
        ]
