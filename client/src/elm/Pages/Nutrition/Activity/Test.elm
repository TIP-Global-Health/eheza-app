module Pages.Nutrition.Activity.Test exposing (all)

import App.Model
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..), NutritionHeight, NutritionWeight, SkippedForm(..), WeightInKg(..))
import Backend.Model exposing (emptyModelIndexedDb)
import Backend.NutritionEncounter.Model
import Date
import EverySet exposing (EverySet)
import Expect exposing (Expectation)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (RangedMeasurement(..), emptyHeightForm, emptyMuacForm, emptyWeightForm)
import Pages.Nutrition.Activity.Model exposing (Msg(..), emptyModel)
import Pages.Nutrition.Activity.Update exposing (update)
import Pages.Page exposing (Page(..), UserPage(..))
import Restful.Endpoint exposing (EntityUuid, toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import TestFixtures exposing (wrapMeasurement)
import Time exposing (Month(..))


all : Test
all =
    describe "Pages.Nutrition.Activity"
        [ measurementOutOfRangeTests
        , notTakenAfterReloadTests
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

        preSaveAt site model msg =
            let
                ( updatedModel, _, appMsgs ) =
                    update site (toEntityUuid "encounter") emptyModelIndexedDb msg model
            in
            -- What the popup names, and whether anything was saved.
            ( updatedModel.measurementOutOfRangePopupState, not <| List.isEmpty appMsgs )

        preSave =
            preSaveAt SiteRwanda

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
        , test "a MUAC that is millimetres at Burundi is within range there, and saves" <|
            \_ ->
                preSaveAt SiteBurundi
                    (modelWith Nothing (Just 12.5) Nothing)
                    (PreSaveMuac person Nothing)
                    |> Expect.equal ( [], True )
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


{-| What pressing Save on the CHW Height and Weight forms does to the
encounter.

"After a reload" means the page model is fresh (`emptyModel`): nothing was
typed or ticked in this app run. The encounter's skipped-forms set, which the
view passes in the PreSave message, still says whether the measurement was
recorded as "unable to take". The form on screen is built from that set, so a
height recorded as not taken shows the box ticked and Save enabled.

Save dispatches `PreSaveHeight` / `PreSaveWeight`, which go on to the Save
message when the value is in range, so the tests drive the page through that
message and look at everything it asks the app to do.

-}
notTakenAfterReloadTests : Test
notTakenAfterReloadTests =
    describe "unable to take, Save"
        [ describe "after a reload, with the box still ticked from the encounter"
            [ test "Height saved as unable to take: Save returns to the encounter page with Height still recorded as not taken" <|
                \_ ->
                    runPage SiteRwanda [ heightSave heightSkipped Nothing ]
                        |> expectNotTaken SkippedHeight
            , test "Height saved as unable to take, at Burundi: Save returns to the encounter page with Height still recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi [ heightSave heightSkipped Nothing ]
                        |> expectNotTaken SkippedHeight

            -- Height was once measured, then later recorded as not taken.
            -- Recording "unable to take" leaves the old height measurement in
            -- place, so both reach the page. The skipped set is what the form
            -- shows, so Save must keep Height as not taken and not re-save
            -- the old value.
            , test "Height saved as unable to take over an earlier height value: Save returns to the encounter page with Height still recorded as not taken" <|
                \_ ->
                    runPage SiteRwanda [ heightSave heightSkipped (savedHeight 100) ]
                        |> expectNotTaken SkippedHeight
            , test "Weight saved as unable to take (Burundi): Save returns to the encounter page with Weight still recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi [ weightSave weightSkipped Nothing ]
                        |> expectNotTaken SkippedWeight
            , test "Weight saved as unable to take over an earlier weight value (Burundi): Save returns to the encounter page with Weight still recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi [ weightSave weightSkipped (savedWeight 12) ]
                        |> expectNotTaken SkippedWeight

            -- Both measurements recorded as not taken: each form's Save keeps
            -- its own measurement as not taken.
            , test "Height and Weight both saved as unable to take: Height's Save returns to the encounter page with Height still recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi [ heightSave bothSkipped Nothing ]
                        |> expectNotTaken SkippedHeight
            , test "Height and Weight both saved as unable to take: Weight's Save returns to the encounter page with Weight still recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi [ weightSave bothSkipped Nothing ]
                        |> expectNotTaken SkippedWeight

            -- Only Weight is recorded as not taken. The Height form is not
            -- ticked and has no value, so there is nothing for its Save to
            -- record: Height must not be taken as "not taken" because some
            -- other measurement was.
            , test "only Weight saved as unable to take: an empty Height form saves nothing and stays on the page" <|
                \_ ->
                    runPage SiteBurundi [ heightSave weightSkipped Nothing ]
                        |> Expect.equal []
            ]
        , describe "after a reload, unticking the box and entering a value"
            [ test "Height saved as unable to take, unticked, given a value and saved: stores that value and Height is no longer recorded as not taken" <|
                \_ ->
                    runPage SiteRwanda
                        [ SetHeightNotTaken False
                        , SetHeight "105"
                        , heightSave heightSkipped Nothing
                        ]
                        |> expectValueStored (HeightSaved Nothing (HeightInCm 105)) SkippedHeight
            , test "Height saved as unable to take over an earlier height value, unticked, given a value and saved: stores the new value on that measurement and Height is no longer recorded as not taken" <|
                \_ ->
                    runPage SiteRwanda
                        [ SetHeightNotTaken False
                        , SetHeight "105"
                        , heightSave heightSkipped (savedHeight 100)
                        ]
                        |> expectValueStored (HeightSaved (Just measurementId) (HeightInCm 105)) SkippedHeight
            , test "Weight saved as unable to take (Burundi), unticked, given a value and saved: stores that value and Weight is no longer recorded as not taken" <|
                \_ ->
                    runPage SiteBurundi
                        [ SetWeightNotTaken False
                        , SetWeight "12"
                        , weightSave weightSkipped Nothing
                        ]
                        |> expectValueStored (WeightSaved Nothing (WeightInKg 12)) SkippedWeight
            ]
        , describe "without a reload"
            [ test "Height ticked as unable to take for the first time: Save returns to the encounter page and records Height as not taken" <|
                \_ ->
                    runPage SiteRwanda
                        [ SetHeightNotTaken True
                        , heightSave EverySet.empty Nothing
                        ]
                        |> expectNewlyNotTaken SkippedHeight
            , test "Weight ticked as unable to take for the first time (Burundi): Save returns to the encounter page and records Weight as not taken" <|
                \_ ->
                    runPage SiteBurundi
                        [ SetWeightNotTaken True
                        , weightSave EverySet.empty Nothing
                        ]
                        |> expectNewlyNotTaken SkippedWeight

            -- Already recorded as not taken, and the box unticked and ticked
            -- again in this app run: the workaround the issue describes.
            , test "Height already recorded as not taken, ticked again in this run: Save returns to the encounter page with Height still recorded as not taken" <|
                \_ ->
                    runPage SiteRwanda
                        [ SetHeightNotTaken False
                        , SetHeightNotTaken True
                        , heightSave heightSkipped Nothing
                        ]
                        |> expectNotTaken SkippedHeight
            ]
        , describe "a height saved with a value, after a reload"
            [ test "Height saved with a value, reopened after a reload, edited and saved: stores the new value" <|
                \_ ->
                    runPage SiteRwanda
                        [ SetHeight "105"
                        , heightSave EverySet.empty (savedHeight 100)
                        ]
                        |> expectValueStored (HeightSaved (Just measurementId) (HeightInCm 105)) SkippedHeight
            , test "Height saved with a value, reopened after a reload and saved unchanged: stores the saved value again" <|
                \_ ->
                    runPage SiteRwanda [ heightSave EverySet.empty (savedHeight 100) ]
                        |> expectValueStored (HeightSaved (Just measurementId) (HeightInCm 100)) SkippedHeight
            , test "Weight saved with a value (Burundi), reopened after a reload, edited and saved: stores the new value" <|
                \_ ->
                    runPage SiteBurundi
                        [ SetWeight "13"
                        , weightSave EverySet.empty (savedWeight 12)
                        ]
                        |> expectValueStored (WeightSaved (Just measurementId) (WeightInKg 13)) SkippedWeight
            ]
        ]


{-| What a Save asks the app to do, in terms the criteria use.
-}
type Effect
    = BackToEncounter
    | HeightSaved (Maybe NutritionHeightId) HeightInCm
    | NotTakenAdded SkippedForm
    | NotTakenRemoved SkippedForm
    | OtherEffect
    | WeightSaved (Maybe NutritionWeightId) WeightInKg


toEffect : App.Model.Msg -> Effect
toEffect msg =
    case msg of
        App.Model.MsgIndexedDb (Backend.Model.MsgNutritionEncounter encounterId_ encounterMsg) ->
            if encounterId_ /= encounterId then
                OtherEffect

            else
                case encounterMsg of
                    Backend.NutritionEncounter.Model.AddSkippedForm form ->
                        NotTakenAdded form

                    Backend.NutritionEncounter.Model.RemoveSkippedForm form ->
                        NotTakenRemoved form

                    Backend.NutritionEncounter.Model.SaveHeight _ measurementId_ value ->
                        HeightSaved measurementId_ value

                    Backend.NutritionEncounter.Model.SaveWeight _ measurementId_ value ->
                        WeightSaved measurementId_ value

                    _ ->
                        OtherEffect

        App.Model.SetActivePage (UserPage (NutritionEncounterPage encounterId_)) ->
            if encounterId_ == encounterId then
                BackToEncounter

            else
                OtherEffect

        _ ->
            OtherEffect


{-| Runs the messages, in order, on a page opened fresh after a reload, and
collects everything they asked the app to do.
-}
runPage : Site -> List Msg -> List Effect
runPage site msgs =
    List.foldl
        (\msg ( model, effects ) ->
            let
                ( updatedModel, _, appMsgs ) =
                    update site encounterId emptyModelIndexedDb msg model
            in
            ( updatedModel, effects ++ List.map toEffect appMsgs )
        )
        ( emptyModel, [] )
        msgs
        |> Tuple.second


{-| Back on the encounter page, no value saved, and the measurement not taken
off the skipped set. Adding it to the set again is allowed: it is already
there.
-}
expectNotTaken : SkippedForm -> List Effect -> Expectation
expectNotTaken skippedForm =
    Expect.all
        [ expectBackToEncounter
        , List.filter isValueSaved
            >> Expect.equal []
            >> Expect.onFail "a value was saved"
        , List.member (NotTakenRemoved skippedForm)
            >> Expect.equal False
            >> Expect.onFail "the measurement was taken off the skipped set"
        ]


{-| As `expectNotTaken`, and the measurement is put on the skipped set.
-}
expectNewlyNotTaken : SkippedForm -> List Effect -> Expectation
expectNewlyNotTaken skippedForm =
    Expect.all
        [ expectNotTaken skippedForm
        , List.member (NotTakenAdded skippedForm)
            >> Expect.equal True
            >> Expect.onFail "the measurement was not put on the skipped set"
        ]


{-| Back on the encounter page with exactly this value saved, and the
measurement no longer on the skipped set.
-}
expectValueStored : Effect -> SkippedForm -> List Effect -> Expectation
expectValueStored saved skippedForm =
    Expect.all
        [ expectBackToEncounter
        , List.filter isValueSaved >> Expect.equal [ saved ]
        , List.member (NotTakenAdded skippedForm)
            >> Expect.equal False
            >> Expect.onFail "the measurement was put on the skipped set"
        , List.member (NotTakenRemoved skippedForm)
            >> Expect.equal True
            >> Expect.onFail "the measurement was left on the skipped set"
        ]


expectBackToEncounter : List Effect -> Expectation
expectBackToEncounter =
    List.member BackToEncounter
        >> Expect.equal True
        >> Expect.onFail "Save did not return to the encounter page"


isValueSaved : Effect -> Bool
isValueSaved effect =
    case effect of
        HeightSaved _ _ ->
            True

        WeightSaved _ _ ->
            True

        _ ->
            False


heightSave : EverySet SkippedForm -> Maybe ( NutritionHeightId, NutritionHeight ) -> Msg
heightSave skippedForms saved =
    PreSaveHeight skippedForms childId saved


weightSave : EverySet SkippedForm -> Maybe ( NutritionWeightId, NutritionWeight ) -> Msg
weightSave skippedForms saved =
    PreSaveWeight skippedForms childId saved


savedHeight : Float -> Maybe ( NutritionHeightId, NutritionHeight )
savedHeight value =
    wrapMeasurement measuredOn (HeightInCm value)


savedWeight : Float -> Maybe ( NutritionWeightId, NutritionWeight )
savedWeight value =
    wrapMeasurement measuredOn (WeightInKg value)


heightSkipped : EverySet SkippedForm
heightSkipped =
    EverySet.singleton SkippedHeight


weightSkipped : EverySet SkippedForm
weightSkipped =
    EverySet.singleton SkippedWeight


bothSkipped : EverySet SkippedForm
bothSkipped =
    EverySet.fromList [ SkippedHeight, SkippedWeight ]


{-| The id `wrapMeasurement` gives a saved measurement.
-}
measurementId : EntityUuid a
measurementId =
    toEntityUuid "dummy-id"


encounterId : NutritionEncounterId
encounterId =
    toEntityUuid "encounter"


childId : PersonId
childId =
    toEntityUuid "person"


measuredOn : NominalDate
measuredOn =
    Date.fromCalendarDate 2026 Nov 10
