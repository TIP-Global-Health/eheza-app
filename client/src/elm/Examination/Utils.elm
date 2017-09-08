module Examination.Utils exposing (getLastExaminationFromChild, mapExaminationChild, mapExaminationMother, supplyMeasurement)

import Child.Model exposing (Child)
import Examination.Model exposing (ExaminationChild, ExaminationMother, Examination(..), emptyExaminationChild, emptyExaminationMother)
import StorageKey exposing (StorageKey(..))


{-| For now, we're just returning a mocked version. Eventually
we'll need to do something different, once we can have more than
one examination. (To make things easy for now, we show the previous
examination as being unsaved, which I suppose is possible).
-}
getLastExaminationFromChild : Child -> Maybe ExaminationChild
getLastExaminationFromChild child =
    Just
        { height = Just ( New, 50.0 )
        , muac = Just ( New, 13.0 )
        , photo = Nothing
        , weight = Just ( New, 4.0 )
        }


{-| This is a convenience for cases in which we've got an `Examination` and
we want to modify it if it is an `ExaminationChild` and leave it alone
otherwise. Using this probably indicates that we could do our data modelling a
bit better, but perhaps not.
-}
mapExaminationChild : (ExaminationChild -> ExaminationChild) -> Examination -> Examination
mapExaminationChild func examination =
    case examination of
        ChildExamination examinationChild ->
            ChildExamination (func examinationChild)

        MotherExamination _ ->
            examination


{-| Like mapExaminationChild, but for mothers.
-}
mapExaminationMother : (ExaminationMother -> ExaminationMother) -> Examination -> Examination
mapExaminationMother func examination =
    case examination of
        MotherExamination examinationMother ->
            MotherExamination (func examinationMother)

        ChildExamination _ ->
            examination


{-| Our measurements are typed as

    Maybe (StorageKey key, value )

Now, when the user actually edits a value, we want to do
a couple of things to this, depending the initial state.
So, this is a helper for that.
-}
supplyMeasurement : v -> Maybe ( StorageKey k, v ) -> Maybe ( StorageKey k, v )
supplyMeasurement value storage =
    case storage of
        Nothing ->
            -- If it was `Nothing`, then we hadn't entered or retrieved
            -- anything at all. So, logically, now it is a 'new' value.
            Just ( New, value )

        Just ( New, _ ) ->
            -- Otherwise, we can just replace the value and keep the key
            Just ( New, value )

        Just ( Existing key, _ ) ->
            Just ( Existing key, value )
