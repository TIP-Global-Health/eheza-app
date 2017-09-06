module Examination.Utils exposing (getLastExaminationFromChild, mapExaminationChild, mapExaminationMother)

import Child.Model exposing (Child)
import Examination.Model exposing (ExaminationChild, ExaminationMother, Examination(..), emptyExaminationChild, emptyExaminationMother)


{-| For now, we're just returning a mocked version. Eventually
we'll need to do something different, once we can have more than
one examination.
-}
getLastExaminationFromChild : Child -> Maybe ExaminationChild
getLastExaminationFromChild child =
    Just
        { height = Just 50.0
        , muac = Just 13.0
        , photo = Nothing
        , weight = Just 4.0
        }


{-| This is a convenience for cases in which we've got an `Examination` and
we want to modify it if it is an `ExaminationChild` and leave it alone
otherwise. Using this probably indicates that we could do our data modelling a
bit better, but prhaps not.
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
