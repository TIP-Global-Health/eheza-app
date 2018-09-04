module Measurement.Model exposing (..)

{-| These modules manage the UI for the various measurements relating to a
participant.
-}

import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)


{-| The strategy here, at least for now, is this:

  - The values in the `Model` here reflect what is entered in the UI. So, they
    are updated on every key-press etc.

  - The `update` function takes a parameter which represents the actual data.
    It updates that parameter only when the value has actually been saved.

So, basically we're doing pure UI here ... all other concerns are handled via
the `OutMsg` returned to the caller, which the caller is expected to do something
useful with.

This means that we need to be able to initialize our UI state here from some
backend state in order to perform an edit -- it's the caller's job to handle
that.

Ideally, we'll eventually use `Restful.RestfulData` to track underlying
data, UI edits, validation, and update status all in one type. If we had that,
we'd wouldn't really need our own model here (and we'd avoid some synchronization
issues) since the data itself would encapsulate an editor state.

-}
type alias ModelChild =
    { height : String
    , muac : String
    , nutritionSigns : EverySet ChildNutritionSign
    , photo : Maybe PhotoValue
    , weight : String
    , counseling : Maybe ( CounselingTiming, EverySet CounselingTopicId )
    }


type alias ModelMother =
    { familyPlanningSigns : EverySet FamilyPlanningSign
    }


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FileId =
    Int


{-| Represents the "file" that DropZone gives us when
the upload is complete. There are several things we
could get from this ... for now, just the location.
-}
type alias DropZoneFile =
    { url : String
    }


type MsgChild
    = SelectNutritionSign Bool ChildNutritionSign
    | SelectCounselingTopic Bool CounselingTopicId
    | SendOutMsgChild OutMsgChild
    | UpdateHeight String
    | UpdateMuac String
    | UpdateWeight String
    | DropZoneComplete DropZoneFile


type MsgMother
    = SelectFamilyPlanningSign Bool FamilyPlanningSign
    | SendOutMsgMother OutMsgMother


{-| This is sort of the "opposite" of `Msg`. Instead of representing messages
which we can handle, it represents messages we **can't** handle, and would
like the caller to take care of.
-}
type OutMsgChild
    = SaveHeight HeightInCm
    | SaveWeight WeightInKg
    | SaveMuac MuacInCm
    | SaveCounselingSession CounselingTiming (EverySet CounselingTopicId)
    | SaveChildNutritionSigns (EverySet ChildNutritionSign)
    | SavePhoto PhotoValue


type OutMsgMother
    = SaveFamilyPlanningSigns (EverySet FamilyPlanningSign)


emptyModelChild : ModelChild
emptyModelChild =
    { height = ""
    , muac = ""
    , nutritionSigns = EverySet.empty
    , photo = Nothing
    , weight = ""
    , counseling = Nothing
    }


emptyModelMother : ModelMother
emptyModelMother =
    { familyPlanningSigns = EverySet.empty
    }
