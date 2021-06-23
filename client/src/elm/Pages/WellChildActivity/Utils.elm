module Pages.WellChildActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (WellChildEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, ifTrue, taskCompleted)
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
expectActivity currentDate child data db activity =
    -- For now, we show all activities without any conditions.
    case activity of
        WellChildNutritionAssessment ->
            True

        WellChildECD ->
            True


activityCompleted : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> WellChildActivity -> Bool
activityCompleted currentDate child data db activity =
    let
        measurements =
            data.measurements

        activityExpected =
            expectActivity currentDate child data db
    in
    case activity of
        WellChildNutritionAssessment ->
            (not <| activityExpected WellChildNutritionAssessment) || False

        WellChildECD ->
            (not <| activityExpected WellChildECD) || isJust measurements.ecd


fromWellChildECDValue : Maybe (EverySet ECDSign) -> WellChildECDForm
fromWellChildECDValue signs =
    { respontToSoundWithSound = Maybe.map (EverySet.member RespontToSoundWithSound) signs
    , turnHeadWhenCalled = Maybe.map (EverySet.member TurnHeadWhenCalled) signs
    , sitWithoutSupport = Maybe.map (EverySet.member SitWithoutSupport) signs
    , smileBack = Maybe.map (EverySet.member SmileBack) signs
    , rollTummyToBack = Maybe.map (EverySet.member RollTummyToBack) signs
    , reachForToys = Maybe.map (EverySet.member ReachForToys) signs
    , useSimpleGestures = Maybe.map (EverySet.member UseSimpleGestures) signs
    , standOnTheirOwn = Maybe.map (EverySet.member StandOnTheirOwn) signs
    , copyDuringPlay = Maybe.map (EverySet.member CopyDuringPlay) signs
    , sayMamaDada = Maybe.map (EverySet.member SayMamaDada) signs
    , canHoldSmallObjects = Maybe.map (EverySet.member CanHoldSmallObjects) signs
    , looksWhenPointedAt = Maybe.map (EverySet.member LooksWhenPointedAt) signs
    , useSingleWords = Maybe.map (EverySet.member UseSingleWords) signs
    , walkWithoutHelp = Maybe.map (EverySet.member WalkWithoutHelp) signs
    , playPretend = Maybe.map (EverySet.member PlayPretend) signs
    , pointToThingsOfInterest = Maybe.map (EverySet.member PointToThingsOfInterest) signs
    , useShortPhrases = Maybe.map (EverySet.member UseShortPhrases) signs
    , interestedInOtherChildren = Maybe.map (EverySet.member InterestedInOtherChildren) signs
    , followSimlpeInstructions = Maybe.map (EverySet.member FollowSimpleInstructions) signs
    , kickBall = Maybe.map (EverySet.member KickBall) signs
    , pointAtNamedObjects = Maybe.map (EverySet.member PointAtNamedObjects) signs
    , dressThemselves = Maybe.map (EverySet.member DressThemselves) signs
    , washHandsGoToToiled = Maybe.map (EverySet.member WashHandsGoToToiled) signs
    , knowsColorsAndNumbers = Maybe.map (EverySet.member KnowsColorsAndNumbers) signs
    , useMediumPhrases = Maybe.map (EverySet.member UseMediumPhrases) signs
    , playMakeBelieve = Maybe.map (EverySet.member PlayMakeBelieve) signs
    , followThreeStepInstructions = Maybe.map (EverySet.member FollowThreeStepInstructions) signs
    , standOnOneFootFiveSeconds = Maybe.map (EverySet.member StandOnOneFootFiveSeconds) signs
    , useLongPhrases = Maybe.map (EverySet.member UseLongPhrases) signs
    , shareWithOtherChildren = Maybe.map (EverySet.member ShareWithOtherChildren) signs
    , countToTen = Maybe.map (EverySet.member CountToTen) signs
    }


wellChildECDFormWithDefault : WellChildECDForm -> Maybe (EverySet ECDSign) -> WellChildECDForm
wellChildECDFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\signs ->
                { respontToSoundWithSound = or form.respontToSoundWithSound (EverySet.member RespontToSoundWithSound signs |> Just)
                , turnHeadWhenCalled = or form.turnHeadWhenCalled (EverySet.member TurnHeadWhenCalled signs |> Just)
                , sitWithoutSupport = or form.sitWithoutSupport (EverySet.member SitWithoutSupport signs |> Just)
                , smileBack = or form.smileBack (EverySet.member SmileBack signs |> Just)
                , rollTummyToBack = or form.rollTummyToBack (EverySet.member RollTummyToBack signs |> Just)
                , reachForToys = or form.reachForToys (EverySet.member ReachForToys signs |> Just)
                , useSimpleGestures = or form.useSimpleGestures (EverySet.member UseSimpleGestures signs |> Just)
                , standOnTheirOwn = or form.standOnTheirOwn (EverySet.member StandOnTheirOwn signs |> Just)
                , copyDuringPlay = or form.copyDuringPlay (EverySet.member CopyDuringPlay signs |> Just)
                , sayMamaDada = or form.sayMamaDada (EverySet.member SayMamaDada signs |> Just)
                , canHoldSmallObjects = or form.canHoldSmallObjects (EverySet.member CanHoldSmallObjects signs |> Just)
                , looksWhenPointedAt = or form.looksWhenPointedAt (EverySet.member LooksWhenPointedAt signs |> Just)
                , useSingleWords = or form.useSingleWords (EverySet.member UseSingleWords signs |> Just)
                , walkWithoutHelp = or form.walkWithoutHelp (EverySet.member WalkWithoutHelp signs |> Just)
                , playPretend = or form.playPretend (EverySet.member PlayPretend signs |> Just)
                , pointToThingsOfInterest = or form.pointToThingsOfInterest (EverySet.member PointToThingsOfInterest signs |> Just)
                , useShortPhrases = or form.useShortPhrases (EverySet.member UseShortPhrases signs |> Just)
                , interestedInOtherChildren = or form.interestedInOtherChildren (EverySet.member InterestedInOtherChildren signs |> Just)
                , followSimlpeInstructions = or form.followSimlpeInstructions (EverySet.member FollowSimpleInstructions signs |> Just)
                , kickBall = or form.kickBall (EverySet.member KickBall signs |> Just)
                , pointAtNamedObjects = or form.pointAtNamedObjects (EverySet.member PointAtNamedObjects signs |> Just)
                , dressThemselves = or form.dressThemselves (EverySet.member DressThemselves signs |> Just)
                , washHandsGoToToiled = or form.washHandsGoToToiled (EverySet.member WashHandsGoToToiled signs |> Just)
                , knowsColorsAndNumbers = or form.knowsColorsAndNumbers (EverySet.member KnowsColorsAndNumbers signs |> Just)
                , useMediumPhrases = or form.useMediumPhrases (EverySet.member UseMediumPhrases signs |> Just)
                , playMakeBelieve = or form.playMakeBelieve (EverySet.member PlayMakeBelieve signs |> Just)
                , followThreeStepInstructions = or form.followThreeStepInstructions (EverySet.member FollowThreeStepInstructions signs |> Just)
                , standOnOneFootFiveSeconds = or form.standOnOneFootFiveSeconds (EverySet.member StandOnOneFootFiveSeconds signs |> Just)
                , useLongPhrases = or form.useLongPhrases (EverySet.member UseLongPhrases signs |> Just)
                , shareWithOtherChildren = or form.shareWithOtherChildren (EverySet.member ShareWithOtherChildren signs |> Just)
                , countToTen = or form.countToTen (EverySet.member CountToTen signs |> Just)
                }
            )


toWellChildECDValueWithDefault : Maybe (EverySet ECDSign) -> WellChildECDForm -> Maybe (EverySet ECDSign)
toWellChildECDValueWithDefault saved form =
    wellChildECDFormWithDefault form saved
        |> toWellChildECDValue


toWellChildECDValue : WellChildECDForm -> Maybe (EverySet ECDSign)
toWellChildECDValue form =
    [ ifNullableTrue RespontToSoundWithSound form.respontToSoundWithSound
    , ifNullableTrue TurnHeadWhenCalled form.turnHeadWhenCalled
    , ifNullableTrue SitWithoutSupport form.sitWithoutSupport
    , ifNullableTrue SmileBack form.smileBack
    , ifNullableTrue RollTummyToBack form.rollTummyToBack
    , ifNullableTrue UseSimpleGestures form.useSimpleGestures
    , ifNullableTrue StandOnTheirOwn form.standOnTheirOwn
    , ifNullableTrue CopyDuringPlay form.copyDuringPlay
    , ifNullableTrue SayMamaDada form.sayMamaDada
    , ifNullableTrue CanHoldSmallObjects form.canHoldSmallObjects
    , ifNullableTrue LooksWhenPointedAt form.looksWhenPointedAt
    , ifNullableTrue UseSingleWords form.useSingleWords
    , ifNullableTrue WalkWithoutHelp form.walkWithoutHelp
    , ifNullableTrue PlayPretend form.playPretend
    , ifNullableTrue PointToThingsOfInterest form.pointToThingsOfInterest
    , ifNullableTrue UseShortPhrases form.useShortPhrases
    , ifNullableTrue InterestedInOtherChildren form.interestedInOtherChildren
    , ifNullableTrue FollowSimpleInstructions form.followSimlpeInstructions
    , ifNullableTrue KickBall form.kickBall
    , ifNullableTrue PointAtNamedObjects form.pointAtNamedObjects
    , ifNullableTrue DressThemselves form.dressThemselves
    , ifNullableTrue WashHandsGoToToiled form.washHandsGoToToiled
    , ifNullableTrue KnowsColorsAndNumbers form.knowsColorsAndNumbers
    , ifNullableTrue UseMediumPhrases form.useMediumPhrases
    , ifNullableTrue PlayMakeBelieve form.playMakeBelieve
    , ifNullableTrue FollowThreeStepInstructions form.followThreeStepInstructions
    , ifNullableTrue StandOnOneFootFiveSeconds form.standOnOneFootFiveSeconds
    , ifNullableTrue UseLongPhrases form.useLongPhrases
    , ifNullableTrue ShareWithOtherChildren form.shareWithOtherChildren
    , ifNullableTrue CountToTen form.countToTen
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoECDSigns)
