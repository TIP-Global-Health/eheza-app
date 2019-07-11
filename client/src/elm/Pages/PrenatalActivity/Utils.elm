module Pages.PrenatalActivity.Utils exposing (fromBreastExamValue, fromCorePhysicalExamValue, fromDangerSignsValue, fromFamilyPlanningValue, fromLastMenstrualPeriodValue, fromMedicalHistoryValue, fromMedicationValue, fromObstetricHistoryValue, fromObstetricalExamValue, fromPrenatalNutritionValue, fromResourceValue, fromSocialHistoryValue, fromVitalsValue, ifEmpty, ifTrue, lastMenstrualPeriodFormWithDefaultValue, toBreastExamValue, toCorePhysicalExamValue, toDangerSignsValue, toEverySet, toFamilyPlanningValue, toLastMenstrualPeriodValue, toLastMenstrualPeriodValueWithDefault, toMedicalHistoryValue, toMedicationValue, toObstetricHistoryValue, toObstetricalExamValue, toPrenatalNutritionValue, toResourceValue, toSocialHistoryValue, toVitalsValue)

import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Maybe.Extra exposing (andMap, or, unwrap)
import Pages.PrenatalActivity.Model exposing (..)


{-| This is a convenience for cases where the form values ought to be redefined
to allow multiple values. So, it should go away eventually.
-}
toEverySet : a -> a -> Bool -> EverySet a
toEverySet presentValue absentValue present =
    if present then
        EverySet.singleton presentValue

    else
        EverySet.singleton absentValue


ifTrue : a -> Bool -> EverySet a
ifTrue value condition =
    if condition then
        EverySet.singleton value

    else
        EverySet.empty


ifEmpty : a -> EverySet a -> EverySet a
ifEmpty value set =
    if EverySet.isEmpty set then
        EverySet.singleton value

    else
        set


fromBreastExamValue : Maybe BreastExamValue -> BreastExamForm
fromBreastExamValue saved =
    -- The `List.head` is temporary, until BreastExamForm is redefined
    -- to allow more than one.
    { breast = Maybe.andThen (.exam >> EverySet.toList >> List.head) saved
    , selfGuidance = Maybe.map .selfGuidance saved
    }


toBreastExamValue : BreastExamForm -> Maybe BreastExamValue
toBreastExamValue form =
    -- The `EverySet.singleton` is temporary, until BresatExamForm is
    -- redefined to allow more than one.
    Maybe.map BreastExamValue (Maybe.map EverySet.singleton form.breast)
        |> andMap form.selfGuidance


fromCorePhysicalExamValue : Maybe CorePhysicalExamValue -> CorePhysicalExamForm
fromCorePhysicalExamValue saved =
    -- Most of this is temporary, until CorePhysicalExamForm is redefined
    { brittleHair = Maybe.map (.hairHead >> EverySet.member BrittleHairCPE) saved
    , paleConjuctiva = Maybe.map (.eyes >> EverySet.member PaleConjuctiva) saved
    , neck = Maybe.andThen (.neck >> EverySet.toList >> List.head) saved
    , abnormalHeart = Maybe.map (.heart >> EverySet.member AbnormalHeart) saved
    , lungs = Maybe.andThen (.lungs >> EverySet.toList >> List.head) saved
    , abdomen = Maybe.andThen (.abdomen >> EverySet.toList >> List.head) saved
    , hands = Maybe.andThen (.hands >> EverySet.toList >> List.head) saved
    , legs = Maybe.andThen (.legs >> EverySet.toList >> List.head) saved
    }


toCorePhysicalExamValue : CorePhysicalExamForm -> Maybe CorePhysicalExamValue
toCorePhysicalExamValue form =
    -- Also, termporary things here, until CorePhysicalExamForm is redefined
    Maybe.map CorePhysicalExamValue (Maybe.map (toEverySet BrittleHairCPE NormalHairHead) form.brittleHair)
        |> andMap (Maybe.map (toEverySet PaleConjuctiva NormalEyes) form.paleConjuctiva)
        |> andMap (Maybe.map (toEverySet AbnormalHeart NormalHeart) form.abnormalHeart)
        |> andMap (Maybe.map EverySet.singleton form.neck)
        |> andMap (Maybe.map EverySet.singleton form.lungs)
        |> andMap (Maybe.map EverySet.singleton form.abdomen)
        |> andMap (Maybe.map EverySet.singleton form.hands)
        |> andMap (Maybe.map EverySet.singleton form.legs)


fromDangerSignsValue : Maybe (EverySet DangerSign) -> DangerSignsForm
fromDangerSignsValue saved =
    { signs = Maybe.map EverySet.toList saved
    }


toDangerSignsValue : DangerSignsForm -> Maybe (EverySet DangerSign)
toDangerSignsValue form =
    Maybe.map EverySet.fromList form.signs


fromLastMenstrualPeriodValue : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
fromLastMenstrualPeriodValue saved =
    { lmpRange = Nothing
    , lmpDate = Maybe.map (.date >> (\date -> toLocalDateTime date 12 0 0 0)) saved
    , lmpDateConfident = Maybe.map .confident saved
    , isDateSelectorOpen = False
    }


lastMenstrualPeriodFormWithDefaultValue : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
lastMenstrualPeriodFormWithDefaultValue form saved =
    saved
        |> unwrap
            form
            (\value ->
                { lmpRange = or form.lmpRange (Just SixMonth)
                , lmpDate = or form.lmpDate (Just (toLocalDateTime value.date 12 0 0 0))
                , lmpDateConfident = or form.lmpDateConfident (Just value.confident)
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toLastMenstrualPeriodValueWithDefault : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValueWithDefault saved form =
    let
        form_ =
            lastMenstrualPeriodFormWithDefaultValue form saved
    in
    toLastMenstrualPeriodValue form_


toLastMenstrualPeriodValue : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValue form =
    Maybe.map LastMenstrualPeriodValue (Maybe.map fromLocalDateTime form.lmpDate)
        |> andMap form.lmpDateConfident


fromMedicalHistoryValue : Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm
fromMedicalHistoryValue saved =
    { uterineMyoma = Maybe.map (EverySet.member UterineMyoma) saved
    , diabetes = Maybe.map (EverySet.member Diabetes) saved
    , cardiacDisease = Maybe.map (EverySet.member CardiacDisease) saved
    , renalDisease = Maybe.map (EverySet.member RenalDisease) saved
    , hypertensionBeforePregnancy = Maybe.map (EverySet.member HypertensionBeforePregnancy) saved
    , tuberculosisPast = Maybe.map (EverySet.member TuberculosisPast) saved
    , tuberculosisPresent = Maybe.map (EverySet.member TuberculosisPresent) saved
    , asthma = Maybe.map (EverySet.member Asthma) saved
    , bowedLegs = Maybe.map (EverySet.member BowedLegs) saved
    , hiv = Maybe.map (EverySet.member HIV) saved
    }


toMedicalHistoryValue : MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign)
toMedicalHistoryValue form =
    [ Maybe.map (ifTrue UterineMyoma) form.uterineMyoma
    , Maybe.map (ifTrue Diabetes) form.diabetes
    , Maybe.map (ifTrue CardiacDisease) form.cardiacDisease
    , Maybe.map (ifTrue HypertensionBeforePregnancy) form.hypertensionBeforePregnancy
    , Maybe.map (ifTrue TuberculosisPast) form.tuberculosisPast
    , Maybe.map (ifTrue TuberculosisPresent) form.tuberculosisPresent
    , Maybe.map (ifTrue Asthma) form.asthma
    , Maybe.map (ifTrue BowedLegs) form.bowedLegs
    , Maybe.map (ifTrue HIV) form.hiv
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoMedicalHistorySigns)


fromMedicationValue : Maybe (EverySet MedicationSign) -> MedicationForm
fromMedicationValue saved =
    { receivedIronFolicAcid = Maybe.map (EverySet.member IronAndFolicAcidSupplement) saved
    , receivedDewormingPill = Maybe.map (EverySet.member DewormingPill) saved
    }


toMedicationValue : MedicationForm -> Maybe (EverySet MedicationSign)
toMedicationValue form =
    [ Maybe.map (ifTrue IronAndFolicAcidSupplement) form.receivedIronFolicAcid
    , Maybe.map (ifTrue DewormingPill) form.receivedDewormingPill
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoMedication)


fromObstetricalExamValue : Maybe ObstetricalExamValue -> ObstetricalExamForm
fromObstetricalExamValue saved =
    { fundalHeight = Maybe.map (.fundalHeight >> (\(HeightInCm cm) -> cm)) saved
    , fetalPresentation = Maybe.map .fetalPresentation saved
    , fetalMovement = Maybe.map .fetalMovement saved
    , fetalHeartRate = Maybe.map .fetalHeartRate saved
    , cSectionScar = Maybe.map .cSectionScar saved
    }


toObstetricalExamValue : ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValue form =
    Maybe.map ObstetricalExamValue (Maybe.map HeightInCm form.fundalHeight)
        |> andMap form.fetalPresentation
        |> andMap form.fetalMovement
        |> andMap form.fetalHeartRate
        |> andMap form.cSectionScar


fromObstetricHistoryValue : Maybe ObstetricHistoryValue -> ObstetricFormFirstStep
fromObstetricHistoryValue saved =
    { currentlyPregnant = Maybe.map .currentlyPregnant saved
    , termPregnancy = Maybe.map .termPregnancy saved
    , preTermPregnancy = Maybe.map .pretermPregnancy saved
    , stillbirthsAtTerm = Maybe.map .stillBirthsAtTerm saved
    , stillbirthsPreTerm = Maybe.map .stillBirthsPreTerm saved
    , abortions = Maybe.map .abortions saved
    , liveChildren = Maybe.map .liveChildren saved
    }


toObstetricHistoryValue : ObstetricFormFirstStep -> Maybe ObstetricHistoryValue
toObstetricHistoryValue form =
    Maybe.map ObstetricHistoryValue form.currentlyPregnant
        |> andMap form.termPregnancy
        |> andMap form.preTermPregnancy
        |> andMap form.stillbirthsAtTerm
        |> andMap form.stillbirthsPreTerm
        |> andMap form.abortions
        |> andMap form.liveChildren


fromFamilyPlanningValue : Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm
fromFamilyPlanningValue saved =
    { signs = Maybe.map EverySet.toList saved }


toFamilyPlanningValue : FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign)
toFamilyPlanningValue form =
    Maybe.map (EverySet.fromList >> ifEmpty NoFamilyPlanning) form.signs


fromPrenatalNutritionValue : Maybe PrenatalNutritionValue -> NutritionAssessmentForm
fromPrenatalNutritionValue saved =
    { height = Maybe.map (.height >> (\(HeightInCm cm) -> cm)) saved
    , weight = Maybe.map (.weight >> (\(WeightInKg kg) -> kg)) saved
    , muac = Maybe.map (.muac >> (\(MuacInCm cm) -> cm)) saved
    }


toPrenatalNutritionValue : NutritionAssessmentForm -> Maybe PrenatalNutritionValue
toPrenatalNutritionValue form =
    Maybe.map PrenatalNutritionValue (Maybe.map HeightInCm form.height)
        |> andMap (Maybe.map WeightInKg form.weight)
        |> andMap (Maybe.map MuacInCm form.muac)


fromResourceValue : Maybe (EverySet ResourceSign) -> ResourcesForm
fromResourceValue saved =
    { receivedMosquitoNet = Maybe.map (EverySet.member MosquitoNet) saved
    }


toResourceValue : ResourcesForm -> Maybe (EverySet ResourceSign)
toResourceValue form =
    Maybe.map (toEverySet MosquitoNet NoResource) form.receivedMosquitoNet


fromSocialHistoryValue : Maybe (EverySet SocialHistorySign) -> SocialHistoryForm
fromSocialHistoryValue saved =
    { accompaniedByPartner = Maybe.map (EverySet.member AccompaniedByPartner) saved
    , partnerReceivedCounseling = Maybe.map (EverySet.member PartnerHivCounseling) saved
    , mentalHealthHistory = Maybe.map (EverySet.member MentalHealthHistory) saved
    }


toSocialHistoryValue : SocialHistoryForm -> Maybe (EverySet SocialHistorySign)
toSocialHistoryValue form =
    [ Maybe.map (ifTrue AccompaniedByPartner) form.accompaniedByPartner
    , Maybe.map (ifTrue PartnerHivCounseling) form.partnerReceivedCounseling
    , Maybe.map (ifTrue MentalHealthHistory) form.mentalHealthHistory
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoSocialHistorySign)


fromVitalsValue : Maybe VitalsValue -> VitalsForm
fromVitalsValue saved =
    { sysBloodPressure = Maybe.map .sys saved
    , diaBloodPressure = Maybe.map .dia saved
    , heartRate = Maybe.map .heartRate saved
    , respiratoryRate = Maybe.map .respiratoryRate saved
    , bodyTemperature = Maybe.map .bodyTemperature saved
    }


toVitalsValue : VitalsForm -> Maybe VitalsValue
toVitalsValue form =
    Maybe.map VitalsValue form.sysBloodPressure
        |> andMap form.diaBloodPressure
        |> andMap form.heartRate
        |> andMap form.respiratoryRate
        |> andMap form.bodyTemperature
