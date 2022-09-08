module Pages.NCD.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
import Backend.NCDEncounter.Utils exposing (getNCDEncountersForParticipant)
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, fromLocalDateTime)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Pages.NCD.Model exposing (..)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, translate)


generateAssembledData : NCDEncounterId -> ModelIndexedDb -> WebData AssembledData
generateAssembledData id db =
    let
        encounter =
            Dict.get id db.ncdEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.ncdMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            RemoteData.toMaybe encounter
                |> Maybe.map (\encounter_ -> generatePreviousMeasurements (Just id) encounter_.participant db)
                |> Maybe.withDefault []
    in
    RemoteData.map AssembledData (Success id)
        |> RemoteData.andMap encounter
        |> RemoteData.andMap participant
        |> RemoteData.andMap person
        |> RemoteData.andMap measurements
        |> RemoteData.andMap (Success previousMeasurementsWithDates)


generatePreviousMeasurements : Maybe NCDEncounterId -> IndividualEncounterParticipantId -> ModelIndexedDb -> List ( NominalDate, ( NCDEncounterId, NCDMeasurements ) )
generatePreviousMeasurements currentEncounterId participantId db =
    getNCDEncountersForParticipant db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- If the ID of current encounter was provided,
                -- we do not want to get its data.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId db.ncdMeasurements of
                        Just (Success data) ->
                            Just ( encounter.startDate, ( encounterId, data ) )

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortTuplesByDateDesc


referralFormWithDefault : ReferralForm -> Maybe ReferralValue -> ReferralForm
referralFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    resolveFromFacilitySignsValue sign =
                        EverySet.member sign value.referralSigns |> Just
                in
                { referToHospital = or form.referToHospital (resolveFromFacilitySignsValue ReferToHospital)
                , referralFormHospital = or form.referralFormHospital (resolveFromFacilitySignsValue ReferralFormHospital)
                , referToANCServices = or form.referToANCServices (resolveFromFacilitySignsValue ReferToANCServices)
                , referralFormANCServices = or form.referralFormANCServices (resolveFromFacilitySignsValue ReferralFormANCServices)
                , accompanyToANCServices = or form.accompanyToANCServices (resolveFromFacilitySignsValue AccompanyToANCServices)
                , nonReferralReasons = or form.nonReferralReasons value.nonReferralReasons
                }
            )


toReferralValueWithDefault : Maybe ReferralValue -> ReferralForm -> Maybe ReferralValue
toReferralValueWithDefault saved form =
    referralFormWithDefault form saved
        |> toReferralValue


toReferralValue : ReferralForm -> Maybe ReferralValue
toReferralValue form =
    let
        referralSigns =
            [ ifNullableTrue ReferToHospital form.referToHospital
            , ifNullableTrue ReferralFormHospital form.referralFormHospital
            , ifNullableTrue ReferToANCServices form.referToANCServices
            , ifNullableTrue ReferralFormANCServices form.referralFormANCServices
            , ifNullableTrue AccompanyToANCServices form.accompanyToANCServices
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoReferToFacilitySigns)
    in
    Maybe.map
        (\signs ->
            { referralSigns = signs
            , nonReferralReasons = form.nonReferralReasons
            }
        )
        referralSigns


toMedicationDistributionValueWithDefault :
    Maybe NCDMedicationDistributionValue
    -> MedicationDistributionForm
    -> Maybe NCDMedicationDistributionValue
toMedicationDistributionValueWithDefault saved form =
    medicationDistributionFormWithDefault form saved
        |> toMedicationDistributionValue


medicationDistributionFormWithDefault : MedicationDistributionForm -> Maybe NCDMedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { recommendedTreatmentSigns = or form.recommendedTreatmentSigns (EverySet.toList value.recommendedTreatmentSigns |> Just)
                , guidedToReturnInOneMonth = or form.guidedToReturnInOneMonth (EverySet.member ReturnInOneMonth value.guidanceSigns |> Just)
                }
            )


toMedicationDistributionValue : MedicationDistributionForm -> Maybe NCDMedicationDistributionValue
toMedicationDistributionValue form =
    let
        recommendedTreatmentSigns =
            Maybe.map EverySet.fromList form.recommendedTreatmentSigns

        guidanceSigns =
            [ ifNullableTrue ReturnInOneMonth form.guidedToReturnInOneMonth ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDGuidanceSigns)
    in
    Maybe.map NCDMedicationDistributionValue recommendedTreatmentSigns
        |> andMap guidanceSigns



--
-- resolveReferralToFacilityInputsAndTasks :
--     Language
--     -> NominalDate
--     -> PrenatalEncounterPhase
--     -> AssembledData
--     -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
--     -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
--     -> ReferralForm
--     -> ReferralFacility
--     -> ( List (Html msg), List (Maybe Bool) )
-- resolveReferralToFacilityInputsAndTasks language currentDate phase assembled setReferralBoolInputMsg setNonReferralReasonMsg form facility =
--     let
--         maybeConfig =
--             case facility of
--                 FacilityHospital ->
--                     let
--                         referralReasons =
--                             diagnosesCausingHospitalReferralByPhase phase assembled
--                                 |> EverySet.toList
--
--                         referralContext =
--                             if not <| List.isEmpty referralReasons then
--                                 let
--                                     diagnosisTransId diagnosis =
--                                         if diagnosis == DiagnosisChronicHypertensionImmediate then
--                                             Translate.Hypertension
--
--                                         else
--                                             Translate.PrenatalDiagnosis diagnosis
--
--                                     reasons =
--                                         List.map (diagnosisTransId >> translate language) referralReasons
--                                             |> String.join ", "
--                                 in
--                                 div [ class "label" ] [ text <| translate language Translate.PatientDiagnosedWithLabel ++ ": " ++ reasons ++ "." ]
--
--                             else
--                                 emptyNode
--                     in
--                     Just
--                         { header =
--                             [ referralContext
--                             , viewCustomLabel language Translate.HighRiskCaseHelper "." "instructions"
--                             ]
--                         , referralField = form.referToHospital
--                         , referralUpdateFunc =
--                             \value form_ ->
--                                 { form_
--                                     | referToHospital = Just value
--                                     , referralFormHospital = Nothing
--                                 }
--                         , formField = form.referralFormHospital
--                         , formUpdateFunc = \value form_ -> { form_ | referralFormHospital = Just value }
--                         , accompanyConfig = Nothing
--                         , reasonToSignFunc = NonReferralReasonHospital
--                         }
--
--                 FacilityMentalHealthSpecialist ->
--                     Just
--                         { header = [ viewCustomLabel language Translate.PrenatalMentalHealthSpecialistHelper "." "instructions" ]
--                         , referralField = form.referToMentalHealthSpecialist
--                         , referralUpdateFunc =
--                             \value form_ ->
--                                 { form_
--                                     | referToMentalHealthSpecialist = Just value
--                                     , referralFormMentalHealthSpecialist = Nothing
--                                     , accompanyToMentalHealthSpecialist = Nothing
--                                 }
--                         , formField = form.referralFormMentalHealthSpecialist
--                         , formUpdateFunc = \value form_ -> { form_ | referralFormMentalHealthSpecialist = Just value }
--                         , accompanyConfig =
--                             Just
--                                 ( form.accompanyToMentalHealthSpecialist
--                                 , \value form_ -> { form_ | accompanyToMentalHealthSpecialist = Just value }
--                                 )
--                         , reasonToSignFunc = NonReferralReasonMentalHealthSpecialist
--                         }
--
--                 FacilityARVProgram ->
--                     Just
--                         { header =
--                             let
--                                 forPostpartum =
--                                     assembled.encounter.encounterType == NursePostpartumEncounter
--                             in
--                             if forPostpartum then
--                                 [ viewCustomLabel language Translate.PrenatalARVProgramPostpartumHeader "." "instructions"
--                                 , viewCustomLabel language (Translate.PrenatalARVProgramInstructions forPostpartum) "." "instructions"
--                                 ]
--
--                             else
--                                 [ viewCustomLabel language (Translate.PrenatalARVProgramInstructions forPostpartum) "." "instructions"
--                                 ]
--                         , referralField = form.referToARVProgram
--                         , referralUpdateFunc =
--                             \value form_ ->
--                                 { form_
--                                     | referToARVProgram = Just value
--                                     , referralFormARVProgram = Nothing
--                                     , accompanyToARVProgram = Nothing
--                                 }
--                         , formField = form.referralFormARVProgram
--                         , formUpdateFunc = \value form_ -> { form_ | referralFormARVProgram = Just value }
--                         , accompanyConfig =
--                             Just
--                                 ( form.accompanyToARVProgram
--                                 , \value form_ -> { form_ | accompanyToARVProgram = Just value }
--                                 )
--                         , reasonToSignFunc = NonReferralReasonARVProgram
--                         }
--
--                 FacilityNCDProgram ->
--                     Just
--                         { header =
--                             let
--                                 headerText =
--                                     translate language Translate.PrenatalNCDProgramHeaderPrefix
--                                         ++ " "
--                                         ++ diagnosesForView
--                                         ++ " "
--                                         ++ translate language Translate.PrenatalNCDProgramHeaderSuffix
--                                         ++ "."
--
--                                 diagnosesForView =
--                                     resolveNCDReferralDiagnoses assembled.nursePreviousMeasurementsWithDates
--                                         |> List.map (Translate.PrenatalDiagnosis >> translate language)
--                                         |> String.join ", "
--                             in
--                             [ div [ class "label" ] [ text headerText ]
--                             , viewCustomLabel language Translate.PrenatalNCDProgramInstructions "." "instructions"
--                             ]
--                         , referralField = form.referToNCDProgram
--                         , referralUpdateFunc =
--                             \value form_ ->
--                                 { form_
--                                     | referToNCDProgram = Just value
--                                     , referralFormNCDProgram = Nothing
--                                     , accompanyToNCDProgram = Nothing
--                                 }
--                         , formField = form.referralFormNCDProgram
--                         , formUpdateFunc = \value form_ -> { form_ | referralFormNCDProgram = Just value }
--                         , accompanyConfig =
--                             Just
--                                 ( form.accompanyToNCDProgram
--                                 , \value form_ -> { form_ | accompanyToNCDProgram = Just value }
--                                 )
--                         , reasonToSignFunc = NonReferralReasonNCDProgram
--                         }
--
--                 FacilityANCServices ->
--                     -- Not in use at Prenatal
--                     Nothing
--
--                 FacilityHealthCenter ->
--                     -- Not in use at Prenatal
--                     Nothing
--     in
--     Maybe.map
--         (\config ->
--             let
--                 instructions =
--                     case facility of
--                         FacilityMentalHealthSpecialist ->
--                             [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing ]
--
--                         _ ->
--                             [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing
--                             , viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" Nothing
--                             ]
--
--                 ( derivedSection, derivedTasks ) =
--                     Maybe.map
--                         (\referred ->
--                             if referred then
--                                 let
--                                     ( accompanySection, accompanyTasks ) =
--                                         Maybe.map
--                                             (\( field, updateFunc ) ->
--                                                 ( [ viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion facility
--                                                   , viewBoolInput
--                                                         language
--                                                         field
--                                                         (setReferralBoolInputMsg updateFunc)
--                                                         "accompany-to-hc"
--                                                         Nothing
--                                                   ]
--                                                 , [ field ]
--                                                 )
--                                             )
--                                             config.accompanyConfig
--                                             |> Maybe.withDefault ( [], [] )
--                                 in
--                                 ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
--                                   , viewBoolInput
--                                         language
--                                         config.formField
--                                         (setReferralBoolInputMsg config.formUpdateFunc)
--                                         "hand-referral-form"
--                                         Nothing
--                                   ]
--                                     ++ accompanySection
--                                 , [ config.formField ] ++ accompanyTasks
--                                 )
--
--                             else
--                                 ( nonReferralReasonSection language facility config.reasonToSignFunc setNonReferralReasonMsg form
--                                 , [ maybeToBoolTask <| getCurrentReasonForNonReferralByForm config.reasonToSignFunc form ]
--                                 )
--                         )
--                         config.referralField
--                         |> Maybe.withDefault ( [], [] )
--             in
--             ( config.header
--                 ++ [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
--                    , div [ class "instructions" ]
--                         instructions
--                    , viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion facility
--                    , viewBoolInput
--                         language
--                         config.referralField
--                         (setReferralBoolInputMsg config.referralUpdateFunc)
--                         "referral"
--                         Nothing
--                    ]
--                 ++ derivedSection
--                 ++ [ div [ class "separator" ] [] ]
--             , [ config.referralField ] ++ derivedTasks
--             )
--         )
--         maybeConfig
--         |> Maybe.withDefault ( [], [] )
--
--
-- nonReferralReasonSection :
--     Language
--     -> ReferralFacility
--     -> (ReasonForNonReferral -> NonReferralSign)
--     -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
--     -> ReferralForm
--     -> List (Html msg)
-- nonReferralReasonSection language facility reasonToSignFunc setNonReferralReasonMsg form =
--     let
--         currentValue =
--             getCurrentReasonForNonReferralByForm reasonToSignFunc form
--
--         options =
--             if facility == FacilityHospital then
--                 [ ClientRefused
--                 , NoAmbulance
--                 , ClientUnableToAffordFees
--                 , ReasonForNonReferralNotIndicated
--                 , ReasonForNonReferralOther
--                 ]
--
--             else
--                 [ ClientRefused
--                 , ClientAlreadyInCare
--                 , ReasonForNonReferralNotIndicated
--                 , ReasonForNonReferralOther
--                 ]
--     in
--     [ viewQuestionLabel language Translate.WhyNot
--     , viewCheckBoxSelectInput language
--         options
--         []
--         currentValue
--         (setNonReferralReasonMsg currentValue facility)
--         Translate.ReasonForNonReferral
--     ]
--
--
-- getCurrentReasonForNonReferralByForm :
--     (ReasonForNonReferral -> NonReferralSign)
--     -> ReferralForm
--     -> Maybe ReasonForNonReferral
-- getCurrentReasonForNonReferralByForm reasonToSignFunc form =
--     getCurrentReasonForNonReferral reasonToSignFunc form.nonReferralReasons
--
--
-- {-| Referal to facility is completed when we mark that facility was referred to,
-- or, reason was set for not referring to that facility.
-- -}
-- referralToFacilityCompleted : AssembledData -> ReferralFacility -> Bool
-- referralToFacilityCompleted assembled facility =
--     getMeasurementValueFunc assembled.measurements.sendToHC
--         |> Maybe.andThen
--             (\value ->
--                 Maybe.map
--                     (\referralSigns ->
--                         Backend.Measurement.Utils.referralToFacilityCompleted referralSigns value.nonReferralReasons facility
--                     )
--                     value.referToFacilitySigns
--             )
--         |> Maybe.withDefault False
