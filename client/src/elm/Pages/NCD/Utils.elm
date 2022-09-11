module Pages.NCD.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
import Backend.NCDEncounter.Types exposing (..)
import Backend.NCDEncounter.Utils exposing (getNCDEncountersForParticipant)
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Utils exposing (ageInMonths)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.View exposing (viewActionTakenLabel, viewMultipleTreatmentWithDosage, viewTreatmentOptionWithDosage)
import Pages.NCD.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeToBoolTask
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewInstructionsLabel
        , viewQuestionLabel
        )
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


generateNCDDiagnoses : NominalDate -> AssembledData -> List NCDDiagnosis
generateNCDDiagnoses currentDate assembled =
    List.filter (matchNCDDiagnosis currentDate assembled) allNCDDiagnoses


matchNCDDiagnosis : NominalDate -> AssembledData -> NCDDiagnosis -> Bool
matchNCDDiagnosis currentDate assembled diagnosis =
    -- @todo
    case diagnosis of
        DiagnosisHypertensionStage1 ->
            False

        DiagnosisHypertensionStage2 ->
            False

        DiagnosisHypertensionStage3 ->
            False

        DiagnosisDiabetesInitial ->
            False

        DiagnosisDiabetesRecurrent ->
            False

        DiagnosisRenalComplications ->
            False

        NoNCDDiagnosis ->
            False


allNCDDiagnoses : List NCDDiagnosis
allNCDDiagnoses =
    [ DiagnosisHypertensionStage1
    , DiagnosisHypertensionStage2
    , DiagnosisHypertensionStage3
    , DiagnosisDiabetesInitial
    , DiagnosisDiabetesRecurrent
    , DiagnosisRenalComplications
    , NoNCDDiagnosis
    ]


{-| Recommended Treatment activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used, and at least one of
them must be set.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
recommendedTreatmentMeasurementTaken : List RecommendedTreatmentSign -> NCDMeasurements -> Bool
recommendedTreatmentMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map
            (.recommendedTreatmentSigns
                >> Backend.Measurement.Utils.recommendedTreatmentMeasurementTaken allowedSigns
            )
        |> Maybe.withDefault False


recommendedTreatmentSignsForHypertension : List RecommendedTreatmentSign
recommendedTreatmentSignsForHypertension =
    [ TreatmentHydrochlorothiazide
    , TreatmentAmlodipine
    , TreatmentNifedipine
    , TreatmentCaptopril
    , TreatmentLisinopril
    , TreatmentAtenlol
    , TreatmentMethyldopa2
    , NoTreatmentForHypertension
    ]


recommendedTreatmentSignsForDiabetes : List RecommendedTreatmentSign
recommendedTreatmentSignsForDiabetes =
    [ TreatmentMetformin1m1e
    , TreatmentGlipenclamide1m1e
    , TreatmentMetformin2m1e
    , TreatmentGlipenclamide2m1e
    , TreatmentMetformin2m2e
    , TreatmentGlipenclamide2m2e
    , TreatmentMetformin2m2eGlipenclamide1m1e
    , TreatmentGlipenclamide2m2eMetformin1m1e
    , NoTreatmentForDiabetes
    ]


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


resolveMedicationDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasks language currentDate assembled setRecommendedTreatmentSignMsg setMedicationDistributionBoolInputMsg form =
    let
        ( hypertensionInputs, hypertensionCompleted, hypertensionActive ) =
            recommendedTreatmentForHypertensionInputAndTask language
                currentDate
                recommendedTreatmentSignsForHypertension
                (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
                assembled
                form

        ( diabetesInputs, diabetesCompleted, diabetesActive ) =
            recommendedTreatmentForDiabetesInputAndTask language
                currentDate
                recommendedTreatmentSignsForDiabetes
                (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForDiabetes)
                assembled
                form

        returnInOneMonthInput =
            [ viewQuestionLabel language <| Translate.NCDGuidanceSignQuestion ReturnInOneMonth
            , viewBoolInput
                language
                form.guidedToReturnInOneMonth
                (setMedicationDistributionBoolInputMsg (\value form_ -> { form_ | guidedToReturnInOneMonth = Just value }))
                "return-in-one-month"
                Nothing
            ]
    in
    ( hypertensionInputs ++ diabetesInputs ++ returnInOneMonthInput
    , hypertensionCompleted + diabetesCompleted + taskCompleted form.guidedToReturnInOneMonth
    , hypertensionActive + diabetesActive + 1
    )


recommendedTreatmentForHypertensionInputAndTask :
    Language
    -> NominalDate
    -> List RecommendedTreatmentSign
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
recommendedTreatmentForHypertensionInputAndTask language currentDate options setRecommendedTreatmentSignMsg assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel
                "icon-pills"
                (text <| translate language Translate.HypertensionRecommendedTreatmentHelper ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            options
            []
            currentValue
            setRecommendedTreatmentSignMsg
            (viewTreatmentOptionWithDosage language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentForDiabetesInputAndTask :
    Language
    -> NominalDate
    -> List RecommendedTreatmentSign
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
recommendedTreatmentForDiabetesInputAndTask language currentDate options setRecommendedTreatmentSignMsg assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForDiabetes)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel
                "icon-pills"
                (text <| translate language Translate.HypertensionRecommendedTreatmentHelper ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            options
            []
            currentValue
            setRecommendedTreatmentSignMsg
            (viewTreatmentOptionForDiabetes language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


viewTreatmentOptionForDiabetes : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionForDiabetes language sign =
    case sign of
        TreatmentMetformin2m2eGlipenclamide1m1e ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentMetformin2m2e
                , TreatmentGlipenclamide1m1e
                ]

        TreatmentGlipenclamide2m2eMetformin1m1e ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentGlipenclamide2m2e
                , TreatmentMetformin1m1e
                ]

        _ ->
            viewTreatmentOptionWithDosage language sign


resolveReferralInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralInputsAndTasks language currentDate assembled setReferralBoolInputMsg setNonReferralReasonMsg form =
    let
        facility =
            if isPregnant then
                FacilityANCServices

            else
                FacilityHospital

        isPregnant =
            getMeasurementValueFunc assembled.measurements.pregnancyTest
                |> Maybe.andThen .testResult
                |> Maybe.map ((==) TestPositive)
                |> Maybe.withDefault False

        maybeConfig =
            case facility of
                FacilityHospital ->
                    Just
                        { header =
                            [ viewCustomLabel language Translate.ReferToHospitalForFurtherEvaluation "." "instructions" ]
                        , referralField = form.referToHospital
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToHospital = Just value
                                    , referralFormHospital = Nothing
                                }
                        , formField = form.referralFormHospital
                        , formUpdateFunc = \value form_ -> { form_ | referralFormHospital = Just value }
                        , accompanyConfig = Nothing
                        , reasonToSignFunc = NonReferralReasonHospital
                        }

                FacilityANCServices ->
                    Just
                        { header =
                            [ viewCustomLabel language Translate.NCDANCServicesInstructions "." "instructions" ]
                        , referralField = form.referToANCServices
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToANCServices = Just value
                                    , referralFormANCServices = Nothing
                                    , accompanyToANCServices = Nothing
                                }
                        , formField = form.referralFormANCServices
                        , formUpdateFunc = \value form_ -> { form_ | referralFormANCServices = Just value }
                        , accompanyConfig =
                            Just
                                ( form.accompanyToANCServices
                                , \value form_ -> { form_ | accompanyToANCServices = Just value }
                                )
                        , reasonToSignFunc = NonReferralReasonANCServices
                        }

                -- Other facilities are not in use at NCD.
                _ ->
                    Nothing
    in
    Maybe.map
        (\config ->
            let
                ( derivedSection, derivedTasks ) =
                    Maybe.map
                        (\referred ->
                            if referred then
                                let
                                    ( accompanySection, accompanyTasks ) =
                                        Maybe.map
                                            (\( field, updateFunc ) ->
                                                ( [ viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion facility
                                                  , viewBoolInput
                                                        language
                                                        field
                                                        (setReferralBoolInputMsg updateFunc)
                                                        "accompany-to-hc"
                                                        Nothing
                                                  ]
                                                , [ field ]
                                                )
                                            )
                                            config.accompanyConfig
                                            |> Maybe.withDefault ( [], [] )
                                in
                                ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
                                  , viewBoolInput
                                        language
                                        config.formField
                                        (setReferralBoolInputMsg config.formUpdateFunc)
                                        "hand-referral-form"
                                        Nothing
                                  ]
                                    ++ accompanySection
                                , [ config.formField ] ++ accompanyTasks
                                )

                            else
                                ( nonReferralReasonSection language facility config.reasonToSignFunc setNonReferralReasonMsg form
                                , [ maybeToBoolTask <| getCurrentReasonForNonReferralByForm config.reasonToSignFunc form ]
                                )
                        )
                        config.referralField
                        |> Maybe.withDefault ( [], [] )
            in
            ( config.header
                ++ [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                   , div [ class "instructions" ]
                        [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing
                        , viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" Nothing
                        ]
                   , viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion facility
                   , viewBoolInput
                        language
                        config.referralField
                        (setReferralBoolInputMsg config.referralUpdateFunc)
                        "referral"
                        Nothing
                   ]
                ++ derivedSection
                ++ [ div [ class "separator" ] [] ]
            , [ config.referralField ] ++ derivedTasks
            )
        )
        maybeConfig
        |> Maybe.withDefault ( [], [] )


nonReferralReasonSection :
    Language
    -> ReferralFacility
    -> (ReasonForNonReferral -> NonReferralSign)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> List (Html msg)
nonReferralReasonSection language facility reasonToSignFunc setNonReferralReasonMsg form =
    let
        currentValue =
            getCurrentReasonForNonReferralByForm reasonToSignFunc form

        options =
            if facility == FacilityHospital then
                [ ClientRefused
                , NoAmbulance
                , ClientUnableToAffordFees
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]

            else
                [ ClientRefused
                , ClientAlreadyInCare
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]
    in
    [ viewQuestionLabel language Translate.WhyNot
    , viewCheckBoxSelectInput language
        options
        []
        currentValue
        (setNonReferralReasonMsg currentValue facility)
        Translate.ReasonForNonReferral
    ]


getCurrentReasonForNonReferralByForm :
    (ReasonForNonReferral -> NonReferralSign)
    -> ReferralForm
    -> Maybe ReasonForNonReferral
getCurrentReasonForNonReferralByForm reasonToSignFunc form =
    getCurrentReasonForNonReferral reasonToSignFunc form.nonReferralReasons


{-| Referal to facility is completed when we mark that facility was referred to,
or, reason was set for not referring to that facility.
-}
referralToFacilityCompleted : AssembledData -> ReferralFacility -> Bool
referralToFacilityCompleted assembled facility =
    getMeasurementValueFunc assembled.measurements.referral
        |> Maybe.map
            (\value ->
                Backend.Measurement.Utils.referralToFacilityCompleted value.referralSigns value.nonReferralReasons facility
            )
        |> Maybe.withDefault False
