module Pages.NCD.Utils exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (..)
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
import Measurement.View exposing (viewActionTakenLabel)
import Pages.NCD.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeToBoolTask
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
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
    -- let
    --
    --     ( inputsByDiagnoses, completedByDiagnoses, activeByDiagnoses ) =
    --         let
    --             ( hypertensionInputs, hypertensionCompleted, hypertensionActive ) =
    --                 if diagnosedHypertension phase assembled then
    --                     resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks language
    --                         currentDate
    --                         (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
    --                         assembled
    --                         form
    --
    --                 else if diagnosedHypertensionPrevoiusly assembled || diagnosedModeratePreeclampsiaPrevoiusly assembled then
    --                     resolveRecommendedTreatmentForPrevoiuslyDiagnosedHypertensionInputsAndTasks language
    --                         currentDate
    --                         (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
    --                         avoidingGuidanceReasonMsg
    --                         assembled
    --                         form
    --
    --                 else
    --                     ( [], 0, 0 )
    --         in
    --         case phase of
    --             PrenatalEncounterPhaseInitial ->
    --                 let
    --                     ( malariaInputs, malariaCompleted, malariaActive ) =
    --                         if diagnosedMalaria assembled then
    --                             resolveRecommendedTreatmentForMalariaInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --
    --                     ( heartburnInputs, heartburnCompleted, heartburnActive ) =
    --                         if diagnosed DiagnosisHeartburn assembled then
    --                             resolveRecommendedTreatmentForHeartburnInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --
    --                     ( urinaryTractInfectionInputs, urinaryTractInfectionCompleted, urinaryTractInfectionActive ) =
    --                         if diagnosed DiagnosisUrinaryTractInfection assembled then
    --                             resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --
    --                     ( candidiasisInputs, candidiasisCompleted, candidiasisActive ) =
    --                         if diagnosed DiagnosisCandidiasis assembled then
    --                             resolveRecommendedTreatmentForCandidiasisInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --
    --                     ( mastitisInputs, mastitisCompleted, mastitisActive ) =
    --                         if diagnosed DiagnosisPostpartumMastitis assembled then
    --                             resolveRecommendedTreatmentForMastitisInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --                 in
    --                 ( malariaInputs
    --                     ++ heartburnInputs
    --                     ++ hypertensionInputs
    --                     ++ urinaryTractInfectionInputs
    --                     ++ candidiasisInputs
    --                     ++ mastitisInputs
    --                 , malariaCompleted
    --                     + heartburnCompleted
    --                     + hypertensionCompleted
    --                     + urinaryTractInfectionCompleted
    --                     + candidiasisCompleted
    --                     + mastitisCompleted
    --                 , malariaActive
    --                     + heartburnActive
    --                     + hypertensionActive
    --                     + urinaryTractInfectionActive
    --                     + candidiasisActive
    --                     + mastitisActive
    --                 )
    --
    --             PrenatalEncounterPhaseRecurrent ->
    --                 let
    --                     ( syphilisInputs, syphilisCompleted, syphilisActive ) =
    --                         if diagnosedSyphilis assembled then
    --                             resolveRecommendedTreatmentForSyphilisInputsAndTasks language
    --                                 currentDate
    --                                 setRecommendedTreatmentSignMsg
    --                                 recommendedTreatmentSignsForSyphilis
    --                                 assembled
    --                                 form
    --
    --                         else
    --                             ( [], 0, 0 )
    --                 in
    --                 ( syphilisInputs ++ hypertensionInputs
    --                 , syphilisCompleted + hypertensionCompleted
    --                 , syphilisActive + hypertensionActive
    --                 )
    -- in
    -- ( inputsByMedications ++ inputsByDiagnoses
    -- , completedByMedications + completedByDiagnoses
    -- , activeByMedications + activeByDiagnoses
    -- )
    -- @todo
    -- resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks :
    --     Language
    --     -> NominalDate
    --     -> (RecommendedTreatmentSign -> msg)
    --     -> AssembledData
    --     -> MedicationDistributionForm
    --     -> ( List (Html msg), Int, Int )
    -- resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    --     let
    --         ( input, completed, active ) =
    --             recommendedTreatmentForHypertensionInputAndTask language
    --                 currentDate
    --                 recommendedTreatmentSignsForHypertensionInitial
    --                 setRecommendedTreatmentSignMsg
    --                 assembled
    --                 form
    --     in
    --     ( viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
    --         :: input
    --         ++ [ div [ class "separator" ] [] ]
    --     , completed
    --     , active
    --     )
    ( [], 0, 1 )


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
