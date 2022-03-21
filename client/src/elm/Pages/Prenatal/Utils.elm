module Pages.Prenatal.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..), PrenatalEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (getCurrentReasonForMedicationNonAdministration, nonAdministrationReasonToSign)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)


calculateEGAWeeks : NominalDate -> NominalDate -> Int
calculateEGAWeeks currentDate lmpDate =
    calculateEGADays currentDate lmpDate // 7


calculateEGADays : NominalDate -> NominalDate -> Int
calculateEGADays currentDate lmpDate =
    diffDays lmpDate currentDate


diagnosed : PrenatalDiagnosis -> AssembledData -> Bool
diagnosed diagnosis assembled =
    EverySet.member diagnosis assembled.encounter.diagnoses


diagnosedAnyOf : List PrenatalDiagnosis -> AssembledData -> Bool
diagnosedAnyOf diagnoses assembled =
    List.any
        (\diagnosis -> diagnosed diagnosis assembled)
        diagnoses


listNonUrgentDiagnoses : List PrenatalDiagnosis -> List PrenatalDiagnosis
listNonUrgentDiagnoses diagnoses =
    let
        exclusions =
            DiagnosisPrescribeMebendezole
                :: emergencyReferralDiagnosesInitial
                ++ emergencyReferralDiagnosesRecurrent
    in
    List.filter (\diagnosis -> not <| List.member diagnosis exclusions) diagnoses


emergencyReferralDiagnosesInitial : List PrenatalDiagnosis
emergencyReferralDiagnosesInitial =
    [ DiagnosisSeverePreeclampsiaImmediate
    , DiagnosisEclampsia
    , DiagnosisMiscarriage
    , DiagnosisMolarPregnancy
    , DiagnosisPlacentaPrevia
    , DiagnosisPlacentalAbruption
    , DiagnosisUterineRupture
    , DiagnosisObstructedLabor
    , DiagnosisPostAbortionSepsis
    , DiagnosisEctopicPregnancy
    , DiagnosisPROM
    , DiagnosisPPROM
    , DiagnosisHyperemesisGravidum
    , DiagnosisMaternalComplications

    -- Infection diagnosis will be available at latter phase.
    -- , DiagnosisInfection
    , DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    ]


emergencyReferralDiagnosesRecurrent : List PrenatalDiagnosis
emergencyReferralDiagnosesRecurrent =
    [ DiagnosisSeverePreeclampsiaAfterRecheck
    , DiagnosisSevereAnemiaWithComplications
    ]


medicationDistributionFormWithDefaultInitialPhase : MedicationDistributionForm -> Maybe MedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefaultInitialPhase form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    allowedSigns =
                        NoMedicationDistributionSignsInitialPhase :: medicationsInitialPhase
                in
                { mebendezole = or form.mebendezole (medicationDistributionResolveFromValue allowedSigns value Mebendezole)
                , tenofovir = or form.tenofovir (medicationDistributionResolveFromValue allowedSigns value Tenofovir)
                , lamivudine = or form.lamivudine (medicationDistributionResolveFromValue allowedSigns value Lamivudine)
                , dolutegravir = or form.dolutegravir (medicationDistributionResolveFromValue allowedSigns value Dolutegravir)
                , tdf3tc = or form.tdf3tc (medicationDistributionResolveFromValue allowedSigns value TDF3TC)

                -- Following 2 do not participate at initial phase, therefore,
                -- resolved directly from value.
                , iron = EverySet.member Iron value.distributionSigns |> Just
                , folicAcid = EverySet.member FolicAcid value.distributionSigns |> Just
                , nonAdministrationSigns = or form.nonAdministrationSigns (Just value.nonAdministrationSigns)
                }
            )


medicationDistributionFormWithDefaultRecurrentPhase : MedicationDistributionForm -> Maybe MedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefaultRecurrentPhase form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    allowedSigns =
                        NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase
                in
                { iron = or form.iron (medicationDistributionResolveFromValue allowedSigns value Iron)
                , folicAcid = or form.folicAcid (medicationDistributionResolveFromValue allowedSigns value FolicAcid)

                -- Following 5 do not participate at recurrent phase, therefore,
                -- resolved directly from value.
                , mebendezole = EverySet.member Mebendezole value.distributionSigns |> Just
                , tenofovir = EverySet.member Tenofovir value.distributionSigns |> Just
                , lamivudine = EverySet.member Lamivudine value.distributionSigns |> Just
                , dolutegravir = EverySet.member Dolutegravir value.distributionSigns |> Just
                , tdf3tc = EverySet.member TDF3TC value.distributionSigns |> Just
                , nonAdministrationSigns = or form.nonAdministrationSigns (Just value.nonAdministrationSigns)
                }
            )


medicationDistributionResolveFromValue : List MedicationDistributionSign -> MedicationDistributionValue -> MedicationDistributionSign -> Maybe Bool
medicationDistributionResolveFromValue allowedSigns value sign =
    let
        valueSetForSign =
            EverySet.member sign value.distributionSigns

        nonAdministrationNoteSetForSign =
            EverySet.toList value.nonAdministrationSigns
                |> List.filterMap
                    (\sign_ ->
                        case sign_ of
                            MedicationAmoxicillin reason ->
                                Just ( Amoxicillin, reason )

                            MedicationCoartem reason ->
                                Just ( Coartem, reason )

                            MedicationORS reason ->
                                Just ( ORS, reason )

                            MedicationZinc reason ->
                                Just ( Zinc, reason )

                            MedicationParacetamol reason ->
                                Just ( Paracetamol, reason )

                            MedicationMebendezole reason ->
                                Just ( Mebendezole, reason )

                            MedicationTenofovir reason ->
                                Just ( Tenofovir, reason )

                            MedicationLamivudine reason ->
                                Just ( Lamivudine, reason )

                            MedicationDolutegravir reason ->
                                Just ( Dolutegravir, reason )

                            MedicationTDF3TC reason ->
                                Just ( TDF3TC, reason )

                            MedicationIron reason ->
                                Just ( Iron, reason )

                            MedicationFolicAcid reason ->
                                Just ( FolicAcid, reason )

                            NoMedicationNonAdministrationSigns ->
                                Nothing
                    )
                |> List.filter (Tuple.first >> (==) sign)
                |> List.isEmpty
                |> not
    in
    if valueSetForSign then
        Just True

    else if nonAdministrationNoteSetForSign then
        Just False

    else
        Nothing


medicationDistributionFormWithDefault : MedicationDistributionSign -> MedicationDistributionForm -> Maybe MedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefault valueForNone form saved =
    case valueForNone of
        NoMedicationDistributionSignsInitialPhase ->
            medicationDistributionFormWithDefaultInitialPhase form saved

        NoMedicationDistributionSignsRecurrentPhase ->
            medicationDistributionFormWithDefaultRecurrentPhase form saved

        -- We should never get here.
        _ ->
            form


toMedicationDistributionValueWithDefaultInitialPhase : Maybe MedicationDistributionValue -> MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValueWithDefaultInitialPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsInitialPhase


toMedicationDistributionValueWithDefaultRecurrentPhase : Maybe MedicationDistributionValue -> MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValueWithDefaultRecurrentPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsRecurrentPhase


toMedicationDistributionValueWithDefault : MedicationDistributionSign -> Maybe MedicationDistributionValue -> MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValueWithDefault valueForNone saved form =
    medicationDistributionFormWithDefault valueForNone form saved
        |> toMedicationDistributionValue valueForNone


toMedicationDistributionValue : MedicationDistributionSign -> MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValue valueForNone form =
    let
        distributionSigns =
            [ ifNullableTrue Mebendezole form.mebendezole
            , ifNullableTrue Tenofovir form.tenofovir
            , ifNullableTrue Lamivudine form.lamivudine
            , ifNullableTrue Dolutegravir form.dolutegravir
            , ifNullableTrue TDF3TC form.tdf3tc
            , ifNullableTrue Iron form.iron
            , ifNullableTrue FolicAcid form.folicAcid
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty valueForNone)

        nonAdministrationSigns =
            form.nonAdministrationSigns
                |> Maybe.withDefault EverySet.empty
                |> ifEverySetEmpty NoMedicationNonAdministrationSigns
                |> Just
    in
    Maybe.map MedicationDistributionValue distributionSigns
        |> andMap nonAdministrationSigns


resolveMedicationDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> List MedicationDistributionSign
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasks language currentDate assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg allowedMedications form =
    resolveMedicationsByDiagnoses assembled allowedMedications
        |> List.map
            (resolveMedicationDistributionInputsAndTasksForMedication language
                currentDate
                assembled.person
                setMedicationDistributionBoolInputMsg
                setMedicationDistributionAdministrationNoteMsg
                form
            )
        |> List.foldr
            (\( inputs, completed, active ) ( accumInputs, accumCompleted, accumActive ) ->
                ( inputs ++ accumInputs, completed + accumCompleted, active + accumActive )
            )
            ( [], 0, 0 )


{-| Medication Distribution activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
medicationDistributionMeasurementTaken : List MedicationDistributionSign -> PrenatalMeasurements -> Bool
medicationDistributionMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map
            (.distributionSigns
                >> (\signs ->
                        List.any (\sign -> EverySet.member sign signs)
                            allowedSigns
                   )
            )
        |> Maybe.withDefault False


resolveMedicationsByDiagnoses : AssembledData -> List MedicationDistributionSign -> List MedicationDistributionSign
resolveMedicationsByDiagnoses assembled allowedMedications =
    List.filter
        (\medication ->
            let
                hivDiagnosed =
                    diagnosed DiagnosisHIV assembled

                hivProgramHC =
                    hivProgramAtHC assembled
            in
            case medication of
                Mebendezole ->
                    diagnosed DiagnosisPrescribeMebendezole assembled

                Tenofovir ->
                    hivDiagnosed && not hivProgramHC

                Lamivudine ->
                    hivDiagnosed && not hivProgramHC

                Dolutegravir ->
                    hivDiagnosed && not hivProgramHC

                TDF3TC ->
                    diagnosed DiagnosisDiscordantPartnership assembled

                Iron ->
                    diagnosed DiagnosisModerateAnemia assembled

                FolicAcid ->
                    diagnosed DiagnosisModerateAnemia assembled

                _ ->
                    False
        )
        allowedMedications


resolveMedicationDistributionInputsAndTasksForMedication :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> MedicationDistributionSign
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasksForMedication language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form medication =
    case medication of
        Mebendezole ->
            resolveMebendezoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Tenofovir ->
            resolveTenofovirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Lamivudine ->
            resolveLamivudineDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Dolutegravir ->
            resolveDolutegravirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        TDF3TC ->
            resolveTDF3TCDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Iron ->
            resolveIronDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        FolicAcid ->
            resolveFolicAcidDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        -- Other medications are not prescribed at Prenatal encounter.
        _ ->
            ( [], 0, 0 )


resolveMebendezoleDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMebendezoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveMebendezoleDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Mebendezole) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerPrenatalMebendezoleHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | mebendezole = Just value, nonAdministrationSigns = updateNonAdministrationSigns Mebendezole MedicationMebendezole value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.mebendezole == Just False then
                ( viewMedicationDistributionDerivedQuestion language Mebendezole MedicationMebendezole setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationMebendezole form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Mebendezole)
      , viewBoolInput
            language
            form.mebendezole
            (setMedicationDistributionBoolInputMsg updateFunc)
            "mebendezole-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.mebendezole + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveTenofovirDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveTenofovirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveTenofovirDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Tenofovir) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | tenofovir = Just value, nonAdministrationSigns = updateNonAdministrationSigns Tenofovir MedicationTenofovir value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.tenofovir == Just False then
                ( viewMedicationDistributionDerivedQuestion language Tenofovir MedicationTenofovir setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationTenofovir form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Tenofovir)
      , viewBoolInput
            language
            form.tenofovir
            (setMedicationDistributionBoolInputMsg updateFunc)
            "tenofovir-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.tenofovir + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveLamivudineDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveLamivudineDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveLamivudineDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Lamivudine) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | lamivudine = Just value, nonAdministrationSigns = updateNonAdministrationSigns Lamivudine MedicationLamivudine value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.lamivudine == Just False then
                ( viewMedicationDistributionDerivedQuestion language Lamivudine MedicationLamivudine setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationLamivudine form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Lamivudine)
      , viewBoolInput
            language
            form.lamivudine
            (setMedicationDistributionBoolInputMsg updateFunc)
            "lamivudine-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.lamivudine + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveDolutegravirDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveDolutegravirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveDolutegravirDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Dolutegravir) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | dolutegravir = Just value, nonAdministrationSigns = updateNonAdministrationSigns Dolutegravir MedicationDolutegravir value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.dolutegravir == Just False then
                ( viewMedicationDistributionDerivedQuestion language Dolutegravir MedicationDolutegravir setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationDolutegravir form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Dolutegravir)
      , viewBoolInput
            language
            form.dolutegravir
            (setMedicationDistributionBoolInputMsg updateFunc)
            "dolutegravir-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.dolutegravir + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveTDF3TCDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveTDF3TCDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            div [ class "instructions" ]
                [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign TDF3TC) "icon-pills" Nothing
                , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                ]

        updateFunc value form_ =
            { form_ | tdf3tc = Just value, nonAdministrationSigns = updateNonAdministrationSigns TDF3TC MedicationTDF3TC value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.tdf3tc == Just False then
                ( viewMedicationDistributionDerivedQuestion language TDF3TC MedicationTDF3TC setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationTDF3TC form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign TDF3TC)
      , viewBoolInput
            language
            form.tdf3tc
            (setMedicationDistributionBoolInputMsg updateFunc)
            "tdf3tc-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.tdf3tc + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveIronDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveIronDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveIronDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Iron) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerIronHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | iron = Just value, nonAdministrationSigns = updateNonAdministrationSigns Iron MedicationIron value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.iron == Just False then
                ( viewMedicationDistributionDerivedQuestion language Iron MedicationIron setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationIron form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Iron)
      , viewBoolInput
            language
            form.iron
            (setMedicationDistributionBoolInputMsg updateFunc)
            "iron-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.iron + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


resolveFolicAcidDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveFolicAcidDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveFolicAcidDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign FolicAcid) ("(" ++ dosage ++ ")") icon ":" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerFolicAcidHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | folicAcid = Just value, nonAdministrationSigns = updateNonAdministrationSigns FolicAcid MedicationFolicAcid value form_ }

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
            if form.folicAcid == Just False then
                ( viewMedicationDistributionDerivedQuestion language FolicAcid MedicationFolicAcid setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationFolicAcid form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign FolicAcid)
      , viewBoolInput
            language
            form.folicAcid
            (setMedicationDistributionBoolInputMsg updateFunc)
            "folicAcid-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.folicAcid + derrivedTaskCompleted
    , 1 + derrivedTaskActive
    )


viewMedicationDistributionDerivedQuestion :
    Language
    -> MedicationDistributionSign
    -> (AdministrationNote -> MedicationNonAdministrationSign)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> List (Html msg)
viewMedicationDistributionDerivedQuestion language medication reasonToSignFunc setMedicationDistributionAdministrationNoteMsg form =
    let
        currentValue =
            getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
    in
    [ viewQuestionLabel language Translate.WhyNot
    , viewCheckBoxSelectInput language
        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
        currentValue
        (setMedicationDistributionAdministrationNoteMsg currentValue medication)
        Translate.AdministrationNote
    ]


{-| When the answer for medication administration is Yes,
we clean the reason for not administering the medication.
-}
updateNonAdministrationSigns :
    MedicationDistributionSign
    -> (AdministrationNote -> MedicationNonAdministrationSign)
    -> Bool
    -> MedicationDistributionForm
    -> Maybe (EverySet MedicationNonAdministrationSign)
updateNonAdministrationSigns medication reasonToSignFunc value form =
    if value == True then
        form.nonAdministrationSigns
            |> Maybe.andThen
                (\nonAdministrationSigns ->
                    getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
                        |> Maybe.map
                            (\reason ->
                                Just <| EverySet.remove (nonAdministrationReasonToSign medication reason) nonAdministrationSigns
                            )
                        |> Maybe.withDefault (Just nonAdministrationSigns)
                )

    else
        form.nonAdministrationSigns


resolveMebendezoleDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveMebendezoleDosageAndIcon currentDate person =
    Just ( "500 mg", "icon-pills" )


resolveTenofovirDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveTenofovirDosageAndIcon currentDate person =
    Just ( "300 mg", "icon-pills" )


resolveLamivudineDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveLamivudineDosageAndIcon currentDate person =
    Just ( "300 mg", "icon-pills" )


resolveDolutegravirDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveDolutegravirDosageAndIcon currentDate person =
    Just ( "50 mg", "icon-pills" )


resolveIronDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveIronDosageAndIcon currentDate person =
    Just ( "120 mg", "icon-pills" )


resolveFolicAcidDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveFolicAcidDosageAndIcon currentDate person =
    Just ( "400 IU", "icon-pills" )


medicationsInitialPhase : List MedicationDistributionSign
medicationsInitialPhase =
    [ Mebendezole
    , Tenofovir
    , Lamivudine
    , Dolutegravir
    , TDF3TC
    ]


medicationsRecurrentPhase : List MedicationDistributionSign
medicationsRecurrentPhase =
    [ Iron
    , FolicAcid
    ]


hivProgramAtHC : AssembledData -> Bool
hivProgramAtHC assembled =
    getMeasurementValueFunc assembled.measurements.hivTest
        |> Maybe.andThen .hivSigns
        |> Maybe.map (EverySet.member HIVProgramHC)
        |> Maybe.withDefault False


recommendedTreatmentFormWithDefault : RecommendedTreatmentForm -> Maybe RecommendedTreatmentValue -> RecommendedTreatmentForm
recommendedTreatmentFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (Maybe.map EverySet.toList value.signs) }
            )


toRecommendedTreatmentValueWithDefault : Maybe RecommendedTreatmentValue -> RecommendedTreatmentForm -> Maybe RecommendedTreatmentValue
toRecommendedTreatmentValueWithDefault saved form =
    recommendedTreatmentFormWithDefault form saved
        |> toRecommendedTreatmentValue


toRecommendedTreatmentValue : RecommendedTreatmentForm -> Maybe RecommendedTreatmentValue
toRecommendedTreatmentValue form =
    Maybe.map
        (\signs ->
            { signs = EverySet.fromList signs |> Just
            }
        )
        form.signs


{-| Recommended Treatment activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used, and at least one of
them must be set.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
recommendedTreatmentMeasurementTaken : List RecommendedTreatmentSign -> PrenatalMeasurements -> Bool
recommendedTreatmentMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.recommendedTreatment
        |> Maybe.andThen .signs
        |> Maybe.map
            (\signs ->
                List.any (\sign -> EverySet.member sign signs)
                    allowedSigns
            )
        |> Maybe.withDefault False


resolveRecommendedTreatmentSectionState : Bool -> List RecommendedTreatmentSign -> Maybe (List RecommendedTreatmentSign) -> ( Int, Int )
resolveRecommendedTreatmentSectionState isDiagnosed allowedSigns currentSigns =
    if isDiagnosed then
        Maybe.map
            (\signs ->
                let
                    completed =
                        -- We know that section is completed when one of allowed
                        -- signs is set (as only single selection is allowed).
                        List.any (\sign -> List.member sign signs) allowedSigns
                in
                if completed then
                    ( 1, 1 )

                else
                    ( 0, 1 )
            )
            currentSigns
            |> Maybe.withDefault ( 0, 1 )

    else
        ( 0, 0 )


recommendTreatmentForHypertension : AssembledData -> RecommendedTreatmentSign
recommendTreatmentForHypertension assembled =
    let
        numberOfTimesMethyldopaWasPerscribed =
            assembled.nursePreviousMeasurementsWithDates
                |> List.filter
                    (\( _, measurements ) ->
                        getMeasurementValueFunc measurements.recommendedTreatment
                            |> Maybe.andThen .signs
                            |> Maybe.map
                                (\signs ->
                                    List.any (\sign -> EverySet.member sign signs) recommendedTreatmentSignsForHypertension
                                )
                            |> Maybe.withDefault False
                    )
                |> List.length
    in
    case numberOfTimesMethyldopaWasPerscribed of
        0 ->
            TreatmentMethyldopa2

        1 ->
            TreatmentMethyldopa3

        _ ->
            TreatmentMethyldopa4


recommendedTreatmentSignsForHypertension : List RecommendedTreatmentSign
recommendedTreatmentSignsForHypertension =
    [ TreatmentMethyldopa2
    , TreatmentMethyldopa3
    , TreatmentMethyldopa4
    ]


marginalBloodPressureCondition : Float -> Float -> Bool
marginalBloodPressureCondition dia sys =
    (dia >= 90 && dia < 110) || (sys >= 140 && sys < 160)
