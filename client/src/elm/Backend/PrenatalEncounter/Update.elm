module Backend.PrenatalEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> PrenatalEncounterId
    -> Maybe PrenatalEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseEncounter ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | endDate = Just currentDate }) model

        SetPrenatalDiagnoses diagnoses ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | diagnoses = diagnoses }) model

        SetPastPrenatalDiagnoses pastDiagnoses ->
            updateEncounter currentDate
                encounterId
                maybeEncounter
                (\encounter ->
                    { encounter
                        | pastDiagnoses =
                            -- If previously there were no diagnoses, we remove
                            -- that indicator.
                            EverySet.remove NoPrenatalDiagnosis encounter.pastDiagnoses
                                |> EverySet.union pastDiagnoses
                    }
                )
                model

        SetLabsHistoryCompleted ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | indicators = EverySet.insert IndicatorHistoryLabsCompleted encounter.indicators }) model

        SetNextVisitDate date ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | nextVisitDate = Just date }) model

        SetGWGIndicator adequateGWG ->
            updateEncounter currentDate
                encounterId
                maybeEncounter
                (\encounter ->
                    if adequateGWG then
                        { encounter
                            | indicators =
                                EverySet.remove NoPrenatalIndicators encounter.indicators
                                    |> EverySet.remove IndicatorInadequateGWG
                                    |> EverySet.insert IndicatorAdequateGWG
                        }

                    else
                        { encounter
                            | indicators =
                                EverySet.remove NoPrenatalIndicators encounter.indicators
                                    |> EverySet.remove IndicatorAdequateGWG
                                    |> EverySet.insert IndicatorInadequateGWG
                        }
                )
                model

        HandleUpdatedPrenatalEncounter data ->
            ( { model | updatePrenatalEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBreastExam personId valueId value ->
            ( { model | saveBreastExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value breastExamEndpoint HandleSavedBreastExam
            , []
            )

        HandleSavedBreastExam data ->
            ( { model | saveBreastExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCorePhysicalExam personId valueId value ->
            ( { model | saveCorePhysicalExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value corePhysicalExamEndpoint HandleSavedCorePhysicalExam
            , []
            )

        HandleSavedCorePhysicalExam data ->
            ( { model | saveCorePhysicalExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDangerSigns personId valueId value ->
            ( { model | saveDangerSigns = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value dangerSignsEndpoint HandleSavedDangerSigns
            , []
            )

        HandleSavedDangerSigns data ->
            ( { model | saveDangerSigns = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveLastMenstrualPeriod personId valueId value ->
            ( { model | saveLastMenstrualPeriod = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value lastMenstrualPeriodEndpoint HandleSavedLastMenstrualPeriod
            , []
            )

        HandleSavedLastMenstrualPeriod data ->
            ( { model | saveLastMenstrualPeriod = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedicalHistory personId valueId value ->
            ( { model | saveMedicalHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicalHistoryEndpoint HandleSavedMedicalHistory
            , []
            )

        HandleSavedMedicalHistory data ->
            ( { model | saveMedicalHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedication personId valueId value ->
            ( { model | saveMedication = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value medicationEndpoint HandleSavedMedication
            , []
            )

        HandleSavedMedication data ->
            ( { model | saveMedication = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveAspirin personId valueId value ->
            ( { model | saveAspirin = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalAspirinEndpoint HandleSavedAspirin
            , []
            )

        HandleSavedAspirin data ->
            ( { model | saveAspirin = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCalcium personId valueId value ->
            ( { model | saveCalcium = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalCalciumEndpoint HandleSavedCalcium
            , []
            )

        HandleSavedCalcium data ->
            ( { model | saveCalcium = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFefol personId valueId value ->
            ( { model | saveFefol = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFefolEndpoint HandleSavedFefol
            , []
            )

        HandleSavedFefol data ->
            ( { model | saveFefol = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFolate personId valueId value ->
            ( { model | saveFolate = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFolateEndpoint HandleSavedFolate
            , []
            )

        HandleSavedFolate data ->
            ( { model | saveFolate = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveIron personId valueId value ->
            ( { model | saveIron = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalIronEndpoint HandleSavedIron
            , []
            )

        HandleSavedIron data ->
            ( { model | saveIron = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMMS personId valueId value ->
            ( { model | saveMMS = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalMMSEndpoint HandleSavedMMS
            , []
            )

        HandleSavedMMS data ->
            ( { model | saveMMS = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMebendazole personId valueId value ->
            ( { model | saveMebendazole = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalMebendazoleEndpoint HandleSavedMebendazole
            , []
            )

        HandleSavedMebendazole data ->
            ( { model | saveMebendazole = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveObstetricalExam personId valueId value ->
            ( { model | saveObstetricalExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricalExamEndpoint HandleSavedObstetricalExam
            , []
            )

        HandleSavedObstetricalExam data ->
            ( { model | saveObstetricalExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveObstetricHistory personId valueId value ->
            ( { model | saveObstetricHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricHistoryEndpoint HandleSavedObstetricHistory
            , []
            )

        HandleSavedObstetricHistory data ->
            ( { model | saveObstetricHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveObstetricHistoryStep2 personId valueId value ->
            ( { model | saveObstetricHistoryStep2 = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value obstetricHistoryStep2Endpoint HandleSavedObstetricHistoryStep2
            , []
            )

        HandleSavedObstetricHistoryStep2 data ->
            ( { model | saveObstetricHistoryStep2 = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFamilyPlanning personId valueId value ->
            ( { model | saveFamilyPlanning = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFamilyPlanningEndpoint HandleSavedFamilyPlanning
            , []
            )

        HandleSavedFamilyPlanning data ->
            ( { model | saveFamilyPlanning = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalNutritionEndpoint HandleSavedNutrition
            , []
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMalariaPrevention personId valueId value ->
            ( { model | saveMalariaPrevention = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value malariaPreventionEndpoint HandleSavedMalariaPrevention
            , []
            )

        HandleSavedMalariaPrevention data ->
            ( { model | saveMalariaPrevention = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSocialHistory personId valueId value ->
            ( { model | saveSocialHistory = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value socialHistoryEndpoint HandleSavedSocialHistory
            , []
            )

        HandleSavedSocialHistory data ->
            ( { model | saveSocialHistory = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value vitalsEndpoint HandleSavedVitals
            , []
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePrenatalPhoto personId valueId value ->
            ( { model | savePrenatalPhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalPhotoEndpoint HandleSavedPrenatalPhoto
            , []
            )

        HandleSavedPrenatalPhoto data ->
            ( { model | savePrenatalPhoto = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBirthPlan personId valueId value ->
            ( { model | saveBirthPlan = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value birthPlanEndpoint HandleSavedBirthPlan
            , []
            )

        HandleSavedBirthPlan data ->
            ( { model | savePrenatalPhoto = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePregnancyTest personId valueId value ->
            ( { model | savePregnancyTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value pregnancyTestEndpoint HandleSavedPregnancyTest
            , []
            )

        HandleSavedPregnancyTest data ->
            ( { model | savePregnancyTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalSendToHcEndpoint HandleSavedSendToHC
            , []
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveAppointmentConfirmation personId valueId value ->
            ( { model | saveAppointmentConfirmation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value appointmentConfirmationEndpoint HandleSavedAppointmentConfirmation
            , []
            )

        HandleSavedAppointmentConfirmation data ->
            ( { model | saveAppointmentConfirmation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHIVTest personId valueId value ->
            ( { model | saveHIVTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHIVTestEndpoint HandleSavedHIVTest
            , []
            )

        HandleSavedHIVTest data ->
            ( { model | saveHIVTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSyphilisTest personId valueId value ->
            ( { model | saveSyphilisTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalSyphilisTestEndpoint HandleSavedSyphilisTest
            , []
            )

        HandleSavedSyphilisTest data ->
            ( { model | saveSyphilisTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHepatitisBTest personId valueId value ->
            ( { model | saveHepatitisBTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHepatitisBTestEndpoint HandleSavedHepatitisBTest
            , []
            )

        HandleSavedHepatitisBTest data ->
            ( { model | saveHepatitisBTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMalariaTest personId valueId value ->
            ( { model | saveMalariaTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalMalariaTestEndpoint HandleSavedMalariaTest
            , []
            )

        HandleSavedMalariaTest data ->
            ( { model | saveMalariaTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBloodGpRsTest personId valueId value ->
            ( { model | saveBloodGpRsTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalBloodGpRsTestEndpoint HandleSavedBloodGpRsTest
            , []
            )

        HandleSavedBloodGpRsTest data ->
            ( { model | saveBloodGpRsTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveUrineDipstickTest personId valueId value ->
            ( { model | saveUrineDipstickTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalUrineDipstickTestEndpoint HandleSavedUrineDipstickTest
            , []
            )

        HandleSavedUrineDipstickTest data ->
            ( { model | saveUrineDipstickTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHemoglobinTest personId valueId value ->
            ( { model | saveHemoglobinTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHemoglobinTestEndpoint HandleSavedHemoglobinTest
            , []
            )

        HandleSavedHemoglobinTest data ->
            ( { model | saveHemoglobinTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveRandomBloodSugarTest personId valueId value ->
            ( { model | saveRandomBloodSugarTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalRandomBloodSugarTestEndpoint HandleSavedRandomBloodSugarTest
            , []
            )

        HandleSavedRandomBloodSugarTest data ->
            ( { model | saveRandomBloodSugarTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveLabsResults personId valueId value ->
            ( { model | saveLabsResults = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalLabsResultsEndpoint HandleSavedLabsResults
            , []
            )

        HandleSavedLabsResults data ->
            ( { model | saveLabsResults = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMedicationDistribution personId valueId value ->
            ( { model | saveMedicationDistribution = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalMedicationDistributionEndpoint HandleSavedMedicationDistribution
            , []
            )

        HandleSavedMedicationDistribution data ->
            ( { model | saveMedicationDistribution = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomReview personId valueId value ->
            ( { model | saveSymptomReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalSymptomReviewEndpoint HandleSavedSymptomReview
            , []
            )

        HandleSavedSymptomReview data ->
            ( { model | saveSymptomReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveOutsideCare personId valueId value ->
            ( { model | saveOutsideCare = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalOutsideCareEndpoint HandleSavedOutsideCare
            , []
            )

        HandleSavedOutsideCare data ->
            ( { model | saveOutsideCare = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHIVPCRTest personId valueId value ->
            ( { model | saveHIVPCRTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalHIVPCRTestEndpoint HandleSavedHIVPCRTest
            , []
            )

        HandleSavedHIVPCRTest data ->
            ( { model | saveHIVPCRTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePartnerHIVTest personId valueId value ->
            ( { model | savePartnerHIVTest = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalPartnerHIVTestEndpoint HandleSavedPartnerHIVTest
            , []
            )

        HandleSavedPartnerHIVTest data ->
            ( { model | savePartnerHIVTest = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMentalHealth personId valueId value ->
            ( { model | saveMentalHealth = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalMentalHealthEndpoint HandleSavedMentalHealth
            , []
            )

        HandleSavedMentalHealth data ->
            ( { model | saveMentalHealth = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveTetanusImmunisation personId valueId value ->
            ( { model | saveTetanusImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalTetanusImmunisationEndpoint HandleSavedTetanusImmunisation
            , []
            )

        HandleSavedTetanusImmunisation data ->
            ( { model | saveTetanusImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBreastfeeding personId valueId value ->
            ( { model | saveBreastfeeding = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalBreastfeedingEndpoint HandleSavedBreastfeeding
            , []
            )

        HandleSavedBreastfeeding data ->
            ( { model | saveBreastfeeding = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveGUExam personId valueId value ->
            ( { model | saveGUExam = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalGUExamEndpoint HandleSavedGUExam
            , []
            )

        HandleSavedGUExam data ->
            ( { model | saveGUExam = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSpecialityCare personId valueId value ->
            ( { model | saveSpecialityCare = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value prenatalSpecialityCareEndpoint HandleSavedSpecialityCare
            , []
            )

        HandleSavedSpecialityCare data ->
            ( { model | saveSpecialityCare = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateEncounter :
    NominalDate
    -> PrenatalEncounterId
    -> Maybe PrenatalEncounter
    -> (PrenatalEncounter -> PrenatalEncounter)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateEncounter currentDate encounterId maybeEncounter updateFunc model =
    maybeEncounter
        |> unwrap ( model, Cmd.none, [] )
            (\encounter ->
                ( { model | updatePrenatalEncounter = Loading }
                , updateFunc encounter
                    |> sw.patchFull prenatalEncounterEndpoint encounterId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedPrenatalEncounter)
                , []
                )
            )
