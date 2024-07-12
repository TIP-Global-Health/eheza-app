module Backend.WellChildEncounter.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Backend.WellChildEncounter.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> WellChildEncounterId
    -> Maybe WellChildEncounter
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate nurseId healthCenterId encounterId maybeEncounter msg model =
    case msg of
        CloseWellChildEncounter ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | endDate = Just currentDate }) model

        SetWellChildEncounterNote note ->
            updateEncounter currentDate encounterId maybeEncounter (\encounter -> { encounter | encounterNote = note }) model

        SetWellChildEncounterWarning warning ->
            let
                updateFunc =
                    \encounter ->
                        let
                            warnings =
                                EverySet.toList encounter.encounterWarnings

                            updatedWarnings =
                                if List.member warning ecdMilestoneWarnings then
                                    List.filter (\item -> not (List.member item (NoEncounterWarnings :: ecdMilestoneWarnings))) warnings
                                        |> List.append [ warning ]

                                else if List.member warning headCircumferenceWarnings then
                                    List.filter (\item -> not (List.member item (NoEncounterWarnings :: headCircumferenceWarnings))) warnings
                                        |> List.append [ warning ]

                                else
                                    [ NoEncounterWarnings ]
                        in
                        { encounter | encounterWarnings = EverySet.fromList updatedWarnings }
            in
            updateEncounter currentDate encounterId maybeEncounter updateFunc model

        HandleUpdatedWellChildEncounter data ->
            ( { model | editWellChildEncounter = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePregnancySummary personId valueId value ->
            ( { model | savePregnancySummary = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPregnancySummaryEndpoint HandleSavedPregnancySummary
            , []
            )

        HandleSavedPregnancySummary data ->
            ( { model | savePregnancySummary = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSymptomsReview personId valueId value ->
            ( { model | saveSymptomsReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildSymptomsReviewEndpoint HandleSavedSymptomsReview
            , []
            )

        HandleSavedSymptomsReview data ->
            ( { model | saveSymptomsReview = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildVitalsEndpoint HandleSavedVitals
            , []
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeightEndpoint HandleSavedHeight
            , []
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHeadCircumference personId valueId value ->
            ( { model | saveHeadCircumference = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeadCircumferenceEndpoint HandleSavedHeadCircumference
            , []
            )

        HandleSavedHeadCircumference data ->
            ( { model | saveHeadCircumference = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMuacEndpoint HandleSavedMuac
            , []
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNutritionEndpoint HandleSavedNutrition
            , []
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePhoto personId valueId value ->
            ( { model | savePhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPhotoEndpoint HandleSavedPhoto
            , []
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildWeightEndpoint HandleSavedWeight
            , []
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveContributingFactors personId valueId value ->
            ( { model | saveContributingFactors = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildContributingFactorsEndpoint HandleSavedContributingFactors
            , []
            )

        HandleSavedContributingFactors data ->
            ( { model | saveContributingFactors = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHealthEducationEndpoint HandleSavedHealthEducation
            , []
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildFollowUpEndpoint HandleSavedFollowUp
            , []
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildSendToHCEndpoint HandleSavedSendToHC
            , []
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveBCGImmunisation personId valueId value ->
            ( { model | saveBCGImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildBCGImmunisationEndpoint HandleSavedBCGImmunisation
            , []
            )

        HandleSavedBCGImmunisation data ->
            ( { model | saveBCGImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDTPImmunisation personId valueId value ->
            ( { model | saveDTPImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildDTPImmunisationEndpoint HandleSavedDTPImmunisation
            , []
            )

        HandleSavedDTPImmunisation data ->
            ( { model | saveDTPImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveDTPStandaloneImmunisation personId valueId value ->
            ( { model | saveDTPStandaloneImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildDTPStandaloneImmunisationEndpoint HandleSavedDTPStandaloneImmunisation
            , []
            )

        HandleSavedDTPStandaloneImmunisation data ->
            ( { model | saveDTPStandaloneImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHPVImmunisation personId valueId value ->
            ( { model | saveHPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHPVImmunisationEndpoint HandleSavedHPVImmunisation
            , []
            )

        HandleSavedHPVImmunisation data ->
            ( { model | saveHPVImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveIPVImmunisation personId valueId value ->
            ( { model | saveIPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildIPVImmunisationEndpoint HandleSavedIPVImmunisation
            , []
            )

        HandleSavedIPVImmunisation data ->
            ( { model | saveIPVImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMRImmunisation personId valueId value ->
            ( { model | saveMRImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMRImmunisationEndpoint HandleSavedMRImmunisation
            , []
            )

        HandleSavedMRImmunisation data ->
            ( { model | saveMRImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveOPVImmunisation personId valueId value ->
            ( { model | saveOPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildOPVImmunisationEndpoint HandleSavedOPVImmunisation
            , []
            )

        HandleSavedOPVImmunisation data ->
            ( { model | saveOPVImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SavePCV13Immunisation personId valueId value ->
            ( { model | savePCV13Immunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPCV13ImmunisationEndpoint HandleSavedPCV13Immunisation
            , []
            )

        HandleSavedPCV13Immunisation data ->
            ( { model | savePCV13Immunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveRotarixImmunisation personId valueId value ->
            ( { model | saveRotarixImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildRotarixImmunisationEndpoint HandleSavedRotarixImmunisation
            , []
            )

        HandleSavedRotarixImmunisation data ->
            ( { model | saveRotarixImmunisation = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveECD personId valueId value ->
            ( { model | saveECD = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildECDEndpoint HandleSavedECD
            , []
            )

        HandleSavedECD data ->
            ( { model | saveECD = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveAlbendazole personId valueId value ->
            ( { model | saveAlbendazole = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildAlbendazoleEndpoint HandleSavedAlbendazole
            , []
            )

        HandleSavedAlbendazole data ->
            ( { model | saveAlbendazole = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveMebendezole personId valueId value ->
            ( { model | saveMebendezole = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMebendezoleEndpoint HandleSavedMebendezole
            , []
            )

        HandleSavedMebendezole data ->
            ( { model | saveMebendezole = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveVitaminA personId valueId value ->
            ( { model | saveVitaminA = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildVitaminAEndpoint HandleSavedVitaminA
            , []
            )

        HandleSavedVitaminA data ->
            ( { model | saveVitaminA = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNextVisit personId valueId value ->
            ( { model | saveNextVisit = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNextVisitEndpoint HandleSavedNextVisit
            , []
            )

        HandleSavedNextVisit data ->
            ( { model | saveNextVisit = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveNCDA personId valueId value ->
            ( { model | saveNCDA = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNCDAEndpoint HandleSavedNCDA
            , []
            )

        HandleSavedNCDA data ->
            ( { model | saveNCDA = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFeeding personId valueId value ->
            ( { model | saveFeeding = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildFeedingEndpoint HandleSavedFeeding
            , []
            )

        HandleSavedFeeding data ->
            ( { model | saveFeeding = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveHygiene personId valueId value ->
            ( { model | saveHygiene = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHygieneEndpoint HandleSavedHygiene
            , []
            )

        HandleSavedHygiene data ->
            ( { model | saveHygiene = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveFoodSecurity personId valueId value ->
            ( { model | saveFoodSecurity = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildFoodSecurityEndpoint HandleSavedFoodSecurity
            , []
            )

        HandleSavedFoodSecurity data ->
            ( { model | saveFoodSecurity = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )

        SaveCaring personId valueId value ->
            ( { model | saveCaring = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildCaringEndpoint HandleSavedCaring
            , []
            )

        HandleSavedCaring data ->
            ( { model | saveCaring = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateEncounter :
    NominalDate
    -> WellChildEncounterId
    -> Maybe WellChildEncounter
    -> (WellChildEncounter -> WellChildEncounter)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateEncounter currentDate encounterId maybeEncounter updateFunc model =
    maybeEncounter
        |> unwrap ( model, Cmd.none, [] )
            (\encounter ->
                ( { model | editWellChildEncounter = Loading }
                , updateFunc encounter
                    |> sw.patchFull wellChildEncounterEndpoint encounterId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedWellChildEncounter)
                , []
                )
            )
