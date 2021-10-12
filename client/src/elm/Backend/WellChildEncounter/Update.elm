module Backend.WellChildEncounter.Update exposing (update)

import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Encoder exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..))
import Backend.Utils exposing (saveMeasurementCmd, sw)
import Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)
import Backend.WellChildEncounter.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
import Json.Encode.Extra
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update : Maybe NurseId -> Maybe HealthCenterId -> WellChildEncounterId -> Maybe WellChildEncounter -> NominalDate -> Msg -> Model -> ( Model, Cmd Msg )
update nurseId healthCenterId encounterId maybeEncounter currentDate msg model =
    case msg of
        CloseWellChildEncounter ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | editWellChildEncounter = Loading }
                        , { encounter | endDate = Just currentDate }
                            |> sw.patchFull wellChildEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleWellChildEncounterEdited)
                        )
                    )

        HandleWellChildEncounterEdited data ->
            ( { model | editWellChildEncounter = data }
            , Cmd.none
            )

        SetWellChildEncounterNote note ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
                        ( { model | editWellChildEncounter = Loading }
                        , { encounter | encounterNote = note }
                            |> sw.patchFull wellChildEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleWellChildEncounterEdited)
                        )
                    )

        SetWellChildEncounterWarning warning ->
            maybeEncounter
                |> unwrap ( model, Cmd.none )
                    (\encounter ->
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
                        ( { model | editWellChildEncounter = Loading }
                        , { encounter | encounterWarnings = EverySet.fromList updatedWarnings }
                            |> sw.patchFull wellChildEncounterEndpoint encounterId
                            |> withoutDecoder
                            |> toCmd (RemoteData.fromResult >> HandleWellChildEncounterEdited)
                        )
                    )

        SavePregnancySummary personId valueId value ->
            ( { model | savePregnancySummary = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPregnancySummaryEndpoint HandleSavedPregnancySummary
            )

        HandleSavedPregnancySummary data ->
            ( { model | savePregnancySummary = data }
            , Cmd.none
            )

        SaveSymptomsReview personId valueId value ->
            ( { model | saveSymptomsReview = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildSymptomsReviewEndpoint HandleSavedSymptomsReview
            )

        HandleSavedSymptomsReview data ->
            ( { model | saveSymptomsReview = data }
            , Cmd.none
            )

        SaveVitals personId valueId value ->
            ( { model | saveVitals = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildVitalsEndpoint HandleSavedVitals
            )

        HandleSavedVitals data ->
            ( { model | saveVitals = data }
            , Cmd.none
            )

        SaveHeight personId valueId value ->
            ( { model | saveHeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeightEndpoint HandleSavedHeight
            )

        HandleSavedHeight data ->
            ( { model | saveHeight = data }
            , Cmd.none
            )

        SaveHeadCircumference personId valueId value ->
            ( { model | saveHeadCircumference = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHeadCircumferenceEndpoint HandleSavedHeadCircumference
            )

        HandleSavedHeadCircumference data ->
            ( { model | saveHeadCircumference = data }
            , Cmd.none
            )

        SaveMuac personId valueId value ->
            ( { model | saveMuac = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMuacEndpoint HandleSavedMuac
            )

        HandleSavedMuac data ->
            ( { model | saveMuac = data }
            , Cmd.none
            )

        SaveNutrition personId valueId value ->
            ( { model | saveNutrition = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNutritionEndpoint HandleSavedNutrition
            )

        HandleSavedNutrition data ->
            ( { model | saveNutrition = data }
            , Cmd.none
            )

        SavePhoto personId valueId value ->
            ( { model | savePhoto = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPhotoEndpoint HandleSavedPhoto
            )

        HandleSavedPhoto data ->
            ( { model | savePhoto = data }
            , Cmd.none
            )

        SaveWeight personId valueId value ->
            ( { model | saveWeight = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildWeightEndpoint HandleSavedWeight
            )

        HandleSavedWeight data ->
            ( { model | saveWeight = data }
            , Cmd.none
            )

        SaveContributingFactors personId valueId value ->
            ( { model | saveContributingFactors = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildContributingFactorsEndpoint HandleSavedContributingFactors
            )

        HandleSavedContributingFactors data ->
            ( { model | saveContributingFactors = data }
            , Cmd.none
            )

        SaveHealthEducation personId valueId value ->
            ( { model | saveHealthEducation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHealthEducationEndpoint HandleSavedHealthEducation
            )

        HandleSavedHealthEducation data ->
            ( { model | saveHealthEducation = data }
            , Cmd.none
            )

        SaveFollowUp personId valueId value ->
            ( { model | saveFollowUp = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildFollowUpEndpoint HandleSavedFollowUp
            )

        HandleSavedFollowUp data ->
            ( { model | saveFollowUp = data }
            , Cmd.none
            )

        SaveSendToHC personId valueId value ->
            ( { model | saveSendToHC = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildSendToHCEndpoint HandleSavedSendToHC
            )

        HandleSavedSendToHC data ->
            ( { model | saveSendToHC = data }
            , Cmd.none
            )

        SaveBCGImmunisation personId valueId value ->
            ( { model | saveBCGImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildBCGImmunisationEndpoint HandleSavedBCGImmunisation
            )

        HandleSavedBCGImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveDTPImmunisation personId valueId value ->
            ( { model | saveDTPImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildDTPImmunisationEndpoint HandleSavedDTPImmunisation
            )

        HandleSavedDTPImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveHPVImmunisation personId valueId value ->
            ( { model | saveHPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildHPVImmunisationEndpoint HandleSavedHPVImmunisation
            )

        HandleSavedHPVImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveIPVImmunisation personId valueId value ->
            ( { model | saveIPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildIPVImmunisationEndpoint HandleSavedIPVImmunisation
            )

        HandleSavedIPVImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveMRImmunisation personId valueId value ->
            ( { model | saveMRImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMRImmunisationEndpoint HandleSavedMRImmunisation
            )

        HandleSavedMRImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveOPVImmunisation personId valueId value ->
            ( { model | saveOPVImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildOPVImmunisationEndpoint HandleSavedOPVImmunisation
            )

        HandleSavedOPVImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SavePCV13Immunisation personId valueId value ->
            ( { model | savePCV13Immunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildPCV13ImmunisationEndpoint HandleSavedPCV13Immunisation
            )

        HandleSavedPCV13Immunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveRotarixImmunisation personId valueId value ->
            ( { model | saveRotarixImmunisation = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildRotarixImmunisationEndpoint HandleSavedRotarixImmunisation
            )

        HandleSavedRotarixImmunisation data ->
            ( { model | saveImmunisation = data }
            , Cmd.none
            )

        SaveECD personId valueId value ->
            ( { model | saveECD = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildECDEndpoint HandleSavedECD
            )

        HandleSavedECD data ->
            ( { model | saveECD = data }
            , Cmd.none
            )

        SaveAlbendazole personId valueId value ->
            ( { model | saveAlbendazole = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildAlbendazoleEndpoint HandleSavedAlbendazole
            )

        HandleSavedAlbendazole data ->
            ( { model | saveAlbendazole = data }
            , Cmd.none
            )

        SaveMebendezole personId valueId value ->
            ( { model | saveMebendezole = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildMebendezoleEndpoint HandleSavedMebendezole
            )

        HandleSavedMebendezole data ->
            ( { model | saveMebendezole = data }
            , Cmd.none
            )

        SaveVitaminA personId valueId value ->
            ( { model | saveVitaminA = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildVitaminAEndpoint HandleSavedVitaminA
            )

        HandleSavedVitaminA data ->
            ( { model | saveVitaminA = data }
            , Cmd.none
            )

        SaveNextVisit personId valueId value ->
            ( { model | saveNextVisit = Loading }
            , saveMeasurementCmd currentDate encounterId personId nurseId healthCenterId valueId value wellChildNextVisitEndpoint HandleSavedNextVisit
            )

        HandleSavedNextVisit data ->
            ( { model | saveNextVisit = data }
            , Cmd.none
            )
