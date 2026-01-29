module Backend.Utils exposing (editMeasurementCmd, everySetsEqual, gpsCoordinatesEnabled, groupEducationEnabled, hivManagementEnabled, mapAcuteIllnessMeasurements, mapChildMeasurements, mapChildScoreboardMeasurements, mapFollowUpMeasurements, mapHIVMeasurements, mapHomeVisitMeasurements, mapMotherMeasurements, mapNCDMeasurements, mapNutritionMeasurements, mapPrenatalMeasurements, mapStockManagementMeasurements, mapTuberculosisMeasurements, mapWellChildMeasurements, ncdaEnabled, reportToWhatsAppEnabled, resolveIndividualParticipantForPerson, resolveIndividualParticipantsForPerson, saveMeasurementCmd, stockManagementEnabled, sw, tuberculosisManagementEnabled)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , ChildMeasurementList
        , ChildScoreboardMeasurements
        , FollowUpMeasurements
        , HIVMeasurements
        , HomeVisitMeasurements
        , MotherMeasurementList
        , NCDMeasurements
        , NutritionMeasurements
        , PrenatalMeasurements
        , StockManagementMeasurements
        , TuberculosisMeasurements
        , WellChildMeasurements
        )
import Backend.Model exposing (..)
import EverySet exposing (EverySet)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (applyBackendUrl, toCmd, withoutDecoder)
import SyncManager.Model exposing (SiteFeature(..))


sw : Restful.Endpoint.CrudOperations w e k v c p
sw =
    applyBackendUrl "/sw"


mapChildMeasurements : PersonId -> (ChildMeasurementList -> ChildMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapChildMeasurements childId func model =
    let
        mapped =
            Dict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\measurements ->
                        Dict.insert childId (func measurements |> Success) model.childMeasurements
                    )
                |> Maybe.withDefault model.childMeasurements
    in
    { model | childMeasurements = mapped }


mapMotherMeasurements : PersonId -> (MotherMeasurementList -> MotherMeasurementList) -> ModelIndexedDb -> ModelIndexedDb
mapMotherMeasurements motherId func model =
    let
        mapped =
            Dict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\measurements ->
                        Dict.insert motherId (func measurements |> Success) model.motherMeasurements
                    )
                |> Maybe.withDefault model.motherMeasurements
    in
    { model | motherMeasurements = mapped }


mapPrenatalMeasurements : Maybe PrenatalEncounterId -> (PrenatalMeasurements -> PrenatalMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapPrenatalMeasurements id func model =
    case id of
        Just encounterId ->
            { model | prenatalMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.prenatalMeasurements }

        Nothing ->
            model


mapNutritionMeasurements : Maybe NutritionEncounterId -> (NutritionMeasurements -> NutritionMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapNutritionMeasurements id func model =
    case id of
        Just encounterId ->
            { model | nutritionMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.nutritionMeasurements }

        Nothing ->
            model


mapAcuteIllnessMeasurements : Maybe AcuteIllnessEncounterId -> (AcuteIllnessMeasurements -> AcuteIllnessMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapAcuteIllnessMeasurements id func model =
    case id of
        Just encounterId ->
            { model | acuteIllnessMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.acuteIllnessMeasurements }

        Nothing ->
            model


mapFollowUpMeasurements : Maybe HealthCenterId -> (FollowUpMeasurements -> FollowUpMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapFollowUpMeasurements id func model =
    case id of
        Just healthCenterId ->
            let
                mapped =
                    Dict.get healthCenterId model.followUpMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (\measurements ->
                                Dict.insert healthCenterId (func measurements |> Success) model.followUpMeasurements
                            )
                        |> Maybe.withDefault model.followUpMeasurements
            in
            { model | followUpMeasurements = mapped }

        Nothing ->
            model


mapHomeVisitMeasurements : Maybe HomeVisitEncounterId -> (HomeVisitMeasurements -> HomeVisitMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapHomeVisitMeasurements id func model =
    case id of
        Just encounterId ->
            { model | homeVisitMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.homeVisitMeasurements }

        Nothing ->
            model


mapWellChildMeasurements : Maybe WellChildEncounterId -> (WellChildMeasurements -> WellChildMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapWellChildMeasurements id func model =
    case id of
        Just encounterId ->
            { model | wellChildMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.wellChildMeasurements }

        Nothing ->
            model


mapNCDMeasurements : Maybe NCDEncounterId -> (NCDMeasurements -> NCDMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapNCDMeasurements id func model =
    case id of
        Just encounterId ->
            { model | ncdMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.ncdMeasurements }

        Nothing ->
            model


mapChildScoreboardMeasurements : Maybe ChildScoreboardEncounterId -> (ChildScoreboardMeasurements -> ChildScoreboardMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapChildScoreboardMeasurements id func model =
    case id of
        Just encounterId ->
            { model | childScoreboardMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.childScoreboardMeasurements }

        Nothing ->
            model


mapTuberculosisMeasurements : Maybe TuberculosisEncounterId -> (TuberculosisMeasurements -> TuberculosisMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapTuberculosisMeasurements id func model =
    case id of
        Just encounterId ->
            { model | tuberculosisMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.tuberculosisMeasurements }

        Nothing ->
            model


mapHIVMeasurements : Maybe HIVEncounterId -> (HIVMeasurements -> HIVMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapHIVMeasurements id func model =
    case id of
        Just encounterId ->
            { model | hivMeasurements = Dict.update encounterId (Maybe.map (RemoteData.map func)) model.hivMeasurements }

        Nothing ->
            model


mapStockManagementMeasurements : Maybe HealthCenterId -> (StockManagementMeasurements -> StockManagementMeasurements) -> ModelIndexedDb -> ModelIndexedDb
mapStockManagementMeasurements id func model =
    case id of
        Just healthCenterId ->
            let
                mapped =
                    Dict.get healthCenterId model.stockManagementMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (\measurements ->
                                Dict.insert healthCenterId (func measurements |> Success) model.stockManagementMeasurements
                            )
                        |> Maybe.withDefault model.stockManagementMeasurements
            in
            { model | stockManagementMeasurements = mapped }

        Nothing ->
            model


saveMeasurementCmd date encounter person nurse healthCenter savedValueId savedValue endpoint handleSavedMsg =
    let
        measurement =
            { participantId = person
            , dateMeasured = date
            , encounterId = Just encounter
            , nurse = nurse
            , healthCenter = healthCenter
            , deleted = False
            , value = savedValue
            }

        requestData =
            case savedValueId of
                Nothing ->
                    measurement
                        |> sw.post endpoint
                        |> withoutDecoder

                Just id ->
                    measurement
                        |> sw.patchFull endpoint id
                        |> withoutDecoder
    in
    toCmd (RemoteData.fromResult >> handleSavedMsg) requestData


editMeasurementCmd id updatedValue endpoint handleSavedMsg measurement =
    { measurement | value = updatedValue }
        |> sw.patchFull endpoint id
        |> withoutDecoder
        |> toCmd (RemoteData.fromResult >> handleSavedMsg)


resolveIndividualParticipantsForPerson : PersonId -> IndividualEncounterType -> ModelIndexedDb -> List IndividualEncounterParticipantId
resolveIndividualParticipantsForPerson personId encounterType db =
    Dict.get personId db.individualParticipantsByPerson
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (Dict.toList
                >> List.filterMap
                    (\( participantId, participant ) ->
                        if participant.encounterType == encounterType then
                            Just participantId

                        else
                            Nothing
                    )
            )
        |> Maybe.withDefault []


resolveIndividualParticipantForPerson : PersonId -> IndividualEncounterType -> ModelIndexedDb -> Maybe IndividualEncounterParticipantId
resolveIndividualParticipantForPerson personId encounterType db =
    resolveIndividualParticipantsForPerson personId encounterType db
        |> List.head


everySetsEqual : EverySet a -> EverySet a -> Bool
everySetsEqual set1 set2 =
    let
        size1 =
            EverySet.size set1

        size2 =
            EverySet.size set2

        sizeIntersect =
            EverySet.intersect set1 set2
                |> EverySet.size
    in
    (size1 == size2) && (size1 == sizeIntersect)



-- FEATURES ON/OFF


ncdaEnabled : EverySet SiteFeature -> Bool
ncdaEnabled =
    EverySet.member FeatureNCDA


reportToWhatsAppEnabled : EverySet SiteFeature -> Bool
reportToWhatsAppEnabled =
    EverySet.member FeatureReportToWhatsApp


stockManagementEnabled : EverySet SiteFeature -> Bool
stockManagementEnabled =
    EverySet.member FeatureStockManagement


tuberculosisManagementEnabled : EverySet SiteFeature -> Bool
tuberculosisManagementEnabled =
    EverySet.member FeatureTuberculosisManagement


groupEducationEnabled : EverySet SiteFeature -> Bool
groupEducationEnabled =
    EverySet.member FeatureGroupEducation


gpsCoordinatesEnabled : EverySet SiteFeature -> Bool
gpsCoordinatesEnabled =
    EverySet.member FeatureGPSCoordinates


hivManagementEnabled : EverySet SiteFeature -> Bool
hivManagementEnabled =
    EverySet.member FeatureHIVManagement
