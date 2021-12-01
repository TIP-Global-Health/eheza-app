module Pages.NutritionProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (HeightInCm(..), MuacInCm(..), NutritionHeight, NutritionWeight, WeightInKg(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust)
import Pages.NutritionActivity.Utils exposing (mandatoryActivitiesCompleted)
import Pages.NutritionEncounter.Model exposing (AssembledData)
import Pages.NutritionEncounter.Utils exposing (generateAssembledData)
import Pages.NutritionEncounter.View exposing (allowEndingEcounter, partitionActivities)
import Pages.NutritionProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChildProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.NominalDate exposing (renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Days(..), Kilograms(..), Length(..), Months(..), ZScore)
import ZScore.Utils exposing (diffDays)
import ZScore.View


view : Language -> NominalDate -> ZScore.Model.Model -> NutritionEncounterId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    let
        encounter =
            Dict.get id db.nutritionEncounters
                |> Maybe.withDefault NotAsked

        participant =
            RemoteData.andThen
                (\encounter_ ->
                    Dict.get encounter_.participant db.individualParticipants
                        |> Maybe.withDefault NotAsked
                )
                encounter

        childData =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (\child_ -> ( participant_.person, child_ ))
                    )

        assembledData =
            generateAssembledData id db
                |> RemoteData.toMaybe

        ( endEnconterData, mandatoryNutritionAssessmentMeasurementsTaken ) =
            Maybe.map2
                (\assembled ( _, child ) ->
                    let
                        ( _, pendingActivities ) =
                            partitionActivities currentDate zscores isChw db assembled
                    in
                    ( Just <|
                        { showEndEncounterDialog = model.showEndEncounterDialog
                        , allowEndEcounter = allowEndingEcounter isChw pendingActivities
                        , closeEncounterMsg = CloseEncounter id
                        , setEndEncounterDialogStateMsg = SetEndEncounterDialogState
                        }
                    , mandatoryActivitiesCompleted currentDate zscores child isChw assembled db
                    )
                )
                assembledData
                (RemoteData.toMaybe childData)
                |> Maybe.withDefault ( Nothing, False )

        initiator =
            InitiatorNutritionIndividual id
    in
    viewWebData language
        (viewProgressReport language
            currentDate
            zscores
            isChw
            initiator
            mandatoryNutritionAssessmentMeasurementsTaken
            db
            model.diagnosisMode
            SetActivePage
            SetDiagnosisMode
            endEnconterData
        )
        identity
        childData
