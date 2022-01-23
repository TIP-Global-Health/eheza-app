module Pages.PatientRecord.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (sortByDate)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInYears, generateFullName, isPersonAnAdult)
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import Pages.WellChildEncounter.View exposing (thumbnailDimensions)
import Pages.WellChildProgressReport.Model exposing (WellChildProgressReportInitiator(..))
import Pages.WellChildProgressReport.View exposing (viewProgressReport)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (spinner, thumbnailImage, viewModal)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw db model =
    Dict.get id db.people
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\person ->
                if isPersonAnAdult currentDate person == Just False then
                    viewContentForChild language currentDate zscores id person isChw db model

                else
                    viewContentForAdult language currentDate id person db model
            )
        |> Maybe.withDefault spinner


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PersonsPage Nothing ParticipantDirectoryOrigin
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContentForChild : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> Person -> Bool -> ModelIndexedDb -> Model -> Html Msg
viewContentForChild language currentDate zscores childId child isChw db model =
    let
        endEncounterData =
            Just <|
                { showEndEncounterDialog = False
                , allowEndEcounter = False
                , closeEncounterMsg = NoOp
                , setEndEncounterDialogStateMsg = always NoOp
                }

        mandatoryNutritionAssessmentMeasurementsTaken =
            False

        initiator =
            InitiatorPatientRecordChild childId
    in
    viewProgressReport language
        currentDate
        zscores
        isChw
        initiator
        mandatoryNutritionAssessmentMeasurementsTaken
        db
        model.diagnosisMode
        SetActivePage
        SetDiagnosisMode
        endEncounterData
        ( childId, child )


viewContentForAdult : Language -> NominalDate -> PersonId -> Person -> ModelIndexedDb -> Model -> Html Msg
viewContentForAdult language currentDate personId person db model =
    div [ class "page-activity patient-record" ]
        [ viewHeader language
        , div [ class "ui unstackable items" ]
            [ div [ class "item" ] <|
                viewAdultDetails
                    language
                    currentDate
                    personId
                    person
                    db
            ]
        ]


viewAdultDetails : Language -> NominalDate -> PersonId -> Person -> ModelIndexedDb -> List (Html Msg)
viewAdultDetails language currentDate personId person db =
    let
        ( thumbnailClass, ageEntry ) =
            ( "mother"
            , ageInYears currentDate person
                |> Maybe.map (\ageYears -> viewTransEntry Translate.AgeWord (Translate.YearsOld ageYears |> translate language))
                |> Maybe.withDefault emptyNode
            )

        dateOfBirthEntry =
            Maybe.map
                (\birthDate ->
                    viewTransEntry Translate.DateOfBirth (formatDDMMYYYY birthDate)
                )
                person.birthDate
                |> Maybe.withDefault emptyNode

        ubudeheEntry =
            Maybe.map (Translate.UbudeheNumber >> translate language >> viewTransEntry Translate.UbudeheLabel) person.ubudehe
                |> Maybe.withDefault emptyNode

        educationLevelEntry =
            Maybe.map (Translate.LevelOfEducation >> translate language >> viewTransEntry Translate.LevelOfEducationLabel) person.educationLevel
                |> Maybe.withDefault emptyNode

        childrenData =
            Dict.get personId db.relationshipsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.values
                        >> List.filterMap
                            (\relationship ->
                                if relationship.relatedBy == MyChild then
                                    Dict.get relationship.relatedTo db.people
                                        |> Maybe.andThen RemoteData.toMaybe
                                        |> Maybe.map (\child -> ( relationship.relatedTo, child ))

                                else
                                    Nothing
                            )
                        >> List.sortWith (sortByDate (Tuple.second >> .birthDate >> Maybe.withDefault currentDate))
                    )
                |> Maybe.withDefault []

        childrenList =
            List.indexedMap
                (\index ( _, child ) ->
                    viewEntry (translate language Translate.Baby ++ " " ++ String.fromInt (index + 1)) child.name
                )
                childrenData

        familyLinks =
            let
                childrenMarkup =
                    List.indexedMap viewChildMarkup childrenData

                viewChildMarkup index ( childId, _ ) =
                    li [ onClick <| SetActivePage <| UserPage <| PatientRecordPage childId ]
                        [ span [ class "icon" ]
                            [ span [ class "icon-baby" ] []
                            , span
                                [ class "count" ]
                                [ text <| String.fromInt (index + 1) ]
                            ]
                        ]

                motherMarkup =
                    li [ class "active" ]
                        [ span [ class "icon" ]
                            [ span
                                [ class "icon-mother" ]
                                []
                            ]
                        ]
            in
            motherMarkup
                :: childrenMarkup
                |> ul [ class "links-body" ]

        viewTransEntry labelTransId content =
            viewEntry (translate language labelTransId) content

        viewEntry label content =
            p []
                [ span [ class "label" ] [ text <| label ++ ": " ]
                , span [] [ text content ]
                ]
    in
    [ div [ class "ui image" ]
        [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
    , div [ class "details" ] <|
        [ h2 [ class "ui header" ]
            [ text person.name ]
        , ageEntry
        , dateOfBirthEntry
        , ubudeheEntry
        , educationLevelEntry
        ]
            ++ childrenList
            ++ [ familyLinks ]
    ]
