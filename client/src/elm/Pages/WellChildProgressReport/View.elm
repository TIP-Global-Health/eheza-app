module Pages.WellChildProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isNothing)
import Measurement.View exposing (renderDatePart, viewActionTakenLabel)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import Pages.WellChildActivity.Model
import Pages.WellChildEncounter.Model exposing (AssembledData)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import Pages.WellChildProgressReport.Model exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 140
    , height = 140
    }


view : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id db model) identity assembled


viewContent : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id db model assembled =
    div [ class "page-report well-child" ]
        [ viewHeader language id
        , div [ class "ui report unstackable items" ]
            [ viewPersonInfoPane language currentDate assembled.person
            , viewActiveDiagnosisPane language currentDate id db model assembled
            ]

        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> WellChildEncounterId -> Html Msg
viewHeader language id =
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language <| Translate.ProgressReport
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage (WellChildEncounterPage id))
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPersonInfoPane : Language -> NominalDate -> Person -> Html Msg
viewPersonInfoPane language currentDate person =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ": " ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div
            [ class "pane-content" ]
            [ div [ class "ui image" ]
                [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "details" ]
                [ h2 [ class "ui header" ]
                    [ text person.name ]
                , viewAge
                , viewVillage
                ]
            ]
        ]


viewActiveDiagnosisPane : Language -> NominalDate -> WellChildEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewActiveDiagnosisPane language currentDate id db model assembled =
    let
        sessions =
            Dict.get assembled.participant.person db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (Dict.toList
                        >> List.filter
                            (\( sessionId, session ) ->
                                session.encounterType == Backend.IndividualEncounterParticipant.Model.AcuteIllnessEncounter
                            )
                    )
                |> Maybe.withDefault []

        _ =
            Debug.log "sessions" sessions
    in
    div [ class "pane active-diagnosis" ]
        [ viewPaneHeading language Translate.ActiveDiagnosis
        ]


viewPaneHeading : Language -> TranslationId -> Html Msg
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]
