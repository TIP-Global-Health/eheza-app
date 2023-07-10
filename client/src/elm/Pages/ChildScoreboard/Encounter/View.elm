module Pages.ChildScoreboard.Encounter.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNewbornExamPregnancySummary
        , resolveNCDANeverFilled
        )
import Backend.Person.Model exposing (Person)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Measurement.Model exposing (NCDAData)
import Measurement.Utils exposing (ncdaFormWithDefault)
import Measurement.View
import Pages.ChildScoreboard.Encounter.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterButton, viewEndEncounterDialog, viewPersonDetails, viewPersonDetailsExtended, viewReportLink)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> ChildScoreboardEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate db model assembled =
    let
        header =
            viewHeader language assembled

        content =
            viewContent language currentDate db model assembled
    in
    div [ class "page-encounter child-scoreboard" ]
        [ header
        , content
        , viewModal <| acuteIllnessEncounterPopup language assembled model
        ]


viewHeader : Language -> AssembledData -> Html Msg
viewHeader language assembled =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <|
                translate language <|
                    Translate.IndividualEncounterLabel
                        Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        True
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| ChildScoreboardParticipantPage assembled.participant.person
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewNCDAContent language currentDate assembled db model.ncdaData
    )
        |> div [ class "ui unstackable items" ]


viewNCDAContent :
    Language
    -> NominalDate
    -> AssembledData
    -> ModelIndexedDb
    -> NCDAData
    -> List (Html Msg)
viewNCDAContent language currentDate assembled db data =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        historyData =
            { pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            }

        config =
            { showTasksTray = True
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setNumberANCVisitsMsg = SetNumberANCVisits
            , setNutritionSupplementTypeMsg = SetNutritionSupplementType
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg =
                if data.form.childGotDiarrhea == Just True then
                    ShowAIEncounterPopup

                else
                    CloseEncounter assembled
            }
    in
    Measurement.View.viewNCDAContent language
        currentDate
        personId
        assembled.person
        config
        data.helperState
        form
        historyData
        db


acuteIllnessEncounterPopup : Language -> AssembledData -> Model -> Maybe (Html Msg)
acuteIllnessEncounterPopup language assembled model =
    if model.showAIEncounterPopup then
        Just <|
            div [ class "ui active modal danger-signs-popup" ]
                [ div [ class "content" ]
                    [ div [ class "popup-heading-wrapper" ]
                        [ img [ src "assets/images/exclamation-red.png" ] []
                        , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                        ]
                    , div [ class "popup-action" ] [ text <| translate language Translate.NCDADiarrheaPopupMessage ]
                    ]
                , div [ class "actions" ]
                    [ button
                        [ class "ui primary fluid button"
                        , onClick <| TriggerAcuteIllnessEncounter assembled
                        ]
                        [ text <| translate language Translate.Continue ]
                    ]
                ]

    else
        Nothing
