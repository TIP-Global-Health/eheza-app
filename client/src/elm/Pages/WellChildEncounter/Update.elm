module Pages.WellChildEncounter.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.WellChildActivity.Model
import Pages.WellChildActivity.Utils
import Pages.WellChildEncounter.Model exposing (..)
import Pages.WellChildEncounter.Utils
import RemoteData
import ZScore.Model


update : NominalDate -> ZScore.Model.Model -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate zscores isChw db msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.WellChildEncounter.Model.CloseWellChildEncounter
                    |> Backend.Model.MsgWellChildEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        NavigateToAcuteIllnessParticipantPage childId encounterId ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (AcuteIllnessParticipantPage childId))
                    , SetWarningPopupState Nothing
                    , CloseEncounter encounterId
                    ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate zscores isChw db) extraMsgs

        NavigateToActivity encounterId activity ->
            let
                extraMsgs =
                    [ SetActivePage (UserPage (WellChildActivityPage encounterId activity)) ]

                appMsgs =
                    if activity == WellChildNextSteps then
                        Maybe.map
                            (\assembled ->
                                let
                                    mandatoryActivitiesCompleted =
                                        Pages.WellChildActivity.Utils.mandatoryNutritionAssessmentTasksCompleted
                                            currentDate
                                            isChw
                                            assembled
                                            db
                                in
                                if not mandatoryActivitiesCompleted then
                                    -- Assement is done only when all mandatory measurements were recorded.
                                    []

                                else
                                    let
                                        assessment =
                                            Pages.WellChildActivity.Utils.generateNutritionAssessment currentDate zscores db assembled
                                    in
                                    if List.isEmpty assessment then
                                        []

                                    else
                                        -- Show warning popup with new assesment.
                                        Pages.WellChildActivity.Model.SetWarningPopupState assessment
                                            |> App.Model.MsgPageWellChildActivity encounterId Backend.WellChildActivity.Model.WellChildNextSteps
                                            |> App.Model.MsgLoggedIn
                                            |> List.singleton
                            )
                            (RemoteData.toMaybe <| Pages.WellChildEncounter.Utils.generateAssembledData encounterId db)
                            |> Maybe.withDefault []

                    else
                        []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate zscores isChw db) extraMsgs
