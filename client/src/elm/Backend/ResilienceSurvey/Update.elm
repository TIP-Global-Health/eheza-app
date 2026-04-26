module Backend.ResilienceSurvey.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (resilienceSurveyEndpoint)
import Backend.ResilienceSurvey.Model exposing (Model, Msg(..), ResilienceSurvey)
import Backend.Utils exposing (sw)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CreateResilienceSurvey survey ->
            createResilienceSurvey survey model

        HandleCreatedResilienceSurvey data ->
            ( { model | createResilienceSurvey = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


createResilienceSurvey : ResilienceSurvey -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
createResilienceSurvey survey model =
    ( { model | createResilienceSurvey = Loading }
    , sw.post resilienceSurveyEndpoint survey
        |> toCmd (RemoteData.fromResult >> HandleCreatedResilienceSurvey)
    , []
    )
