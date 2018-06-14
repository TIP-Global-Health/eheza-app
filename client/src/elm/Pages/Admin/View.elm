module Pages.Admin.View exposing (view)

import Backend.Clinic.Model exposing (Clinic)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelBackend)
import Backend.Session.Model exposing (Session)
import Backend.Session.Form exposing (..)
import Config.Model as Config
import EveryDictList exposing (EveryDictList)
import EverySet
import Form
import Form.Input exposing (..)
import Gizra.Html exposing (showIf, emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Admin.Model exposing (..)
import Pages.Page exposing (..)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityId)
import Translate exposing (Language, translate)
import Time.Date
import User.Model exposing (..)
import Utils.WebData exposing (viewOrFetch, viewError)


view : Config.Model -> Language -> NominalDate -> User -> ModelBackend -> Model -> Html Msg
view config language currentDate user backend model =
    let
        content =
            if EverySet.member Administrator user.roles then
                contentForAdmin config language currentDate backend model
            else
                contentForOthers language
    in
        div [ class "wrap wrap-alt-2 admin-page" ]
            [ div
                [ class "ui basic head segment" ]
                [ h1
                    [ class "ui header" ]
                    [ text <| translate language Translate.Admin ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage LoginPage
                    ]
                    [ span [ class "icon-back" ] []
                    , span [] []
                    ]
                ]
            , div [ class "ui basic segment" ] [ content ]
            ]


contentForOthers : Language -> Html Msg
contentForOthers language =
    div [ class "ui basic segment" ]
        [ text <| translate language Translate.YouAreNotAnAdmin ]


contentForAdmin : Config.Model -> Language -> NominalDate -> ModelBackend -> Model -> Html Msg
contentForAdmin config language currentDate backend model =
    div [] <|
        viewOrFetch language
            (MsgBackend Backend.Model.FetchClinics)
            (viewLoadedClinics config language currentDate backend model)
            identity
            backend.clinics


viewLoadedClinics : Config.Model -> Language -> NominalDate -> ModelBackend -> Model -> EveryDictList ClinicId Clinic -> List (Html Msg)
viewLoadedClinics config language currentDate backend model clinics =
    viewOrFetch language
        (MsgBackend <| Backend.Model.FetchFutureSessions currentDate)
        (viewLoadedSessions config language backend model clinics)
        identity
        backend.futureSessions


viewLoadedSessions : Config.Model -> Language -> ModelBackend -> Model -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewLoadedSessions config language backend model clinics sessions =
    case model.createSession of
        Just form ->
            viewCreateSessionForm config language backend model form clinics sessions

        Nothing ->
            viewClinicList config language model clinics sessions


viewCreateSessionForm : Config.Model -> Language -> ModelBackend -> Model -> SessionForm -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewCreateSessionForm config language backend model form clinics sessions =
    let
        dates =
            scheduledDateState form

        errors =
            Form.getErrors form

        clinic =
            clinicIdState form

        clinicOption ( clinicId, clinic ) =
            ( toString (fromEntityId clinicId), clinic.name )

        clinicOptions =
            ( "", translate language Translate.SelectClinic )
                :: (clinics |> EveryDictList.toList |> List.sortBy (Tuple.second >> .name) |> List.map clinicOption)

        requestStatus =
            case backend.postSessionRequest of
                Success _ ->
                    div
                        [ class "ui success message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.Success ]
                        , div [] [ text <| translate language Translate.YourSessionHasBeenSaved ]
                        ]

                Failure err ->
                    div
                        [ class "ui warning message" ]
                        [ div [ class "header" ] [ text <| translate language Translate.BackendError ]
                        , viewError language err
                        ]

                Loading ->
                    emptyNode

                NotAsked ->
                    emptyNode
    in
        [ h2 [] [ text <| translate language Translate.CreateSession ]
        , div
            -- For the moment, we're using "error" for validation errors,
            -- "warning" for HTTP errors.
            [ classList
                [ ( "ui", True )
                , ( "form", True )
                , ( "error", Form.isSubmitted form && (not (List.isEmpty errors)) )
                , ( "success", RemoteData.isSuccess backend.postSessionRequest )
                , ( "warning", RemoteData.isFailure backend.postSessionRequest )
                ]
            ]
            [ div
                [ classList
                    [ ( "field", True )
                    , ( "error", isJust clinic.liveError )
                    ]
                ]
                [ selectInput clinicOptions clinic []
                    |> Html.map MsgCreateSession
                ]
            , div
                [ class "two fields" ]
                [ div
                    [ classList
                        [ ( "field", True )
                        , ( "error", isJust dates.start.liveError )
                        ]
                    ]
                    [ label [] [ text <| translate language Translate.StartDate ]
                    , textInput dates.start []
                        |> Html.map MsgCreateSession
                    ]
                , div
                    [ classList
                        [ ( "field", True )
                        , ( "error", isJust dates.end.liveError )
                        ]
                    ]
                    [ label [] [ text <| translate language Translate.EndDate ]
                    , textInput dates.end []
                        |> Html.map MsgCreateSession
                    ]
                ]
            , div
                [ class "field" ]
                [ div
                    [ class "ui checkbox admin" ]
                    [ checkboxInput (closedState form) [ id "session-closed" ]
                        |> Html.map MsgCreateSession
                    , label
                        [ for "session-closed" ]
                        [ text <| translate language Translate.Closed ]
                    ]
                ]
            , showIf config.sandbox <|
                div
                    [ class "field" ]
                    [ div
                        [ class "ui checkbox admin" ]
                        [ checkboxInput (trainingState form) [ id "session-sandbox" ]
                            |> Html.map MsgCreateSession
                        , label
                            [ for "session-sandbox" ]
                            [ text <| translate language Translate.Training ]
                        ]
                    ]
            , div []
                [ button
                    [ class "ui button"
                    , onClick <| ShowCreateSessionForm False
                    ]
                    [ text <| translate language Translate.Cancel ]
                , button
                    [ class "ui button primary"
                    , type_ "submit"
                    , onClick <| MsgCreateSession Form.Submit
                    ]
                    [ text <| translate language Translate.Save ]
                ]

            -- Note that these are hidden by deafult by semantic-ui ... the
            -- class of the "form" controls whether they are shown.
            , requestStatus
            , div
                [ class "ui error message" ]
                [ div [ class "header" ] [ text <| translate language Translate.ValidationErrors ]
                , dumpErrors form
                ]
            ]
        ]


viewClinicList : Config.Model -> Language -> Model -> EveryDictList ClinicId Clinic -> ( NominalDate, EveryDictList SessionId Session ) -> List (Html Msg)
viewClinicList config language model clinics ( _, futureSessions ) =
    let
        buttons =
            div []
                [ button
                    [ class "ui primary button small"
                    , onClick <| ShowCreateSessionForm True
                    ]
                    [ text <| translate language <| Translate.CreateSession ]
                , text " "
                , button
                    [ class "ui primary button small" ]
                    [ text <| translate language <| Translate.CreateTrainingSessions ]
                , text " "
                , button
                    [ class "ui primary button small" ]
                    [ text <| translate language <| Translate.DeleteTrainingSessions
                    ]
                ]

        futureSessionsByClinic =
            EveryDictList.groupBy (Tuple.second >> .clinicId) (EveryDictList.toList futureSessions)

        clinicList =
            clinics
                |> EveryDictList.sortBy .name
                |> EveryDictList.toList
                |> List.map (viewClinic language futureSessionsByClinic)
                |> div []
    in
        [ buttons
        , clinicList
        ]


viewClinic : Language -> EveryDictList ClinicId (List ( SessionId, Session )) -> ( ClinicId, Clinic ) -> Html Msg
viewClinic language futureSessionsByClinic ( clinicId, clinic ) =
    let
        futureSessions =
            futureSessionsByClinic
                |> EveryDictList.get clinicId
                |> Maybe.withDefault []
                |> List.sortBy (Tuple.second >> .scheduledDate >> .start >> Time.Date.toTuple)
    in
        div []
            [ h2 [] [ text clinic.name ]
            , viewFutureSessions language futureSessions
            ]


viewFutureSessions : Language -> List ( SessionId, Session ) -> Html Msg
viewFutureSessions language sessions =
    table
        [ class "ui striped table" ]
        [ thead [] []
        , tbody [] (List.map (viewFutureSession language) sessions)
        , tfoot [] []
        ]


viewFutureSession : Language -> ( SessionId, Session ) -> Html Msg
viewFutureSession language ( sessionId, session ) =
    tr []
        [ td []
            [ text <| formatYYYYMMDD session.scheduledDate.start
            , text " - "
            , text <| formatYYYYMMDD session.scheduledDate.end
            , text " "
            , showIf session.training <|
                span []
                    [ span
                        [ class "ui teal tag label training-label" ]
                        [ text <| translate language Translate.Training ]
                    , text " "
                    , button
                        [ class "ui icon negative small button" ]
                        [ i [ class "trash icon" ] []
                        ]
                    ]
            ]
        ]
