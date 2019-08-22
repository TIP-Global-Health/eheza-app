module Pages.PrenatalParticipant.View exposing (view)

import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import EveryDict
import EveryDictList exposing (EveryDictList)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatYYYYMMDD)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalParticipant.Model exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityId, fromEntityUuid, toEntityId)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Html App.Model.Msg
view language currentDate id db =
    let
        person =
            EveryDict.get id db.people
                |> Maybe.withDefault NotAsked

        headerName =
            person
                |> RemoteData.map .name
                |> RemoteData.withDefault (translate language Translate.PrenatalParticipant ++ " " ++ fromEntityUuid id)

        prenatalSessions =
            EveryDict.get id db.prenatalParticipantsByPerson
                |> Maybe.withDefault NotAsked
    in
    div
        [ class "page-prenatal-participant" ]
        [ viewHeader language headerName
        , div
            [ class "ui full segment blue" ]
            [ viewWebData language (viewPrenatalSessions language id db) identity prenatalSessions
            ]
        ]


viewHeader : Language -> String -> Html App.Model.Msg
viewHeader language name =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text name ]
        , a
            [ class "link-back"
            , onClick <| App.Model.SetActivePage <| UserPage PrenatalParticipantsPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewPrenatalSessions : Language -> PersonId -> ModelIndexedDb -> EveryDictList PrenatalParticipantId PrenatalParticipant -> Html App.Model.Msg
viewPrenatalSessions language id db prenatalSessions =
    div [ class "ui table session-list" ]
        [ h1 [] [ text <| translate language Translate.RecentAndUpcomingGroupEncounters ]
        , table
            [ class "ui table session-list" ]
            [ thead []
                [ tr []
                    [ th [] [ text <| translate language Translate.StartDate ]
                    , th [] [ text <| translate language Translate.EndDate ]
                    ]
                ]
            , prenatalSessions
                |> EveryDictList.map (viewPrenatalSession language db)
                |> EveryDictList.values
                |> tbody []
            ]
        , createSessionButton language id
        ]


viewPrenatalSession : Language -> ModelIndexedDb -> PrenatalParticipantId -> PrenatalParticipant -> Html App.Model.Msg
viewPrenatalSession language db sessionId session =
    let
        enableLink =
            True

        action =
            EveryDict.get sessionId db.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (EveryDictList.toList
                        -- Commenting out temporarary
                        -- >> List.filter (\( _, encounter ) -> isNothing encounter.endDate)
                        >> List.head
                        >> Maybe.map
                            (\( encounterId, _ ) ->
                                [ onClick <| App.Model.SetActivePage <| UserPage <| Pages.Page.PrenatalEncounterPage encounterId ]
                            )
                        >> Maybe.withDefault []
                    )
                |> RemoteData.withDefault []

        link =
            button
                (classList
                    [ ( "ui button", True )
                    , ( "disabled", not enableLink )
                    , ( "active", enableLink )
                    ]
                    :: action
                )
                [ text <| translate language Translate.Attendance ]
    in
    tr []
        [ td [] [ text <| formatYYYYMMDD session.startDate ]
        , td [] [ text <| Maybe.withDefault "" <| Maybe.map formatYYYYMMDD session.endDate ]
        , td [] [ link ]
        ]


createSessionButton : Language -> PersonId -> Html App.Model.Msg
createSessionButton language id =
    button
        [ classList
            [ ( "ui button active", True )

            --  , ( "loading", isLoading postSession )
            ]

        -- , defaultSession
        --     |> PostSession
        --     |> App.Model.MsgIndexedDb
        --     |> onClick
        ]
        [ text <| translate language Translate.CreateGroupEncounter ]
