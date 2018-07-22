module Utils.Confirmation exposing (..)

{-| A generic way to require a confirmation dialog before
sending a message.

To integrate this into `Foo.Model`, you'd want something like:

    module Foo.Model exposing (..)

    import Utils.Confirmation as Confirmation

    ...

    type alias Model =
        { ...
        , confirmation : Confirmation.Model Msg
        ...
        }

    type Msg
        = ...
        | MsgConfirmation (Confirmation.Msg Msg)
        | ...

    emptyModel : Model
    emptyModel =
        { ...
        , confirmation : Confirmation.emptyModel
        }

-}

import Gizra.Html exposing (emptyNode)
import Html exposing (Html, a, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Model msg
    = Closed
    | InProgress (ConfirmationDialog msg)


emptyModel : Model any
emptyModel =
    Closed


type alias ConfirmationDialog msg =
    { title : String
    , body : String
    , okButton : String
    , confirmMsg : msg
    , cancelMsg : Maybe msg
    }


type Msg msg
    = Open (ConfirmationDialog msg)
    | Cancel (Maybe msg)
    | Confirm msg


{-| To integrate this into `Foo.Update`, you'd want something roughly like this.

    import Utils.Confirmation as Confirmation
    import Update.Extra exposing (sequence)

    ...

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ...

            MsgConfirmation subMsg ->
                let
                    ( subModel, subCmd, msgs ) =
                        Confirmation.update subMsg model.confirmation
                in
                    sequence update
                        msgs
                        ( { model | confirmation = subModel }
                        , Cmd.map MsgConfirmation subCmd
                        )

            ...

-}
update : Msg msg -> Model msg -> ( Model msg, Cmd (Msg msg), List msg )
update message model =
    case message of
        Open dialog ->
            ( InProgress dialog
            , Cmd.none
            , []
            )

        Cancel maybeMsg ->
            ( Closed
            , Cmd.none
            , maybeMsg |> Maybe.map (\msg -> [ msg ]) |> Maybe.withDefault []
            )

        Confirm msg ->
            ( Closed
            , Cmd.none
            , [ msg ]
            )


{-| To integrate this into `Foo.View`, you'd want to do something
roughly like this:

    module Foo.View exposing (view)

    import Foo.Model exposing (..)
    import Utils.Confirmation as Confirmation
    import Html
    ...

    view : Model -> Html Msg
    view model =
        div [ ... ]
            [ ...
            , Html.map MsgConfirmation <|
                Confirmation.view model.confirmation
            ]

Note that you can call `view` here unconditionally, as it will emit
an empty node if we're in the `Closed` state.

Then, at some point in your `view` function, you're going to want to
have an event that opens the confirmation dialog. To do that, you'd
do something roughly like this:

    a
        [ class "item-change"
        , onClick <|
            MsgConfirmation <|
                Confirmation.Open
                    { title = "Are you sure you want to delete this subject?"
                    , body = "This action can not be undone"
                    , msg = DeleteSubject subject.id
                    }
        ]
        [ i
            [ class "unioicon-trash" ]
            []
        , text "Delete"
        ]

... where the `msg = ...` part would be the message you want processed if confirmed.
(Of course, it's the `onClick` handler that's significant in the example ... the
rest is just context).

-}
view : Model msg -> Html (Msg msg)
view model =
    case model of
        Closed ->
            emptyNode

        InProgress dialog ->
            div [ class "ui dimmer modals page transition visible active" ]
                [ div
                    [ class "ui modal center transition visible active" ]
                    [ div
                        [ class "header" ]
                        [ h2 [] [ text dialog.title ] ]
                    , div
                        [ class "clearfix content" ]
                        [ text dialog.body ]
                    , div
                        [ class "actions" ]
                        [ a
                            [ onClick <| Cancel dialog.cancelMsg
                            , class "ui button"
                            ]
                            [ text "Cancel" ]
                        , a
                            [ onClick <| Confirm dialog.confirmMsg
                            , class "ui button primary"
                            ]
                            [ text dialog.okButton ]
                        ]
                    ]
                ]
