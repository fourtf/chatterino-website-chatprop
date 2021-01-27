module View exposing (..)

import Common exposing (Message, Model, Msg, Split, Tab, TabStatus(..))
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (class, src)
import Time


tabStatusClass : TabStatus -> String
tabStatusClass status =
    case status of
        TabActive ->
            "active"

        TabSeen ->
            "seen"

        TabNewMessages ->
            "new-messages"

        TabHighlighted ->
            "highlighted"


tabTitle : Tab -> String
tabTitle tab =
    String.join ", " <| List.map .name tab.splits


viewWindow : Model -> Html Msg
viewWindow model =
    div [ class "c-window" ]
        [ div [ class "c-window-titlebar" ] [ text "Chatterino" ]
        , div [] (List.map viewTab model.tabs)
        , div
            [ class "c-split-container" ]
            -- yuck
            (Maybe.withDefault [ text "" ] <|
                Maybe.map (List.map viewSplit) <|
                    Maybe.map .splits <|
                        List.head model.tabs
            )
        ]


viewTab : Tab -> Html Msg
viewTab tab =
    div [ class "c-tab", class <| tabStatusClass tab.status ]
        [ text <|
            tabTitle tab
        ]


viewSplit : Split -> Html Msg
viewSplit split =
    div [ class "c-split" ]
        [ viewSplitHeader split, viewSplitContent split, viewSplitInput split ]


viewSplitHeader : Split -> Html Msg
viewSplitHeader split =
    div [ class "c-split-header" ]
        [ div [] [ text split.name ]
        ]


viewSplitContent : Split -> Html Msg
viewSplitContent split =
    div [ class "c-split-content" ] (List.map viewMessage split.messages)


viewMessage : Message -> Html msg
viewMessage message =
    div [ class "c-message" ]
        [ span [ class "c-timestamp" ] [ text <| formatTime message.time ]
        , span [ class "c-username" ] [ text <| message.user ++ ":" ]
        , span [] [ text message.text ]
        , img [ src <| "emotes/" ++ message.emote ++ ".png", class "c-emote-2x" ] []
        ]


formatTime : Time.Posix -> String
formatTime t =
    let
        h =
            String.fromInt <| Time.toHour Time.utc t

        m =
            String.padLeft 2 '0' <| String.fromInt <| Time.toMinute Time.utc t
    in
    h ++ ":" ++ m


viewSplitInput : Split -> Html Msg
viewSplitInput _ =
    div [ class "c-split-input" ]
        [ text "" ]
