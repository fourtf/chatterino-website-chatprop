module Main exposing (main, update, view)

import Browser
import Common exposing (Message, Model, Msg(..), Split, defaultModel, listMaxLength, maxMessageCount, randomMessage, transformFirst)
import Html exposing (Html)
import Random exposing (generate)
import Task
import Time
import View exposing (viewWindow)


init : a -> ( Model, Cmd Msg )
init _ =
    -- unused argument is the Program arg
    ( defaultModel
    , Cmd.batch
        [ Task.perform
            InitTime
            (Task.map2 Tuple.pair
                Time.here
                Time.now
            )
        ]
    )


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


view : Model -> Html Msg
view =
    viewWindow


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 GenerateMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitTime ( zone, now ) ->
            ( { model | zone = zone }, generate InitMessages (Random.list maxMessageCount <| randomMessage now) )

        GenerateMessage t ->
            ( model, generate AddMessage <| randomMessage t )

        AddMessage message ->
            ( addMessage message model, Cmd.none )

        InitMessages messages ->
            ( initMessages messages model, Cmd.none )



-- ACTIONS


addMessage : Message -> Model -> Model
addMessage message =
    transformSplit (\split -> { split | messages = listMaxLength maxMessageCount <| split.messages ++ [ message ] })


initMessages : List Message -> Model -> Model
initMessages messages =
    transformSplit (\split -> { split | messages = listMaxLength maxMessageCount <| messages })


transformSplit : (Split -> Split) -> Model -> Model
transformSplit f model =
    { model | tabs = transformFirst (\tab -> { tab | splits = transformFirst f tab.splits }) model.tabs }
