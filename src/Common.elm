module Common exposing (..)

import Array
import Random exposing (Generator)
import Time exposing (Posix, utc)


type Msg
    = InitTime ( Time.Zone, Posix ) -- here, now
    | InitMessages (List Message)
    | MaybeGenerateMessage Posix
    | GenerateMessage Posix Bool
    | AddMessage Message


maxMessageCount : number
maxMessageCount =
    15


w1 : Array.Array String
w1 =
    Array.fromList [ "wow", "WOW", "omg", "oh no" ]


w2 : Array.Array String
w2 =
    Array.fromList [ "this guy", "seriously", "so good", "my streamer", "pogger", "let's throw" ]


emotes : Array.Array String
emotes =
    Array.fromList [ "LUL", "LULW", "KEKW", "Sadge", "Kappa", "YEP", "4Weird" ]


colors : Array.Array String
colors =
    Array.fromList
        [ "#fff"
        , "#ff0000"
        , "#c6c6ff"
        , "#00ff00"
        , "#e71818"
        , "#ff7f50"
        , "#9acd32"
        , "#ff4500"
        , "#2ad575"
        , "#edae12"
        , "#ed6d12"
        , "#5f9ea0"
        , "#6fb4f6"
        , "#ff69b4"
        , "#c897f5"
        , "#00ff7f"
        ]


chatters : Array.Array String
chatters =
    Array.fromList
        [ "Kappa123"
        , "grey_face"
        , "shortwig"
        , "justinfan123"
        , "championsen"
        , "mxr"
        , "Ke2"
        , "PO_Box"
        , "ChickenDinner"
        , "BillDipperly"
        , "check_unban_forms"
        , "WgXcQ"
        ]


type TabStatus
    = TabActive
    | TabSeen
    | TabNewMessages
    | TabHighlighted


type alias Tab =
    { splits : List Split
    , status : TabStatus
    }


fakeTab : String -> Tab
fakeTab title =
    Tab [ Split title [] ] TabSeen


type alias Split =
    { name : String
    , messages : List Message
    }


type alias Message =
    { key : String
    , time : Posix
    , user : String
    , userColor : String
    , text : String
    , emote : Maybe String
    }


type alias Model =
    { tabs : List Tab, zone : Time.Zone }


defaultModel : Model
defaultModel =
    Model defaultTabs utc


defaultTabs : List Tab
defaultTabs =
    [ Tab [ Split "Streamer" [], Split "/mentions" [] ] TabActive
    , fakeTab "a"
    , fakeTab "b"
    , fakeTab "c"
    , fakeTab "d"
    , fakeTab "e"
    ]


randomMessage : Posix -> Generator Message
randomMessage time =
    Random.map5
        (\emoteIndex i1 i2 chatterIndex key ->
            let
                userColor =
                    Maybe.withDefault "#fff" <| Array.get chatterIndex colors

                msg =
                    String.trim <|
                        (Maybe.withDefault "" <| Array.get i1 w1)
                            ++ " "
                            ++ (Maybe.withDefault "" <| Array.get i2 w2)
                            ++ " "

                emote =
                    if String.isEmpty msg then
                        Just <| Maybe.withDefault "LULW" <| Array.get emoteIndex emotes

                    else
                        Array.get emoteIndex emotes

                chatter =
                    Maybe.withDefault "fourtf" <| Array.get chatterIndex chatters
            in
            Message (String.fromInt key) time chatter userColor msg emote
        )
        (Random.int 0 (Array.length emotes - 1 + 2))
        (Random.int 0 (Array.length w1 - 1 + 10))
        (Random.int 0 (Array.length w2 - 1 + 10))
        -- (Random.int 0 (Array.length colors - 1))
        (Random.int 0 (Array.length chatters - 1))
        (Random.int Random.minInt Random.maxInt)


listMaxLength : Int -> List a -> List a
listMaxLength n l =
    if List.length l > n then
        List.drop (List.length l - n) l

    else
        l


transformFirst : (a -> a) -> List a -> List a
transformFirst f l =
    case l of
        x :: xs ->
            f x :: xs

        _ ->
            []
