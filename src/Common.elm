module Common exposing (..)

import Array
import Random exposing (Generator)
import Time exposing (Posix, utc)


type Msg
    = InitTime ( Time.Zone, Posix ) -- here, now
    | InitMessages (List Message)
    | GenerateMessage Posix
    | AddMessage Message


maxMessageCount : number
maxMessageCount =
    15


emotes : Array.Array String
emotes =
    Array.fromList [ "LULW", "KEKW" ]


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
    { time : Posix
    , user : String
    , text : String
    , emote : String
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
    Random.map
        (\emoteIndex -> Message time "asd" "asd" <| Maybe.withDefault "" <| Array.get emoteIndex emotes)
    <|
        Random.int 0 (Array.length emotes - 1)


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
