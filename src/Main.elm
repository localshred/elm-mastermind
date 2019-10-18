module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Types exposing (..)
import Update exposing (update)
import View exposing (view)


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown keyDownDecoder


keyDownDecoder : Decode.Decoder Msg
keyDownDecoder =
    Decode.map KeyPress (Decode.field "key" Decode.string)
