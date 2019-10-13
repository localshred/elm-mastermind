module Main exposing (..)

import Browser
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
    Sub.none
