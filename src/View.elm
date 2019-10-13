module View exposing (..)

import Debug
import Dict exposing (toList)
import Html exposing (Html, button, div, h2, input, label, text, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (classList, disabled, style, type_, value)
import Types exposing (..)


view : Model -> Html Msg
view model =
    let
        innerGameView =
            case model.gameState of
                GameNew ->
                    newGameView model

                GamePlaying ->
                    playingGameView model

                GameWon ->
                    gameWonView model

                GameLost ->
                    gameLostView model
    in
        div
            [ style "width" "50%"
            , style "margin" "0 auto"
            ]
            ([ h2 [] [ text "Mastermind" ] ]
                ++ [ innerGameView ]
            )


gameWonView : Model -> Html Msg
gameWonView model =
    div [] [ text "you won the game" ]


gameLostView : Model -> Html Msg
gameLostView model =
    div [] [ text "you lost the game" ]


textInput : String -> String -> (String -> Msg) -> Html Msg
textInput labelText inputValue inputHandler =
    div
        []
        [ label [] [ text labelText ]
        , input
            [ type_ "text"
            , value inputValue
            , onInput inputHandler
            ]
            []
        ]


newGameView : Model -> Html Msg
newGameView model =
    let
        isStartGameEnabled =
            not (model.totalRounds > 0 && model.totalSlots > 0)
    in
        div
            []
            [ div [] [ textInput "total rounds" (String.fromInt model.totalRounds) ConfigSetTotalRounds ]
            , div [] [ textInput "total slots" (String.fromInt model.totalSlots) ConfigSetTotalSlots ]
            , div []
                [ button
                    [ onClick StartGame
                    , disabled isStartGameEnabled
                    ]
                    [ text "new game" ]
                ]
            ]


playingGameView : Model -> Html Msg
playingGameView model =
    div
        []
        [ h2 [] [ text <| guessesCountDescription model ]
        , roundsView model
        , h2 [] [ text "Solution" ]
        , solutionView model
        ]


slotView : RoundNumber -> ( SlotNumber, Slot ) -> Html Msg
slotView roundNumber ( slotNumber, ( peg, pip ) ) =
    div
        [ style "border" "2px solid #333"
        , style "border-style" <| pegBorderStyle peg
        , style "border-radius" "50%"
        , style "background" <| pegColor peg
        , style "display" "inline-block"
        , style "width" "25px"
        , style "height" "25px"
        , style "overflow" "hidden"
        , style "margin" "5px 10px"
        , style "cursor" "pointer"
        , onClick (SetPeg roundNumber slotNumber PegGreen)
        ]
        []


roundView : Model -> RoundNumber -> Slots -> List (Html Msg) -> List (Html Msg)
roundView model roundNumber slots accumulator =
    let
        roundDiv =
            div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "space-between"
                , style "align-items" "center"
                , style "width" "100%"
                ]
                ([ roundNameView roundNumber
                 , slotsView roundNumber slots
                 , roundInfoView roundNumber model
                 ]
                )
    in
        accumulator ++ [ roundDiv ]


slotsView : RoundNumber -> Slots -> Html Msg
slotsView roundNumber slots =
    div
        [ style "width" "60%"
        ]
        (slots
            |> Dict.toList
            |> List.map (slotView roundNumber)
        )


roundInfoView : RoundNumber -> Model -> Html Msg
roundInfoView roundNumber model =
    let
        children =
            if roundNumber == model.currentRound then
                [ button
                    [ onClick CommitRound
                    , disabled <| (areRoundSlotsFull roundNumber model |> not)
                    ]
                    [ text "Check My Guess" ]
                ]
            else if roundNumber < model.currentRound then
                pipsForRoundView roundNumber model
            else
                []
    in
        div
            [ style "margin-top" "8px"
            , style "width" "20%"
            ]
            children


pipsForRoundView : RoundNumber -> Model -> List (Html Msg)
pipsForRoundView roundNumber model =
    let
        slots =
            model.rounds
                |> Dict.get roundNumber
                |> Maybe.withDefault Dict.empty

        pips =
            slots
                |> Dict.map (\k slot -> Tuple.second slot)
                |> Dict.values
                |> List.map pipView
    in
        [ div [] pips ]


pipView : Pip -> Html Msg
pipView pip =
    case pip of
        PipNoMatch ->
            div [] [ text "0" ]

        PipColorMatch ->
            div [] [ text "1" ]

        PipColorSlotMatch ->
            div [] [ text "2" ]


roundNameView : RoundNumber -> Html Msg
roundNameView roundNumber =
    let
        roundName =
            if roundNumber == 0 then
                ""
            else
                "Round " ++ String.fromInt roundNumber
    in
        div
            [ style "padding-top" "8px"
            , style "width" "20%"
            ]
            [ text roundName ]


solutionView : Model -> Html Msg
solutionView model =
    div
        [ style "padding" "5px"
        , style "margin" "5px"
        , style "text-align" "center"
        ]
        (roundView model -1 (Debug.log "solution" model.solution) [])


roundsView : Model -> Html Msg
roundsView model =
    div
        [ style "background-color" "#eee"
        , style "width" "100%"
        , style "padding" "5px"
        , style "margin" "5px"
        , style "text-align" "center"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-between"
        , style "align-items" "center"
        ]
        (Dict.foldl
            (roundView model)
            []
            model.rounds
        )


guessesCountDescription : Model -> String
guessesCountDescription model =
    let
        currentRound =
            String.fromInt model.currentRound

        totalRounds =
            model.rounds |> Dict.size |> String.fromInt
    in
        "Guesses (" ++ currentRound ++ " of " ++ totalRounds ++ ")"


pegBorderStyle : Peg -> String
pegBorderStyle peg =
    if peg == PegNone then
        "dotted"
    else
        "solid"


pegColor : Peg -> String
pegColor peg =
    case peg of
        PegGreen ->
            "#0f0"

        PegYellow ->
            "yellow"

        PegBlack ->
            "#000"

        PegWhite ->
            "#fff"

        PegBlue ->
            "#00f"

        PegRed ->
            "#f00"

        PegNone ->
            "clear"
