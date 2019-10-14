module View exposing (..)

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

                GamePlaying _ ->
                    playingGameView model

                GameWon ->
                    playingGameView model

                GameLost ->
                    playingGameView model
    in
        div
            [ style "width" "50%"
            , style "margin" "0 auto"
            , style "background" "#f1eed3"
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
        [ displayGameStateTitle model
        , roundsView model
        , h2 [] [ text "Solution" ]
        , solutionView model
        ]


pegView : String -> String -> List (Html.Attribute Msg) -> Html Msg
pegView borderStyle backgroundColor additionalAttrs =
    div
        ([ style "border" "2px solid #333"
         , style "border-radius" "50%"
         , style "border-style" borderStyle
         , style "background" backgroundColor
         , style "display" "inline-block"
         , style "width" "25px"
         , style "height" "25px"
         , style "overflow" "hidden"
         , style "margin" "5px 10px"
         ]
            ++ additionalAttrs
        )
        []


slotView : RoundNumber -> Model -> ( SlotNumber, Slot ) -> Html Msg
slotView roundNumber model ( slotNumber, ( peg, pip ) ) =
    let
        borderStyle =
            pegBorderStyle peg

        backgroundColor =
            pegColor peg

        currentRoundSlotAttrs =
            [ style "cursor" "pointer"
            , onClick (SelectSlot roundNumber slotNumber)
            ]

        isCurrentRound =
            model.currentRound == roundNumber
    in
        if isCurrentRound then
            pegView borderStyle backgroundColor currentRoundSlotAttrs
        else
            pegView borderStyle backgroundColor []


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
                 , slotsView roundNumber slots model
                 , roundInfoView roundNumber model
                 ]
                )
    in
        accumulator ++ [ roundDiv ]


slotsView : RoundNumber -> Slots -> Model -> Html Msg
slotsView roundNumber slots model =
    div
        [ style "width" "60%"
        , style "position" "relative"
        ]
        (slots
            |> Dict.toList
            |> List.map (slotView roundNumber model)
            |> List.append [ pegPickerView roundNumber model ]
        )


pegPickerView : RoundNumber -> Model -> Html Msg
pegPickerView roundNumber model =
    case model.gameState of
        GamePlaying (PegPickerOpen selectedRoundNumber selectedSlotNumber) ->
            if model.currentRound == roundNumber then
                div
                    [ style "background" "#aaa"
                    , style "position" "absolute"
                    , style "top" "50px"
                    , style "left" "125px"
                    , style "width" "150px"
                    , style "z-index" "1"
                    ]
                    ([ span [] [ text "Pick Color" ] ] ++ (pegPickerSlotsView selectedRoundNumber selectedSlotNumber))
            else
                span [] []

        _ ->
            span [] []


pegPickerSlotsView : RoundNumber -> SlotNumber -> List (Html Msg)
pegPickerSlotsView roundNumber slotNumber =
    let
        pegSlotView peg =
            pegView (pegBorderStyle peg) (pegColor peg) [ onClick <| SetPeg roundNumber slotNumber peg ]

        slots =
            allPegs
                |> List.map pegSlotView
    in
        [ div [] slots ]


roundInfoView : RoundNumber -> Model -> Html Msg
roundInfoView roundNumber model =
    let
        buttonChild =
            case model.gameState of
                GamePlaying _ ->
                    button
                        [ onClick CommitRound
                        , disabled <| (areRoundSlotsFull roundNumber model |> not)
                        ]
                        [ text "Check My Guess" ]

                _ ->
                    span [] []

        children =
            if roundNumber == model.currentRound then
                [ buttonChild
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
        [ div [ style "width" "48px" ] pips ]


pipView : Pip -> Html Msg
pipView pip =
    let
        backgroundColor =
            case pip of
                PipNoMatch ->
                    "#666"

                PipColorMatch ->
                    "#fff"

                PipColorSlotMatch ->
                    "#f00"
    in
        span
            [ style "border" "1px solid #333"
            , style "border-radius" "50%"
            , style "border-style" "solid"
            , style "background" backgroundColor
            , style "display" "inline-block"
            , style "width" "10px"
            , style "height" "10px"
            , style "overflow" "hidden"
            , style "margin" "3px"
            ]
            []


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
        (roundView model 0 model.solution [])


roundsView : Model -> Html Msg
roundsView model =
    div
        [ style "width" "100%"
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


displayGameStateTitle : Model -> Html Msg
displayGameStateTitle model =
    case model.gameState of
        GameNew ->
            h2 [] [ text "Game hasn't started yet" ]

        GamePlaying _ ->
            h2 [] [ text <| guessesCountDescription model ]

        GameWon ->
            h2 []
                [ text "Yahoo! You won the game!"
                , button [ onClick ResetGame ] [ text "New Game" ]
                ]

        GameLost ->
            h2 []
                [ text "Sorry, you lost the game!"
                , button [ onClick ResetGame ] [ text "New Game" ]
                ]


guessesCountDescription : Model -> String
guessesCountDescription model =
    let
        currentRound =
            String.fromInt model.currentRound

        totalRounds =
            model.rounds |> Dict.size |> String.fromInt
    in
        "Round " ++ currentRound ++ " of " ++ totalRounds


pegBorderStyle : Peg -> String
pegBorderStyle peg =
    case peg of
        PegNone ->
            "dotted"

        _ ->
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
