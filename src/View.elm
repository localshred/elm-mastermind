module View exposing (..)

import Dict exposing (toList)
import Html exposing (Html, button, div, h2, h5, input, label, text, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (classList, disabled, id, type_, value)
import Types exposing (..)


view : Model -> Html Msg
view model =
    let
        innerGameView =
            case model.gameState of
                GameNew ->
                    viewGameNew model

                _ ->
                    viewGamePlaying model
    in
        div [ id "main" ] <|
            [ h2 [ id "game-title" ] [ text "Mastermind" ] ]
                ++ [ innerGameView ]


textInput : String -> String -> (String -> Msg) -> Html Msg
textInput labelText inputValue inputHandler =
    inputFieldRow
        [ label [] [ text labelText ]
        , input
            [ type_ "number"
            , value inputValue
            , onInput inputHandler
            ]
            []
        ]


inputFieldRow : List (Html Msg) -> Html Msg
inputFieldRow children =
    div
        [ classList [ ( "input-field-row", True ) ] ]
        children


viewGameNew : Model -> Html Msg
viewGameNew model =
    let
        isStartGameEnabled =
            not <| model.totalRounds > 0 && model.totalSlots > 0
    in
        div
            [ classList [ ( "new-game", True ) ] ]
            [ inputFieldRow <| [ textInput "# Rounds" (String.fromInt model.totalRounds) ConfigSetTotalRounds ]
            , inputFieldRow <| [ textInput "# Pegs" (String.fromInt model.totalSlots) ConfigSetTotalSlots ]
            , inputFieldRow <|
                [ btn "Crack the Code!" "grey" StartGame [ disabled isStartGameEnabled ] ]
            ]


maybeToListItem : Maybe a -> List a
maybeToListItem maybe =
    maybe |> Maybe.map List.singleton |> Maybe.withDefault []


viewGamePlaying : Model -> Html Msg
viewGamePlaying model =
    let
        solution =
            model |> solutionView |> maybeToListItem
    in
        div [] <|
            [ displayGameStateTitle model ]
                ++ solution
                ++ [ roundsView model ]


pegView : String -> List (Html.Attribute Msg) -> Html Msg
pegView pegClassName additionalAttrs =
    div
        ([ classList
            [ ( "peg", True )
            , ( pegClassName, True )
            , ( "pegged", pegClassName /= "clear" )
            ]
         ]
            ++ additionalAttrs
        )
        []


slotView : RoundNumber -> Model -> ( SlotNumber, Slot ) -> Html Msg
slotView roundNumber model ( slotNumber, ( peg, pip ) ) =
    let
        pegClassName =
            pegClassColor peg

        currentRoundSlotAttrs =
            [ classList [ ( "current-round", True ) ]
            , onClick (SelectSlot roundNumber slotNumber)
            ]

        isCurrentRound =
            model.currentRound == roundNumber
    in
        if isCurrentRound then
            pegView pegClassName currentRoundSlotAttrs
        else
            pegView pegClassName []


roundView : Model -> ( RoundNumber, Slots ) -> Html Msg
roundView model ( roundNumber, slots ) =
    div
        [ classList [ ( "round", True ) ] ]
        [ roundNameView roundNumber
        , slotsView roundNumber slots model
        , roundInfoView roundNumber model
        ]


isPickerOpen : Model -> Bool
isPickerOpen model =
    case model.gameState of
        GamePlaying (PegPickerOpen _ _) ->
            True

        _ ->
            False


slotsView : RoundNumber -> Slots -> Model -> Html Msg
slotsView roundNumber slots model =
    div
        [ classList
            [ ( "slots", True )
            , ( "picker-open", isPickerOpen model )
            ]
        ]
        (slots
            |> Dict.toList
            |> List.map (slotView roundNumber model)
            |> List.append
                (model
                    |> pegPickerView roundNumber
                    |> maybeToListItem
                )
        )


pegPickerView : RoundNumber -> Model -> Maybe (Html Msg)
pegPickerView roundNumber model =
    case model.gameState of
        GamePlaying (PegPickerOpen selectedRoundNumber selectedSlotNumber) ->
            if model.currentRound == roundNumber then
                Just <| pegPickerSlotsView selectedRoundNumber selectedSlotNumber
            else
                Nothing

        _ ->
            Nothing


pegPickerSlotsView : RoundNumber -> SlotNumber -> Html Msg
pegPickerSlotsView roundNumber slotNumber =
    let
        pegSlotView peg =
            pegView (pegClassColor peg) [ onClick <| SetPeg roundNumber slotNumber peg ]
    in
        div
            [ classList [ ( "peg-picker", True ) ] ]
            (allPegs |> List.map pegSlotView)


btn : String -> String -> Msg -> List (Html.Attribute Msg) -> Html Msg
btn txt color msg attrs =
    button
        ([ classList [ ( "btn-" ++ color, True ) ]
         , onClick msg
         ]
            ++ attrs
        )
        [ text txt ]


roundInfoView : RoundNumber -> Model -> Html Msg
roundInfoView roundNumber model =
    let
        buttonChild : Maybe (Html Msg)
        buttonChild =
            case model.gameState of
                GamePlaying _ ->
                    if areRoundSlotsFull roundNumber model then
                        Just <|
                            btn "COMPUTE" "grey" CommitRound []
                    else
                        Nothing

                _ ->
                    Nothing

        children : List (Html Msg)
        children =
            if roundNumber == model.currentRound then
                buttonChild |> maybeToListItem
            else if roundNumber < model.currentRound then
                model |> pipsForRoundView roundNumber
            else
                []
    in
        div
            [ classList [ ( "round-info", True ) ] ]
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
        [ div [ classList [ ( "pips", True ) ] ] pips ]


pipView : Pip -> Html Msg
pipView pip =
    span
        [ classList
            [ ( "pip", True )
            , ( "no-match", pip == PipNoMatch )
            , ( "color-match", pip == PipColorMatch )
            , ( "color-slot-match", pip == PipColorSlotMatch )
            ]
        ]
        []


roundNameView : RoundNumber -> Html Msg
roundNameView roundNumber =
    let
        roundName =
            case roundNumber of
                0 ->
                    "Solution"

                _ ->
                    String.fromInt roundNumber
    in
        div
            [ classList [ ( "round-name", True ) ] ]
            [ text roundName ]


solutionView : Model -> Maybe (Html Msg)
solutionView model =
    case model.gameState of
        GamePlaying _ ->
            Nothing

        GameNew ->
            Nothing

        _ ->
            Just <|
                div [ classList [ ( "solution", True ) ] ]
                    [ roundView model ( 0, model.solution ) ]


roundsView : Model -> Html Msg
roundsView model =
    div [ classList [ ( "rounds", True ) ] ]
        (model.rounds
            |> Dict.toList
            |> List.take model.currentRound
            |> List.map (roundView model)
        )


gameEnd : String -> String -> Html Msg
gameEnd class txt =
    div [ classList [ ( "game-state-title", True ), ( "game-state-end", True ), ( class, True ) ] ]
        [ h5 [] [ text txt ]
        , btn "New Game" "grey" ResetGame []
        ]


displayGameStateTitle : Model -> Html Msg
displayGameStateTitle model =
    case model.gameState of
        GameNew ->
            h5 [ classList [ ( "game-state-title", True ) ] ] [ text "Game hasn't started yet" ]

        GamePlaying _ ->
            h5 [ classList [ ( "game-state-title", True ) ] ] [ text <| guessesCountDescription model ]

        GameWon ->
            gameEnd "game-state-won" <| "You won the game in " ++ String.fromInt model.currentRound ++ " Rounds!"

        GameLost ->
            gameEnd "game-state-lost" "Sorry, you lost the game!"


guessesCountDescription : Model -> String
guessesCountDescription model =
    let
        currentRound =
            String.fromInt model.currentRound

        totalRounds =
            model.rounds |> Dict.size |> String.fromInt
    in
        "Round " ++ currentRound ++ " of " ++ totalRounds


pegClassColor : Peg -> String
pegClassColor peg =
    case peg of
        PegGreen ->
            "green"

        PegYellow ->
            "yellow"

        PegBlack ->
            "black"

        PegWhite ->
            "white"

        PegBlue ->
            "blue"

        PegRed ->
            "red"

        PegNone ->
            "clear"
