module View exposing (..)

import Dict
import Html exposing (Html, button, div, h2, h5, input, label, text, span)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (classList, disabled, id, type_, value)
import Types exposing (..)


classes : List String -> Html.Attribute Msg
classes enabledClasses =
    classList
        (enabledClasses |> List.map (\class -> ( class, True )))


view : Model -> Html Msg
view model =
    let
        innerGameView =
            case model.gameState of
                GameNew ->
                    viewGameNew model

                _ ->
                    viewGamePlaying model

        gameStateClasses =
            case model.gameState of
                GameNew ->
                    [ "game-state-new" ]

                GamePlaying _ ->
                    [ "game-state-playing" ]

                GameWon ->
                    [ "game-state-end", "game-state-won" ]

                GameLost ->
                    [ "game-state-end", "game-state-lost" ]
    in
        div
            [ id "main"
            , classes gameStateClasses
            ]
        <|
            [ h2 [ id "game-title" ] [ text "MASTERMIND" ] ]
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
            [ classList [ ( "game-board", True ) ] ]
            [ inputFieldRow <|
                [ textInput
                    "# Rounds"
                    (String.fromInt model.totalRounds)
                    (ConfigSet ConfigKeyTotalRounds)
                ]
            , inputFieldRow <|
                [ textInput
                    "# Pegs"
                    (String.fromInt model.totalSlots)
                    (ConfigSet ConfigKeyTotalSlots)
                ]
            , inputFieldRow <|
                [ btn
                    "Crack the Code!"
                    "filled"
                    StartGame
                    [ disabled isStartGameEnabled ]
                ]
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
        div [ classList [ ( "game-board", True ) ] ] <|
            [ displayGameStateTitle model
            , roundsView model
            ]
                ++ solution


pegView : Bool -> String -> List (Html.Attribute Msg) -> Html Msg
pegView isSelected className additionalAttrs =
    div
        ([ classList
            [ ( "peg", True )
            , ( "pegged", className /= "clear" )
            , ( "selected", isSelected )
            , ( className, True )
            ]
         ]
            ++ additionalAttrs
        )
        []


slotView : RoundNumber -> Model -> ( SlotNumber, Slot ) -> Html Msg
slotView roundNumber model ( slotNumber, ( peg, pip ) ) =
    let
        className =
            pegClassColor peg

        isSelected =
            case model.gameState of
                GamePlaying (PegPickerOpen selectedRoundNumber selectedSlotNumber) ->
                    selectedRoundNumber == roundNumber && selectedSlotNumber == slotNumber

                _ ->
                    False

        additionalAttrs =
            if model.currentRound == roundNumber then
                [ onClick (SelectSlot roundNumber slotNumber) ]
            else
                []
    in
        pegView isSelected className additionalAttrs


roundView : Model -> ( RoundNumber, Slots ) -> Html Msg
roundView model ( roundNumber, slots ) =
    div
        [ classList [ ( "round", True ), ( "current-round", model.currentRound == roundNumber ) ] ]
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
            [ ( "round-slots", True )
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
            pegView
                False
                (pegClassColor peg)
                [ onClick <| SetPeg roundNumber slotNumber peg ]
    in
        div
            [ classList [ ( "peg-picker", True ) ] ]
            (allPegs |> List.map pegSlotView)


btn : String -> String -> Msg -> List (Html.Attribute Msg) -> Html Msg
btn txt color msg attrs =
    button
        ([ classList [ ( "btn", True ), ( "btn-" ++ color, True ) ]
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
                            btn "CHECK" "filled" CommitRound []
                    else
                        Nothing

                _ ->
                    Nothing

        infoCell : List (Html Msg)
        infoCell =
            if roundNumber == model.currentRound then
                buttonChild |> maybeToListItem
            else if roundNumber > 0 then
                [ model |> pipsForRoundView roundNumber ]
            else
                []
    in
        div
            [ classList [ ( "round-info", True ) ] ]
            infoCell


pipsForRoundView : RoundNumber -> Model -> Html Msg
pipsForRoundView roundNumber model =
    div [ classList [ ( "pips", True ) ] ]
        (model.rounds
            |> Dict.get roundNumber
            |> Maybe.withDefault Dict.empty
            |> Dict.map (\_ slot -> Tuple.second slot)
            |> randomizePips model.randomPipIndexes
            |> List.map pipView
        )


randomizePips : List Int -> Dict.Dict SlotNumber Pip -> List Pip
randomizePips randomPipIndexes pips =
    randomPipIndexes
        |> List.map
            (\index ->
                pips
                    |> Dict.get index
                    |> Maybe.withDefault PipNoMatch
            )


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
            [ classList [ ( "round-number", True ) ] ]
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


title : List String -> String -> List (Html Msg) -> Html Msg
title classNames txt otherChildren =
    div [ classes ([ "game-state-title" ] ++ classNames) ]
        ([ h5 [] [ text txt ] ] ++ otherChildren)


displayGameStateTitle : Model -> Html Msg
displayGameStateTitle model =
    case model.gameState of
        GameNew ->
            title
                [ "game-state-new" ]
                "Game hasn't started yet"
                []

        GamePlaying _ ->
            title
                [ "game-state-playing" ]
                (guessesCountDescription model)
                []

        GameWon ->
            title
                [ "game-state-end", "game-state-won" ]
                ("You won the game in "
                    ++ String.fromInt model.currentRound
                    ++ " Rounds!"
                )
                [ btn "New Game" "filled" ResetGame [] ]

        GameLost ->
            title
                [ "game-state-end", "game-state-lost" ]
                "Sorry, you lost the game!"
                [ btn "New Game" "filled" ResetGame [] ]


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
