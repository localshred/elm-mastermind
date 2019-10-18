module Update exposing (..)

import Dict
import Random
import Random.List exposing (shuffle)
import Maybe exposing (withDefault)
import Tuple
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplyGenerators randomizers ->
            model
                |> applyGenerators randomizers

        CommitRound ->
            model
                |> computePips
                |> computeNextGameState

        ConfigSet ConfigKeyTotalRounds totalRounds ->
            model
                |> setTotalRounds totalRounds

        ConfigSet ConfigKeyTotalSlots totalSlots ->
            model
                |> setTotalSlots totalSlots

        KeyPress key ->
            model
                |> onKeyPress key

        ResetGame ->
            model
                |> resetGame

        StartGame ->
            model
                |> startGame
                |> dispatchApplyGeneratorsMsg

        SelectSlot roundNumber slotNumber ->
            selectSlot roundNumber slotNumber model

        SetPeg roundNumber slotNumber peg ->
            setPeg model roundNumber slotNumber peg


computeNextGameState : Model -> ( Model, Cmd Msg )
computeNextGameState model =
    let
        extractPegs slots =
            slots
                |> Dict.values
                |> List.map Tuple.first

        solutionPegs =
            extractPegs model.solution

        roundPegs =
            model.rounds
                |> Dict.get model.currentRound
                |> Maybe.withDefault Dict.empty
                |> extractPegs

        isGameWon =
            solutionPegs == roundPegs

        isGameLost =
            (not isGameWon) && model.currentRound == model.totalRounds

        updatedModel =
            if isGameWon then
                { model | gameState = GameWon }
            else if isGameLost then
                { model | gameState = GameLost }
            else
                { model | currentRound = model.currentRound + 1 }
    in
        ( updatedModel, Cmd.none )


setTotalRounds : String -> Model -> ( Model, Cmd Msg )
setTotalRounds totalRounds model =
    let
        totalRoundsInt =
            totalRounds |> String.toInt |> withDefault 10
    in
        ( { model
            | totalRounds = totalRoundsInt
          }
        , Cmd.none
        )


setTotalSlots : String -> Model -> ( Model, Cmd Msg )
setTotalSlots totalSlots model =
    let
        totalSlotsInt =
            totalSlots |> String.toInt |> withDefault 4
    in
        ( { model
            | totalSlots = totalSlotsInt
          }
        , Cmd.none
        )


dispatchApplyGeneratorsMsg : Model -> ( Model, Cmd Msg )
dispatchApplyGeneratorsMsg model =
    if model.totalRounds > 0 && model.totalSlots > 0 then
        ( model
        , Random.generate ApplyGenerators <|
            Random.pair
                (generateRandomSolution model.totalSlots)
                (generatePipRandomizer model.totalSlots)
        )
    else
        ( model, Cmd.none )


generatePipRandomizer : Int -> Random.Generator (List Int)
generatePipRandomizer totalSlots =
    (totalSlots - 1)
        |> List.range 0
        |> shuffle


generateRandomSolution : SlotNumber -> Random.Generator (List Int)
generateRandomSolution totalSlots =
    totalPegColors
        |> Random.int 1
        |> Random.list totalSlots


applyGenerators : ( List Int, List Int ) -> Model -> ( Model, Cmd Msg )
applyGenerators ( randomizedSolution, randomizedPipIndexes ) model =
    ( { model
        | randomPipIndexes = randomizedPipIndexes
        , solution =
            randomizedSolution
                |> List.map buildSlot
                |> buildSlots
      }
    , Cmd.none
    )


resetGame : Model -> ( Model, Cmd Msg )
resetGame model =
    ( { model
        | currentRound = 1
        , gameState = GameNew
        , rounds = Dict.empty
        , solution = Dict.empty
      }
    , Cmd.none
    )


startGame : Model -> Model
startGame model =
    { model
        | gameState = GamePlaying PegPickerClosed
        , rounds = initRounds model.totalRounds model.totalSlots
    }


onKeyPress : String -> Model -> ( Model, Cmd Msg )
onKeyPress key model =
    case model.gameState of
        GamePlaying (PegPickerOpen _ _) ->
            case key of
                "Escape" ->
                    closePegPicker model

                "b" ->
                    setPegForKeyPress PegBlack model

                "g" ->
                    setPegForKeyPress PegGreen model

                "r" ->
                    setPegForKeyPress PegRed model

                "u" ->
                    setPegForKeyPress PegBlue model

                "w" ->
                    setPegForKeyPress PegWhite model

                "y" ->
                    setPegForKeyPress PegYellow model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


setPegForKeyPress : Peg -> Model -> ( Model, Cmd Msg )
setPegForKeyPress peg model =
    case model.gameState of
        GamePlaying (PegPickerOpen roundIndex slotIndex) ->
            setPeg model roundIndex slotIndex peg

        _ ->
            ( model, Cmd.none )


closePegPicker : Model -> ( Model, Cmd Msg )
closePegPicker model =
    ( { model
        | gameState = GamePlaying PegPickerClosed
      }
    , Cmd.none
    )


computePips : Model -> Model
computePips model =
    let
        currentRound =
            model.rounds
                |> Dict.get model.currentRound
                |> Maybe.withDefault Dict.empty

        updatedRound =
            setRoundPips model.solution currentRound

        rounds =
            model.rounds
                |> Dict.insert model.currentRound updatedRound
    in
        { model
            | rounds = rounds
        }


selectSlot : RoundNumber -> SlotNumber -> Model -> ( Model, Cmd Msg )
selectSlot roundNumber slotNumber model =
    ( { model
        | gameState = GamePlaying (PegPickerOpen roundNumber slotNumber)
      }
    , Cmd.none
    )


setPeg : Model -> RoundNumber -> SlotNumber -> Peg -> ( Model, Cmd Msg )
setPeg model roundIndex slotIndex peg =
    if model.currentRound /= roundIndex then
        ( model, Cmd.none )
    else
        let
            setUnmatchedPeg _ =
                ( peg, PipNoMatch )

            updateRoundSlots slots =
                slots
                    |> Dict.update slotIndex (Maybe.map setUnmatchedPeg)

            rounds =
                model.rounds
                    |> Dict.update roundIndex (Maybe.map updateRoundSlots)
        in
            ( { model
                | rounds = rounds
                , gameState = GamePlaying PegPickerClosed
              }
            , Cmd.none
            )


pegAtSlot : SlotNumber -> Slots -> Peg
pegAtSlot slotNumber slots =
    slots
        |> Dict.get slotNumber
        |> Maybe.withDefault initSlot
        |> Tuple.first


pipAtSlot : SlotNumber -> Slots -> Pip
pipAtSlot slotNumber slots =
    slots
        |> Dict.get slotNumber
        |> Maybe.withDefault initSlot
        |> Tuple.second


setRoundPips : Solution -> Slots -> Slots
setRoundPips incomingSolution incomingRound =
    let
        colorSlotMatchReducer roundSlotNumber ( peg, pip ) ( solutionLookup, roundLookup, round ) =
            if pegAtSlot roundSlotNumber solutionLookup == peg then
                ( solutionLookup |> Dict.remove roundSlotNumber
                , roundLookup |> Dict.remove roundSlotNumber
                , round |> Dict.insert roundSlotNumber ( peg, PipColorSlotMatch )
                )
            else
                ( solutionLookup, roundLookup, round )

        colorMatchMapper roundSlotNumber ( peg, _ ) ( solutionLookup, roundLookup, round ) =
            let
                isPegAlreadyMatched =
                    pipAtSlot roundSlotNumber round /= PipNoMatch

                colorMatches =
                    if not isPegAlreadyMatched then
                        solutionLookup
                            |> Dict.filter
                                (\solutionSlotNumber ( solutionPeg, _ ) ->
                                    peg == solutionPeg && roundSlotNumber /= solutionSlotNumber
                                )
                    else
                        Dict.empty
            in
                if Dict.size colorMatches > 0 then
                    ( (Dict.foldl
                        (\matchedSlotNumber _ remainingSolutions -> Dict.remove matchedSlotNumber remainingSolutions)
                        solutionLookup
                        colorMatches
                      )
                    , roundLookup |> Dict.remove roundSlotNumber
                    , round |> Dict.insert roundSlotNumber ( peg, PipColorMatch )
                    )
                else
                    ( solutionLookup, roundLookup, round )

        ( s, r, resultRound ) =
            incomingRound
                |> Dict.foldl colorSlotMatchReducer ( incomingSolution, incomingRound, incomingRound )
                |> (\previousAcc -> Dict.foldl colorMatchMapper previousAcc incomingRound)
    in
        resultRound
