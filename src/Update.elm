module Update exposing (..)

import Dict
import Random
import Maybe exposing (withDefault)
import Tuple
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CommitRound ->
            model
                |> computePips
                |> computeNextGameState

        ConfigSetTotalRounds totalRounds ->
            model
                |> setTotalRounds totalRounds

        ConfigSetTotalSlots totalSlots ->
            model
                |> setTotalSlots totalSlots

        GenerateSolution randomPegInts ->
            generateSolution model randomPegInts

        ResetGame ->
            resetGame model

        StartGame ->
            startGame model
                |> applyGeneratedSolution

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


applyGeneratedSolution : Model -> ( Model, Cmd Msg )
applyGeneratedSolution model =
    if model.totalRounds > 0 && model.totalSlots > 0 then
        ( model, generateRandomSolution model.totalSlots )
    else
        ( model, Cmd.none )


generateRandomSolution : SlotNumber -> Cmd Msg
generateRandomSolution totalSlots =
    Random.int 1 totalPegColors
        |> Random.list totalSlots
        |> Random.generate GenerateSolution


generateSolution : Model -> List Int -> ( Model, Cmd Msg )
generateSolution model randomPegColors =
    let
        slots =
            randomPegColors
                |> List.map (\randomPegNumber -> ( intToPeg randomPegNumber, PipNoMatch ))
                |> buildSlots
    in
        ( { model
            | solution = slots
          }
        , Cmd.none
        )


intToPeg : Int -> Peg
intToPeg randomPegNumber =
    case randomPegNumber of
        1 ->
            PegBlack

        2 ->
            PegBlue

        3 ->
            PegGreen

        4 ->
            PegRed

        5 ->
            PegWhite

        6 ->
            PegYellow

        _ ->
            PegNone


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
