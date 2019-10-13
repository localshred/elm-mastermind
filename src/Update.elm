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
                |> applyGeneratedSolution

        ConfigSetTotalSlots totalSlots ->
            model
                |> setTotalSlots totalSlots
                |> applyGeneratedSolution

        GenerateSolution randomPegInts ->
            generateSolution model randomPegInts

        StartGame ->
            startGame model

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
            (not isGameWon) && model.currentRound == model.totalRounds - 1

        updatedModel =
            if isGameWon then
                { model | gameState = GameWon }
            else if isGameLost then
                { model | gameState = GameLost }
            else
                { model | currentRound = model.currentRound + 1 }
    in
        ( updatedModel, Cmd.none )


setTotalRounds : String -> Model -> Model
setTotalRounds totalRounds model =
    let
        totalRoundsInt =
            totalRounds |> String.toInt |> withDefault 10
    in
        { model | totalRounds = totalRoundsInt }


setTotalSlots : String -> Model -> Model
setTotalSlots totalSlots model =
    let
        totalSlotsInt =
            totalSlots |> String.toInt |> withDefault 4
    in
        { model | totalSlots = totalSlotsInt }


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
        ( { model | solution = slots }
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


startGame : Model -> ( Model, Cmd Msg )
startGame model =
    ( { model
        | gameState = GamePlaying
        , rounds = initRounds model.totalRounds model.totalSlots
      }
    , Cmd.none
    )


setSlotPip : Model -> Solution -> SlotNumber -> ( Solution, Slot )
setSlotPip model availableSolution slotNumber =
    let
        peg =
            model.rounds
                |> Dict.get model.currentRound
                |> Maybe.withDefault Dict.empty
                |> Dict.get slotNumber
                |> Maybe.withDefault initSlot
                |> Tuple.first

        solutionSlot =
            availableSolution
                |> Dict.get slotNumber
                |> Maybe.withDefault initSlot

        isColorSlotMatch =
            solutionSlot
                |> \( solutionSlotPeg, _ ) -> solutionSlotPeg == peg

        filteredSlotsForColor =
            availableSolution
                |> Dict.filter (\_ ( solutionSlotPeg, _ ) -> solutionSlotPeg == peg)

        isColorMatch =
            filteredSlotsForColor
                |> \filteredSlots -> Dict.size filteredSlots > 0

        pip =
            if isColorSlotMatch then
                PipColorSlotMatch
            else if isColorMatch then
                PipColorMatch
            else
                PipNoMatch

        updatedAvailableSolution =
            case pip of
                PipNoMatch ->
                    availableSolution

                PipColorMatch ->
                    filteredSlotsForColor
                        |> Dict.keys
                        |> List.head
                        |> Maybe.withDefault -1
                        |> (\keyToRemove -> Dict.remove keyToRemove availableSolution)

                PipColorSlotMatch ->
                    Dict.remove slotNumber availableSolution
    in
        ( updatedAvailableSolution, ( peg, pip ) )


computePips : Model -> Model
computePips model =
    let
        ( _, updatedRound ) =
            Dict.foldl
                (\slotNumber v ( availableSolution, pips ) ->
                    let
                        ( updatedAvailableSolution, slot ) =
                            setSlotPip model availableSolution slotNumber
                    in
                        ( updatedAvailableSolution, List.append pips [ slot ] )
                )
                ( model.solution, [] )
                model.solution

        rounds =
            model.rounds
                |> Dict.insert model.currentRound (buildSlots updatedRound)
    in
        { model | rounds = rounds }


setPeg : Model -> RoundNumber -> SlotNumber -> Peg -> ( Model, Cmd Msg )
setPeg model roundIndex slotIndex peg =
    if model.currentRound /= roundIndex then
        ( model, Cmd.none )
    else
        let
            updateRoundSlots slots =
                slots
                    |> Dict.update slotIndex (Maybe.map <| \_ -> ( peg, PipNoMatch ))

            rounds =
                model.rounds
                    |> Dict.update roundIndex (Maybe.map updateRoundSlots)
        in
            ( { model | rounds = rounds }, Cmd.none )
