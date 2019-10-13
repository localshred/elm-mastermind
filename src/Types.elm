module Types exposing (..)

import Dict exposing (..)
import Tuple exposing (pair)


type GameState
    = GameNew
    | GamePlaying
    | GameWon
    | GameLost


type alias Model =
    { currentRound : RoundNumber
    , gameState : GameState
    , rounds : Rounds
    , solution : Solution
    , totalRounds : RoundNumber
    , totalSlots : SlotNumber
    }


type Msg
    = CommitRound
    | ConfigSetTotalRounds String
    | ConfigSetTotalSlots String
    | GenerateSolution (List Int)
    | StartGame
    | SetPeg RoundNumber SlotNumber Peg


type Peg
    = PegNone
    | PegBlack
    | PegBlue
    | PegGreen
    | PegRed
    | PegWhite
    | PegYellow


totalPegColors =
    6


type Pip
    = PipNoMatch
    | PipColorMatch
    | PipColorSlotMatch


type alias Rounds =
    Dict RoundNumber Slots


type alias RoundNumber =
    Int


type alias Slot =
    ( Peg, Pip )


type alias SlotNumber =
    Int


type alias Slots =
    Dict SlotNumber Slot


type alias Solution =
    Slots


init : () -> ( Model, Cmd Msg )
init flags =
    ( { currentRound = 1
      , gameState = GameNew
      , solution = Dict.empty
      , rounds = Dict.empty
      , totalRounds = 10
      , totalSlots = 4
      }
    , Cmd.none
    )


initRounds : RoundNumber -> SlotNumber -> Dict RoundNumber Slots
initRounds totalRounds totalSlots =
    List.range 0 (totalRounds - 1)
        |> List.map (\index -> ( index, initRound totalSlots index ))
        |> Dict.fromList


initSlot : Slot
initSlot =
    ( PegNone, PipNoMatch )


initRound : RoundNumber -> SlotNumber -> Slots
initRound totalSlots roundNumber =
    initSlot
        |> List.repeat totalSlots
        |> buildSlots


buildSlots : List Slot -> Slots
buildSlots slots =
    slots
        |> List.indexedMap pair
        |> Dict.fromList


isPegSet : SlotNumber -> Slot -> Bool
isPegSet _ slot =
    PegNone /= Tuple.first slot


areRoundSlotsFull : RoundNumber -> Model -> Bool
areRoundSlotsFull roundIndex model =
    let
        areAllPegsSet roundSlots =
            roundSlots
                |> Dict.filter isPegSet
                |> \slottedPegs -> Dict.size slottedPegs == model.totalSlots
    in
        model.rounds
            |> Dict.get roundIndex
            |> Maybe.map areAllPegsSet
            |> Maybe.withDefault False
