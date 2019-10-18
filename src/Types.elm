module Types exposing (..)

import Dict exposing (..)
import Tuple exposing (pair)


type GameState
    = GameNew
    | GamePlaying PegPickerState
    | GameWon
    | GameLost


type alias Model =
    { currentRound : RoundNumber
    , gameState : GameState
    , randomPipIndexes : List Int
    , rounds : Rounds
    , solution : Solution
    , totalRounds : RoundNumber
    , totalSlots : SlotNumber
    }


type ConfigKey
    = ConfigKeyTotalSlots
    | ConfigKeyTotalRounds


type Msg
    = ApplyGenerators ( List Int, List Int )
    | CommitRound
    | ConfigSet ConfigKey String
    | KeyPress String
    | ResetGame
    | StartGame
    | SelectSlot RoundNumber SlotNumber
    | SetPeg RoundNumber SlotNumber Peg


type Peg
    = PegNone
    | PegBlack
    | PegBlue
    | PegGreen
    | PegRed
    | PegWhite
    | PegYellow


type PegPickerState
    = PegPickerClosed
    | PegPickerOpen RoundNumber SlotNumber


allPegs =
    [ PegBlack
    , PegBlue
    , PegGreen
    , PegRed
    , PegWhite
    , PegYellow
    ]


totalPegColors =
    allPegs |> List.length


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
      , randomPipIndexes = []
      , rounds = Dict.empty
      , solution = Dict.empty
      , totalRounds = 10
      , totalSlots = 4
      }
    , Cmd.none
    )


initRounds : RoundNumber -> SlotNumber -> Dict RoundNumber Slots
initRounds totalRounds totalSlots =
    List.range 1 totalRounds
        |> List.map (\index -> ( index, initRound totalSlots index ))
        |> Dict.fromList


initSlot : Slot
initSlot =
    buildSlot 0


buildSlot : Int -> Slot
buildSlot pegInt =
    case pegInt of
        1 ->
            ( PegBlack, PipNoMatch )

        2 ->
            ( PegBlue, PipNoMatch )

        3 ->
            ( PegGreen, PipNoMatch )

        4 ->
            ( PegRed, PipNoMatch )

        5 ->
            ( PegWhite, PipNoMatch )

        6 ->
            ( PegYellow, PipNoMatch )

        _ ->
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
