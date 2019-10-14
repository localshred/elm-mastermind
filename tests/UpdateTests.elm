module UpdateTests exposing (..)

import Dict
import Types exposing (..)
import Update exposing (setRoundPips, computePips, update)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Update"
        [ describe "update"
            [ test "CommitRound advance to next round" <|
                \_ ->
                    let
                        solution =
                            buildSlots [ red, yellow, red, blue ]

                        rounds =
                            initRounds 2 4
                                |> Dict.insert 1
                                    (buildSlots [ black, blue, black, blue ])

                        model =
                            buildPlayingModelForTest 4 solution []
                                |> \m -> { m | rounds = rounds }

                        ( result, _ ) =
                            model
                                |> update CommitRound

                        expectedRound =
                            buildSlots [ black, blue, black, ( PegBlue, PipColorSlotMatch ) ]
                    in
                        result
                            |> Expect.all
                                [ \m -> m.gameState |> Expect.equal (GamePlaying PegPickerClosed)
                                , \m -> m.currentRound |> Expect.equal 2
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 1
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
              -- test
            , test "CommitRound GameWon" <|
                \_ ->
                    let
                        solution =
                            buildSlots [ green, blue, white, black ]

                        rounds =
                            initRounds 2 4
                                |> Dict.insert 1 (buildSlots [ green, blue, white, black ])

                        model =
                            buildPlayingModelForTest 4 solution []
                                |> \m -> { m | rounds = rounds }

                        ( result, _ ) =
                            model
                                |> update CommitRound

                        expectedRound =
                            buildSlots
                                [ ( PegGreen, PipColorSlotMatch )
                                , ( PegBlue, PipColorSlotMatch )
                                , ( PegWhite, PipColorSlotMatch )
                                , ( PegBlack, PipColorSlotMatch )
                                ]
                    in
                        result
                            |> Expect.all
                                [ \m -> m.gameState |> Expect.equal GameWon
                                , \m -> m.currentRound |> Expect.equal 1
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 1
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
              -- test
            , test "CommitRound GameLost" <|
                \_ ->
                    let
                        solution =
                            buildSlots [ green, blue, white, black ]

                        rounds =
                            initRounds 1 4
                                |> Dict.insert 1 (buildSlots [ green, green, white, black ])

                        ( result, _ ) =
                            buildPlayingModelForTest 4 solution []
                                |> (\m -> { m | rounds = rounds, totalRounds = Dict.size rounds })
                                |> update CommitRound

                        expectedRound =
                            buildSlots
                                [ ( PegGreen, PipColorSlotMatch )
                                , green
                                , ( PegWhite, PipColorSlotMatch )
                                , ( PegBlack, PipColorSlotMatch )
                                ]
                    in
                        result
                            |> Expect.all
                                [ \m -> m.gameState |> Expect.equal GameLost
                                , \m -> m.currentRound |> Expect.equal 1
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 1
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
            ]
        , describe "computePips"
            [ test "solved game" <|
                \_ ->
                    let
                        solution =
                            buildSlots [ green, blue, white, black ]

                        round =
                            buildSlots [ green, blue, white, black ]

                        result =
                            [ round ]
                                |> buildPlayingModelForTest 4 solution
                                |> computePips

                        actualRound =
                            result
                                |> .rounds
                                |> Dict.get result.currentRound
                                |> Maybe.withDefault Dict.empty

                        expectedRound =
                            buildSlots
                                [ ( PegGreen, PipColorSlotMatch )
                                , ( PegBlue, PipColorSlotMatch )
                                , ( PegWhite, PipColorSlotMatch )
                                , ( PegBlack, PipColorSlotMatch )
                                ]
                    in
                        actualRound
                            |> Expect.equal expectedRound
            ]
        , describe "setRoundPips"
            ([ { desc = "one slot match"
               , solution = buildSlots [ green ]
               , round = buildSlots [ green ]
               , expectedRound = buildSlots [ ( PegGreen, PipColorSlotMatch ) ]
               }
             , { desc = "one slot no match"
               , solution = buildSlots [ blue ]
               , round = buildSlots [ green ]
               , expectedRound = buildSlots [ green ]
               }
             , { desc = "two color slot matches"
               , solution = buildSlots [ blue, blue ]
               , round = buildSlots [ blue, blue ]
               , expectedRound = buildSlots [ ( PegBlue, PipColorSlotMatch ), ( PegBlue, PipColorSlotMatch ) ]
               }
             , { desc = "two color matches"
               , solution = buildSlots [ blue, green ]
               , round = buildSlots [ green, blue ]
               , expectedRound = buildSlots [ ( PegGreen, PipColorMatch ), ( PegBlue, PipColorMatch ) ]
               }
             , { desc = "two slots no match"
               , solution = buildSlots [ blue, blue ]
               , round = buildSlots [ green, green ]
               , expectedRound = buildSlots [ green, green ]
               }
             , { desc = "two round pegs matching slot in solution"
               , solution = buildSlots [ red, yellow, red, blue ]
               , round = buildSlots [ black, blue, black, blue ]
               , expectedRound = buildSlots [ black, blue, black, ( PegBlue, PipColorSlotMatch ) ]
               }
             , { desc = "one color match"
               , solution = buildSlots [ red, yellow, blue, white ]
               , round = buildSlots [ black, blue, black, green ]
               , expectedRound = buildSlots [ black, ( PegBlue, PipColorMatch ), black, green ]
               }
             , { desc = "four color slot matches"
               , solution = buildSlots [ red, blue, green, white ]
               , round = buildSlots [ red, blue, green, white ]
               , expectedRound =
                    buildSlots
                        [ ( PegRed, PipColorSlotMatch )
                        , ( PegBlue, PipColorSlotMatch )
                        , ( PegGreen, PipColorSlotMatch )
                        , ( PegWhite, PipColorSlotMatch )
                        ]
               }
             , { desc = "four color matches"
               , solution = buildSlots [ white, green, blue, red ]
               , round = buildSlots [ red, blue, green, white ]
               , expectedRound =
                    buildSlots
                        [ ( PegRed, PipColorMatch )
                        , ( PegBlue, PipColorMatch )
                        , ( PegGreen, PipColorMatch )
                        , ( PegWhite, PipColorMatch )
                        ]
               }
             , { desc = "four color matches 1"
               , solution = buildSlots [ white, blue, black, black ]
               , round = buildSlots [ blue, black, black, white ]
               , expectedRound =
                    buildSlots
                        [ ( PegBlue, PipColorMatch )
                        , ( PegBlack, PipColorMatch )
                        , ( PegBlack, PipColorSlotMatch )
                        , ( PegWhite, PipColorMatch )
                        ]
               }
             , { desc = "three color matches, one color slot match"
               , solution = buildSlots [ yellow, yellow, red, black ]
               , round = buildSlots [ yellow, red, black, yellow ]
               , expectedRound =
                    buildSlots
                        [ ( PegYellow, PipColorSlotMatch )
                        , ( PegRed, PipColorMatch )
                        , ( PegBlack, PipColorMatch )
                        , ( PegYellow, PipColorMatch )
                        ]
               }
             , { desc = "asf"
               , solution = buildSlots [ green, green, green, white ]
               , round = buildSlots [ blue, green, blue, white ]
               , expectedRound =
                    buildSlots
                        [ ( PegBlue, PipNoMatch )
                        , ( PegGreen, PipColorSlotMatch )
                        , ( PegBlue, PipNoMatch )
                        , ( PegWhite, PipColorSlotMatch )
                        ]
               }
             ]
                |> List.map testThing
            )
        ]


none =
    ( PegNone, PipNoMatch )


green =
    ( PegGreen, PipNoMatch )


yellow =
    ( PegYellow, PipNoMatch )


blue =
    ( PegBlue, PipNoMatch )


red =
    ( PegRed, PipNoMatch )


black =
    ( PegBlack, PipNoMatch )


white =
    ( PegWhite, PipNoMatch )


testThing foo =
    test foo.desc <|
        \_ ->
            foo.round
                |> (setRoundPips foo.solution)
                |> Expect.equal foo.expectedRound


buildPlayingModelForTest : Int -> Slots -> List Slots -> Model
buildPlayingModelForTest totalSlots solution rounds =
    { currentRound = 1
    , gameState = GamePlaying PegPickerClosed
    , solution = solution
    , rounds = buildRounds rounds
    , totalRounds = List.length rounds
    , totalSlots = totalSlots
    }


buildRounds : List Slots -> Rounds
buildRounds rounds =
    rounds
        |> List.indexedMap (\index round -> ( index + 1, round ))
        |> Dict.fromList
