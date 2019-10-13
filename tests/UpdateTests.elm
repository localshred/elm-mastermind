module UpdateTests exposing (..)

import Dict
import Types exposing (..)
import Update exposing (computePips, setSlotPip, update)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Update"
        [ describe "setSlotPip"
            [ test "color slot match" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        expectedAvailableSolution =
                            solution
                                |> Dict.remove 0

                        round =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                ]

                        model =
                            buildPlayingModelForTest 4 solution [ round ]

                        expected =
                            ( expectedAvailableSolution, ( PegGreen, PipColorSlotMatch ) )

                        actual =
                            setSlotPip model model.solution 0
                    in
                        actual
                            |> Expect.equal expected
              -- new test
            , test "single color matches solution in other slot" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        expectedAvailableSolution =
                            solution
                                |> Dict.remove 1

                        round =
                            buildSlots
                                [ ( PegBlue, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                ]

                        model =
                            buildPlayingModelForTest 4 solution [ round ]

                        expected =
                            ( expectedAvailableSolution, ( PegBlue, PipColorMatch ) )

                        actual =
                            setSlotPip model model.solution 0
                    in
                        actual
                            |> Expect.equal expected
              -- new test
            , test "multiple color matches solution in other slot only once" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                ]

                        expectedAvailableSolution =
                            solution
                                |> Dict.remove 1

                        round =
                            buildSlots
                                [ ( PegBlue, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                ]

                        model =
                            buildPlayingModelForTest 4 solution [ round ]

                        expected =
                            ( expectedAvailableSolution, ( PegBlue, PipColorMatch ) )

                        actual =
                            setSlotPip model model.solution 0
                    in
                        actual
                            |> Expect.equal expected
              -- new test
            , test "no matches" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        expectedAvailableSolution =
                            solution

                        round =
                            buildSlots
                                [ ( PegRed, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                , ( PegNone, PipNoMatch )
                                ]

                        model =
                            buildPlayingModelForTest 4 solution [ round ]

                        expected =
                            ( expectedAvailableSolution, ( PegRed, PipNoMatch ) )

                        actual =
                            setSlotPip model model.solution 0
                    in
                        actual
                            |> Expect.equal expected
            ]
        , describe "update"
            [ test "CommitRound advance to next round" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        rounds =
                            initRounds 2 4
                                |> Dict.insert 0
                                    (buildSlots
                                        [ ( PegGreen, PipNoMatch )
                                        , ( PegGreen, PipNoMatch )
                                        , ( PegWhite, PipNoMatch )
                                        , ( PegBlack, PipNoMatch )
                                        ]
                                    )

                        model =
                            buildPlayingModelForTest 4 solution []
                                |> \m -> { m | rounds = rounds }

                        ( result, _ ) =
                            model
                                |> update CommitRound

                        expectedRound =
                            buildSlots
                                [ ( PegGreen, PipColorSlotMatch )
                                , ( PegGreen, PipNoMatch )
                                , ( PegWhite, PipColorSlotMatch )
                                , ( PegBlack, PipColorSlotMatch )
                                ]
                    in
                        result
                            |> Expect.all
                                [ \m -> m.gameState |> Expect.equal GamePlaying
                                , \m -> m.currentRound |> Expect.equal 1
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 0
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
              -- test
            , test "CommitRound GameWon" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        rounds =
                            initRounds 2 4
                                |> Dict.insert 0
                                    (buildSlots
                                        [ ( PegGreen, PipNoMatch )
                                        , ( PegBlue, PipNoMatch )
                                        , ( PegWhite, PipNoMatch )
                                        , ( PegBlack, PipNoMatch )
                                        ]
                                    )

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
                                , \m -> m.currentRound |> Expect.equal 0
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 0
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
              -- test
            , test "CommitRound GameLost" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        rounds =
                            initRounds 1 4
                                |> Dict.insert 0
                                    (buildSlots
                                        [ ( PegGreen, PipNoMatch )
                                        , ( PegGreen, PipNoMatch )
                                        , ( PegWhite, PipNoMatch )
                                        , ( PegBlack, PipNoMatch )
                                        ]
                                    )

                        ( result, _ ) =
                            buildPlayingModelForTest 4 solution []
                                |> (\m -> { m | rounds = rounds, totalRounds = Dict.size rounds })
                                |> update CommitRound

                        expectedRound =
                            buildSlots
                                [ ( PegGreen, PipColorSlotMatch )
                                , ( PegGreen, PipNoMatch )
                                , ( PegWhite, PipColorSlotMatch )
                                , ( PegBlack, PipColorSlotMatch )
                                ]
                    in
                        result
                            |> Expect.all
                                [ \m -> m.gameState |> Expect.equal GameLost
                                , \m -> m.currentRound |> Expect.equal 0
                                , \m ->
                                    m
                                        |> .rounds
                                        |> Dict.get 0
                                        |> Maybe.withDefault Dict.empty
                                        |> Expect.equal expectedRound
                                ]
            ]
        , describe "computePips"
            [ test "solved game" <|
                \_ ->
                    let
                        solution =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

                        round =
                            buildSlots
                                [ ( PegGreen, PipNoMatch )
                                , ( PegBlue, PipNoMatch )
                                , ( PegWhite, PipNoMatch )
                                , ( PegBlack, PipNoMatch )
                                ]

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
        ]


buildPlayingModelForTest : Int -> Slots -> List Slots -> Model
buildPlayingModelForTest totalSlots solution rounds =
    { currentRound = 0
    , gameState = GamePlaying
    , solution = solution
    , rounds = buildRounds rounds
    , totalRounds = List.length rounds
    , totalSlots = totalSlots
    }


buildRounds : List Slots -> Rounds
buildRounds rounds =
    rounds
        |> List.indexedMap Tuple.pair
        |> Dict.fromList
