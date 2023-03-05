module Solve.HintTests exposing (..)

import AVL.Set as Set
import Expect
import Solve.Hint as Hint exposing (comparePair)
import Sudoku.Number as Number
import Test exposing (..)


suite : Test
suite =
    describe "Solve.Hint Module"
        [ describe "Solve.Hint.checkNakedPairCandidates"
            [ test "Return empty set if no pairs" <|
                \_ ->
                    [ { pos = { x = 0, y = 1 }
                      , nums = ( Number.two, Number.four )
                      }
                    , { pos = { x = 0, y = 6 }
                      , nums = ( Number.three, Number.nine )
                      }
                    ]
                        |> Hint.checkNakedPairCandidates
                        |> Expect.equal (Set.emptyWith comparePair)
            , test "Return set of one if only one pair" <|
                \_ ->
                    [ { pos = { x = 0, y = 1 }
                      , nums = ( Number.two, Number.four )
                      }
                    , { pos = { x = 0, y = 6 }
                      , nums = ( Number.three, Number.nine )
                      }
                    , { pos = { x = 0, y = 7 }
                      , nums = ( Number.two, Number.four )
                      }
                    ]
                        |> Hint.checkNakedPairCandidates
                        |> Expect.equal
                            (Set.fromListWith comparePair
                                [ ( { x = 0, y = 1 }
                                  , { x = 0, y = 7 }
                                  )
                                ]
                            )
            , test "Return set of two if two pairs" <|
                \_ ->
                    [ { pos = { x = 0, y = 1 }
                      , nums = ( Number.two, Number.four )
                      }
                    , { pos = { x = 0, y = 6 }
                      , nums = ( Number.three, Number.nine )
                      }
                    , { pos = { x = 0, y = 7 }
                      , nums = ( Number.two, Number.four )
                      }
                    , { pos = { x = 0, y = 8 }
                      , nums = ( Number.three, Number.nine )
                      }
                    , { pos = { x = 0, y = 9 }
                      , nums = ( Number.five, Number.eight )
                      }
                    ]
                        |> Hint.checkNakedPairCandidates
                        |> Expect.equal
                            (Set.fromListWith comparePair
                                [ ( { x = 0, y = 1 }
                                  , { x = 0, y = 7 }
                                  )
                                , ( { x = 0, y = 6 }
                                  , { x = 0, y = 8 }
                                  )
                                ]
                            )
            ]
        ]
