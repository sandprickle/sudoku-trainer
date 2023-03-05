module Solve.Hint exposing
    ( Hint(..)
    , Pair
    , checkNakedPairCandidates
    , comparePair
    , generateHint
    )

import AVL.Set as Set exposing (Set)
import List.Extra
import Solve.Cell
import Solve.Puzzle
import Sudoku.Grid as Grid exposing (Coord, Grid)
import Sudoku.Number as Number exposing (NumSet, Number)



-- Hint Type


type Hint
    = NakedSingle
    | NakedPair



-- Cell type and helpers


type Cell
    = Fixed Coord Number
    | Possible Coord NumSet


convertCell : Coord -> Solve.Cell.Cell -> Cell
convertCell coord cell =
    case cell of
        Solve.Cell.Given num ->
            Fixed coord num

        Solve.Cell.Fixed num _ ->
            Fixed coord num

        Solve.Cell.Possible set _ ->
            Possible coord set



-- Hint Generation


generateHint : Hint -> Solve.Puzzle.Puzzle -> String
generateHint hint puzzle =
    let
        grid =
            Grid.coordMap convertCell puzzle
    in
    case hint of
        NakedSingle ->
            let
                qty =
                    countNakedSingles grid
            in
            if qty >= 1 then
                "Found " ++ String.fromInt qty ++ " naked single"

            else
                "No naked singles"

        NakedPair ->
            let
                nakedPairs =
                    findNakedPairs grid

                qty =
                    Set.size nakedPairs
            in
            if qty >= 2 then
                "Found " ++ String.fromInt qty ++ " naked pairs"

            else if qty == 1 then
                "Found " ++ String.fromInt qty ++ " naked pair"

            else
                "No naked pairs"



-- Singles


countNakedSingles : Grid Cell -> Int
countNakedSingles puzzle =
    puzzle
        |> Grid.coordMap
            (\_ cell ->
                case cell of
                    Fixed _ _ ->
                        0

                    Possible _ numbers ->
                        Number.setSize numbers
            )
        |> Grid.toList
        |> List.filter (\x -> x == 1)
        |> List.length



-- Pairs (Naked, Hidden)


type alias Pair =
    ( Coord, Coord )


emptyPairSet : Set Pair
emptyPairSet =
    Set.emptyWith comparePair


comparePair : Pair -> Pair -> Order
comparePair ( pair1A, pair1B ) ( pair2A, pair2B ) =
    case Grid.compareCoord pair1A pair2A of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            case Grid.compareCoord pair1B pair2B of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    EQ


findNakedPairs : Grid Cell -> Set Pair
findNakedPairs grid =
    let
        rows =
            Grid.toRows grid

        cols =
            Grid.toCols grid

        boxes =
            Grid.toBoxes grid
    in
    [ rows, cols, boxes ]
        |> List.map
            (\houses ->
                houses
                    |> List.map extractNakedPairCandidates
                    |> List.map checkNakedPairCandidates
                    |> List.foldl Set.union emptyPairSet
            )
        |> List.foldl Set.union emptyPairSet


type alias PairCandidate =
    { pos : Coord
    , nums : ( Number, Number )
    }


checkNakedPairCandidates : List PairCandidate -> Set Pair
checkNakedPairCandidates candidates =
    candidates
        |> List.Extra.gatherEqualsBy .nums
        |> List.filterMap
            (\( candidateA, xs ) ->
                List.head xs
                    |> Maybe.andThen
                        (\candidateB ->
                            Just ( candidateA.pos, candidateB.pos )
                        )
            )
        |> Set.fromListWith comparePair


extractNakedPairCandidates : List Cell -> List PairCandidate
extractNakedPairCandidates cells =
    let
        numSetToTuple : NumSet -> Maybe ( Number, Number )
        numSetToTuple set =
            if Number.setSize set /= 2 then
                Nothing

            else
                let
                    list =
                        Number.setToList set

                    first =
                        list
                            |> List.head
                            |> Maybe.withDefault Number.one

                    last =
                        list
                            |> List.drop 1
                            |> List.head
                            |> Maybe.withDefault Number.one
                in
                Just ( first, last )
    in
    cells
        |> List.filterMap
            (\cell ->
                case cell of
                    Fixed _ _ ->
                        Nothing

                    Possible pos set ->
                        set
                            |> numSetToTuple
                            |> Maybe.andThen
                                (\nums ->
                                    Just
                                        { pos = pos
                                        , nums = nums
                                        }
                                )
            )
