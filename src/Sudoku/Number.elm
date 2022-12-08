module Sudoku.Number exposing
    ( NumSet
    , Number
    , all
    , decoder
    , eight
    , emptySet
    , encode
    , encodeSet
    , five
    , four
    , fromChar
    , fromString
    , fullSet
    , nine
    , one
    , setDecoder
    , setDiff
    , setFromList
    , setInsert
    , setMember
    , setRemove
    , setToList
    , seven
    , six
    , three
    , toString
    , two
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)


type Number
    = Number Int



-- Primitive Conversions


fromChar : Char -> Maybe Number
fromChar char =
    case char of
        '1' ->
            Just (Number 1)

        '2' ->
            Just (Number 2)

        '3' ->
            Just (Number 3)

        '4' ->
            Just (Number 4)

        '5' ->
            Just (Number 5)

        '6' ->
            Just (Number 6)

        '7' ->
            Just (Number 7)

        '8' ->
            Just (Number 8)

        '9' ->
            Just (Number 9)

        _ ->
            Nothing


toString : Number -> String
toString (Number int) =
    String.fromInt int


fromString : String -> Maybe Number
fromString str =
    case str of
        "1" ->
            Just (Number 1)

        "2" ->
            Just (Number 2)

        "3" ->
            Just (Number 3)

        "4" ->
            Just (Number 4)

        "5" ->
            Just (Number 5)

        "6" ->
            Just (Number 6)

        "7" ->
            Just (Number 7)

        "8" ->
            Just (Number 8)

        "9" ->
            Just (Number 9)

        _ ->
            Nothing


toInt : Number -> Int
toInt (Number int) =
    int


fromInt : Int -> Maybe Number
fromInt int =
    case int of
        1 ->
            Just (Number 1)

        2 ->
            Just (Number 2)

        3 ->
            Just (Number 3)

        4 ->
            Just (Number 4)

        5 ->
            Just (Number 5)

        6 ->
            Just (Number 6)

        7 ->
            Just (Number 7)

        8 ->
            Just (Number 8)

        9 ->
            Just (Number 9)

        _ ->
            Nothing



-- NumSet Type


{-| A set of Numbers
-}
type NumSet
    = NumSet (Set Int)


{-| A set of all Numbers
-}
fullSet : NumSet
fullSet =
    List.range 1 9 |> Set.fromList |> NumSet


emptySet : NumSet
emptySet =
    [] |> Set.fromList |> NumSet


setInsert : Number -> NumSet -> NumSet
setInsert (Number num) (NumSet set) =
    NumSet (Set.insert num set)


setRemove : Number -> NumSet -> NumSet
setRemove (Number num) (NumSet set) =
    NumSet (Set.remove num set)


{-| Get the difference between the first set and the second.
Keeps values that do not appear in the second set.
-}
setDiff : NumSet -> NumSet -> NumSet
setDiff (NumSet set1) (NumSet set2) =
    NumSet (Set.diff set1 set2)


setFromList : List Number -> NumSet
setFromList list =
    list
        |> List.map toInt
        |> Set.fromList
        |> NumSet


setMember : Number -> NumSet -> Bool
setMember (Number num) (NumSet set) =
    Set.member num set


setToList : NumSet -> List Number
setToList (NumSet set) =
    set
        |> Set.toList
        |> List.map
            (\int -> fromInt int |> Maybe.withDefault one)


encodeSet : NumSet -> Encode.Value
encodeSet (NumSet set) =
    Encode.set Encode.int set


setDecoder : Decoder NumSet
setDecoder =
    Decode.list decoder
        |> Decode.andThen
            (\list ->
                Decode.succeed (setFromList list)
            )



-- Hardcoded Numbers


one : Number
one =
    Number 1


two : Number
two =
    Number 2


three : Number
three =
    Number 3


four : Number
four =
    Number 4


five : Number
five =
    Number 5


six : Number
six =
    Number 6


seven : Number
seven =
    Number 7


eight : Number
eight =
    Number 8


nine : Number
nine =
    Number 9


all : List Number
all =
    [ one, two, three, four, five, six, seven, eight, nine ]



-- Encode/Decode


encode : Number -> Encode.Value
encode (Number num) =
    Encode.int num


decoder : Decoder Number
decoder =
    Decode.int
        |> Decode.andThen
            (\num ->
                if num >= 1 && num <= 9 then
                    Decode.succeed (Number num)

                else
                    Decode.fail "Number must be between 1 and 9"
            )
