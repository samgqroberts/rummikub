module RummikubEngine.Utils exposing (allColorsTheSame, allColorsUnique, allNumbersSequential, allNumbersTheSame, allUniqueTiles, boardToString, colorToInt, colorToString, colors, containsAll, createTile, createTilesForColor, defaultNumPlayers, defaultStartingPlayerTileCount, defaultTileDuplicates, defaultingToEmptyList, generateAllTiles, getColor, getNumber, groupToString, isValidBoard, isValidGroup, listDiff, moveTile, nextPlayerTurn, numPlayers, numberToInt, numberToString, numbers, playerHandToString, removeAt, replaceAt, takeRandomTile, takeTiles, tileListToString, tileToString)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed)
import RummikubEngine.Models exposing (Board, Color(..), GameState, Group, Number(..), PlayerHand, Tile)
import Tuple


colors =
    [ Black, Red, Orange, Blue ]


numbers =
    [ One
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Eleven
    , Twelve
    , Thirteen
    ]


{-| how many (Black, One) tiles are there?
-}
defaultTileDuplicates =
    2


defaultNumPlayers =
    4


defaultStartingPlayerTileCount =
    14


getColor : Tile -> Color
getColor tile =
    Tuple.first tile


getNumber : Tile -> Number
getNumber tile =
    Tuple.second tile


createTile : Color -> Number -> Tile
createTile color number =
    ( color, number )


colorToInt : Color -> Int
colorToInt color =
    case color of
        Black ->
            0

        Red ->
            1

        Orange ->
            2

        Blue ->
            3


numberToInt : Number -> Int
numberToInt number =
    case number of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Eleven ->
            11

        Twelve ->
            12

        Thirteen ->
            13


colorToString : Color -> String
colorToString color =
    case color of
        Black ->
            "Black"

        Red ->
            "Red"

        Orange ->
            "Orange"

        Blue ->
            "Blue"


numberToString : Number -> String
numberToString number =
    case number of
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"

        Seven ->
            "Seven"

        Eight ->
            "Eight"

        Nine ->
            "Nine"

        Ten ->
            "Ten"

        Eleven ->
            "Eleven"

        Twelve ->
            "Twelve"

        Thirteen ->
            "Thirteen"


tileListToString : List Tile -> String
tileListToString tileList =
    "[" ++ String.join ", " (List.map tileToString tileList) ++ "]"


groupToString : Group -> String
groupToString group =
    tileListToString group


tileToString : Tile -> String
tileToString tile =
    let
        colorString =
            colorToString (getColor tile)

        numberString =
            numberToString (getNumber tile)
    in
    "(" ++ colorString ++ ", " ++ numberString ++ ")"


boardToString : Board -> String
boardToString board =
    "[" ++ String.join ", " (List.map groupToString board) ++ "]"


playerHandToString : PlayerHand -> String
playerHandToString playerHand =
    tileListToString playerHand


createTilesForColor : Color -> List Tile
createTilesForColor color =
    numbers
        |> List.map (\number -> ( color, number ))


generateAllTiles : Int -> List Tile
generateAllTiles tileDuplicates =
    allUniqueTiles
        |> List.concatMap (\uniqueTile -> List.repeat tileDuplicates uniqueTile)


allUniqueTiles : List Tile
allUniqueTiles =
    colors
        |> List.map createTilesForColor
        |> List.concat


takeRandomTile : Seed -> List Tile -> ( List Tile, Tile )
takeRandomTile seed tiles =
    let
        numTiles =
            List.length tiles

        generator =
            Random.int 0 (numTiles - 1)

        randomIndex =
            Tuple.first (Random.step generator seed)
    in
    ( List.append (List.take randomIndex tiles) (List.drop (randomIndex + 1) tiles)
    , Maybe.withDefault ( Black, One ) {- TODO bad! -} (getAt randomIndex tiles)
    )


{-| moves a tile from the first list in the tuple to the second list
-}
moveTile : Seed -> ( List Tile, List Tile ) -> ( List Tile, List Tile )
moveTile seed tileLists =
    let
        ( newLeft, tileTaken ) =
            takeRandomTile seed (Tuple.first tileLists)
    in
    ( newLeft, tileTaken :: Tuple.second tileLists )


takeTiles : Seed -> List Tile -> Int -> ( List Tile, List Tile )
takeTiles seed tiles numTiles =
    List.range 1 numTiles
        |> List.foldl (\_ tilesTuple -> moveTile seed tilesTuple) ( tiles, [] )


allColorsUnique : Group -> Bool
allColorsUnique group =
    let
        colorsInGroup =
            List.map getColor group
    in
    List.length (uniqueBy colorToInt colorsInGroup) == List.length colorsInGroup


allColorsTheSame : Group -> Bool
allColorsTheSame group =
    case Maybe.map getColor (getAt 0 group) of
        Nothing ->
            False

        Just firstColor ->
            List.all (\tile -> getColor tile == firstColor) group


allNumbersTheSame : Group -> Bool
allNumbersTheSame group =
    case Maybe.map getNumber (getAt 0 group) of
        Nothing ->
            False

        Just firstNumber ->
            List.all (\tile -> getNumber tile == firstNumber) group


allNumbersSequential : Group -> Bool
allNumbersSequential group =
    let
        sortedNumbers =
            List.map getNumber group
                |> List.map numberToInt
                |> List.sort
    in
    case List.head sortedNumbers of
        Nothing ->
            True

        Just firstNum ->
            case List.head (List.reverse sortedNumbers) of
                Nothing ->
                    True

                Just lastNum ->
                    lastNum - firstNum == List.length sortedNumbers - 1


isValidGroup : Group -> Bool
isValidGroup group =
    List.length group
        > 2
        && ((allColorsUnique group && allNumbersTheSame group)
                || (allColorsTheSame group && allNumbersSequential group)
           )


isValidBoard : Board -> Bool
isValidBoard board =
    List.all isValidGroup board


removeAt : Int -> List a -> List a
removeAt index list =
    splitAt index list
        |> (\( first, second ) ->
                List.concat [ first, defaultingToEmptyList (List.tail second) ]
           )


listDiff : List a -> List a -> ( List a, List a )
listDiff minuend subtrahend =
    List.foldl
        (\element ( leftList, rightList ) ->
            case elemIndex element leftList of
                Nothing ->
                    ( leftList, rightList )

                Just leftIndex ->
                    case elemIndex element rightList of
                        Nothing ->
                            -- TODO impossible state
                            ( leftList, rightList )

                        Just rightIndex ->
                            ( removeAt leftIndex leftList, removeAt rightIndex rightList )
        )
        ( minuend, subtrahend )
        subtrahend


containsAll : List a -> List a -> Bool
containsAll list1 list2 =
    let
        ( _, remaining ) =
            listDiff list1 list2
    in
    List.isEmpty remaining


replaceAt : Int -> a -> List a -> Maybe (List a)
replaceAt index element list =
    case index >= 0 && List.length list > index of
        False ->
            Nothing

        True ->
            let
                ( before, after ) =
                    splitAt index list
                        |> Tuple.mapSecond (\second -> defaultingToEmptyList (List.tail second))
            in
            Just <| List.concat [ before, [ element ], after ]


defaultingToEmptyList : Maybe (List a) -> List a
defaultingToEmptyList aListMaybe =
    Maybe.withDefault [] aListMaybe


numPlayers : GameState -> Int
numPlayers gameState =
    List.length gameState.playerHands


nextPlayerTurn : GameState -> Int
nextPlayerTurn gameState =
    modBy (numPlayers gameState) (gameState.playerTurn + 1)
