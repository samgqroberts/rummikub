module RummikubEngine.Utils exposing (allColorsTheSame, allColorsUnique, allNumbersSequential, allNumbersTheSame, allUniqueTiles, boardToString, colorToInt, colorToString, colors, containsAll, createTile, createTilesForColor, defaultGameConfig, defaultingToEmptyList, flattenGroups, generateAllTiles, getBoard, getColor, getHand, getHasPlayed, getNumPlayers, getNumber, getPlayerHands, getPlayerStates, getPlayerTurn, getPointValue, groupToString, isValidBoard, isValidGroup, listDiff, moveTile, nextPlayerTurn, numberToInt, numberToString, numbers, playerHandToString, removeAt, replaceAt, shuffleList, takeTiles, tileListToString, tileToString)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed, int, step)
import RummikubEngine.Models exposing (..)
import Tuple


flattenGroups : List Group -> List Tile
flattenGroups groups =
    groups
        |> List.concatMap (\a -> a)


getPlayerStates : GameState -> PlayerStates
getPlayerStates gameState =
    gameState.playerStates


getPlayerHands : GameState -> PlayerHands
getPlayerHands gameState =
    gameState.playerStates
        |> List.map .hand


getPlayerTurn : GameState -> Int
getPlayerTurn gameState =
    gameState.playerTurn


getHand : PlayerState -> PlayerHand
getHand playerState =
    playerState.hand


getHasPlayed : PlayerState -> Bool
getHasPlayed playerState =
    playerState.hasPlayed


getBoard : GameState -> Board
getBoard gameState =
    gameState.board


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


defaultGameConfig : GameConfig
defaultGameConfig =
    { numPlayers = 4
    , tileDuplicates = 2
    , startingPlayerTileCount = 14
    }


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


{-| moves the first tile from the first list in the tuple to the second list
-}
moveTile : ( List Tile, List Tile ) -> ( List Tile, List Tile )
moveTile ( source, target ) =
    case source of
        [] ->
            ( source, target )

        firstInSource :: restOfSource ->
            ( restOfSource, firstInSource :: target )


takeTiles : List Tile -> Int -> ( List Tile, List Tile )
takeTiles tiles numTiles =
    List.range 1 numTiles
        |> List.foldl (\_ tilesTuple -> moveTile tilesTuple) ( tiles, [] )


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


getNumPlayers : GameState -> Int
getNumPlayers gameState =
    List.length (getPlayerStates gameState)


nextPlayerTurn : GameState -> Int
nextPlayerTurn gameState =
    modBy (getNumPlayers gameState) (gameState.playerTurn + 1)


shuffleList : Seed -> List a -> List a
shuffleList seed list =
    shuffleListHelper seed list []


shuffleListHelper : Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                int 0 (List.length source - 1)

            ( index, nextSeed ) =
                step indexGenerator seed

            valAtIndex =
                getAt index source

            sourceWithoutIndex =
                removeAt index source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                -- TODO impossible state
                result


getTilePointValue : Tile -> Int
getTilePointValue ( _, number ) =
    numberToInt number


getGroupPointValue : Group -> Int
getGroupPointValue group =
    group
        |> List.map getTilePointValue
        |> List.sum


getPointValue : List Group -> Int
getPointValue groups =
    groups
        |> List.map getGroupPointValue
        |> List.sum
