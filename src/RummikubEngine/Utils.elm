module RummikubEngine.Utils exposing (allColorsTheSame, allColorsUnique, allNumbersSequential, allNumbersTheSame, allUniqueTiles, boardToString, cards, colorToInt, colorToString, colors, containsAllTiles, createCard, createCardsForColor, defaultGameConfig, defaultingToEmptyList, flattenGroups, flattenTileValueGroups, generateAllTiles, getBoard, getColor, getCurrentPlayerHand, getCurrentPlayerHasPlayed, getCurrentPlayerState, getHand, getHasPlayed, getInitialPlayPointValue, getNumPlayers, getNumber, getPlayerHands, getPlayerStates, getPlayerTurn, groupToString, isValidBoard, isValidGroup, moveTile, nextPlayerTurn, numberToInt, numberToString, numbers, playerHandToString, removeAt, replaceAt, shuffleList, sortTilesByColor, sortTilesByNumber, takeTiles, tileIsJoker, tileListDiff, tileListToString, tileToString)

import List
import List.Extra exposing (findIndex, getAt, splitAt, uniqueBy)
import Random exposing (Seed, int, step)
import RummikubEngine.Models exposing (..)
import Tuple


colorOrdinal : Color -> Int
colorOrdinal color =
    case color of
        Black ->
            1

        Red ->
            2

        Orange ->
            3

        Blue ->
            4


compareColor : Color -> Color -> Order
compareColor color1 color2 =
    compare (colorOrdinal color1) (colorOrdinal color2)


compareNumber : Number -> Number -> Order
compareNumber number1 number2 =
    compare (numberToInt number1) (numberToInt number2)


colorThenNumberComparison : Tile -> Tile -> Order
colorThenNumberComparison tile1 tile2 =
    case tile1 of
        Card tileValue1 ->
            case tile2 of
                Card tileValue2 ->
                    case compareColor (getColor tileValue1) (getColor tileValue2) of
                        LT ->
                            LT

                        EQ ->
                            compareNumber (getNumber tileValue1) (getNumber tileValue2)

                        GT ->
                            GT

                Joker tileValueMaybe ->
                    LT

        Joker _ ->
            case tile2 of
                Card tileValue ->
                    GT

                Joker tileValueMaybe ->
                    EQ


numberThenColorComparison : Tile -> Tile -> Order
numberThenColorComparison tile1 tile2 =
    case tile1 of
        Card tileValue1 ->
            case tile2 of
                Card tileValue2 ->
                    case compareNumber (getNumber tileValue1) (getNumber tileValue2) of
                        LT ->
                            LT

                        EQ ->
                            compareColor (getColor tileValue1) (getColor tileValue2)

                        GT ->
                            GT

                Joker tileValueMaybe ->
                    LT

        Joker _ ->
            case tile2 of
                Card tileValue ->
                    GT

                Joker tileValueMaybe ->
                    EQ


sortTilesByColor : List Tile -> List Tile
sortTilesByColor tileList =
    List.sortWith colorThenNumberComparison tileList


sortTilesByNumber : List Tile -> List Tile
sortTilesByNumber tileList =
    List.sortWith numberThenColorComparison tileList


getCurrentPlayerState : GameState -> PlayerState
getCurrentPlayerState gameState =
    -- TODO impossible state
    Maybe.withDefault { hand = [], hasPlayed = False } (getAt (getPlayerTurn gameState) (getPlayerStates gameState))


getCurrentPlayerHand : GameState -> PlayerHand
getCurrentPlayerHand gameState =
    getCurrentPlayerState gameState
        |> getHand


getCurrentPlayerHasPlayed : GameState -> Bool
getCurrentPlayerHasPlayed gameState =
    getCurrentPlayerState gameState
        |> getHasPlayed


tileIsJoker : Tile -> Bool
tileIsJoker tile =
    case tile of
        Card _ ->
            False

        Joker _ ->
            True


flattenGroups : List Group -> List Tile
flattenGroups groups =
    groups
        |> List.concatMap identity


flattenTileValueGroups : List TileValueGroup -> List TileValue
flattenTileValueGroups groups =
    groups
        |> List.concatMap identity


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
    , numJokers = 2
    }


getColor : TileValue -> Color
getColor ( color, _ ) =
    color


getNumber : TileValue -> Number
getNumber ( _, number ) =
    number


createCard : Color -> Number -> Tile
createCard color number =
    Card ( color, number )


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


tileValueToString : TileValue -> String
tileValueToString tileValue =
    let
        colorString =
            colorToString (getColor tileValue)

        numberString =
            numberToString (getNumber tileValue)
    in
    "(" ++ colorString ++ ", " ++ numberString ++ ")"


tileValueMaybeToString : Maybe TileValue -> String
tileValueMaybeToString tileValueMaybe =
    case tileValueMaybe of
        Nothing ->
            "(unassigned)"

        Just tileValue ->
            tileValueToString tileValue


tileToString : Tile -> String
tileToString tile =
    case tile of
        Card tileValue ->
            "(Card: " ++ tileValueToString tileValue ++ ")"

        Joker tileValueMaybe ->
            "(Joker: " ++ tileValueMaybeToString tileValueMaybe ++ ")"


boardToString : Board -> String
boardToString board =
    "[" ++ String.join ", " (List.map groupToString board) ++ "]"


playerHandToString : PlayerHand -> String
playerHandToString playerHand =
    tileListToString playerHand


createCardsForColor : Color -> List Tile
createCardsForColor color =
    numbers
        |> List.map (\number -> Card ( color, number ))


generateAllTiles : Int -> Int -> List Tile
generateAllTiles tileDuplicates numJokers =
    allUniqueTiles
        |> List.concatMap (\uniqueTile -> List.repeat tileDuplicates uniqueTile)
        |> List.append (List.repeat numJokers (Joker Nothing))


allUniqueTiles : List Tile
allUniqueTiles =
    colors
        |> List.map createCardsForColor
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


allColorsUnique : TileValueGroup -> Bool
allColorsUnique group =
    let
        colorsInGroup =
            List.map getColor group
    in
    List.length (uniqueBy colorToInt colorsInGroup) == List.length colorsInGroup


allColorsTheSame : TileValueGroup -> Bool
allColorsTheSame group =
    case Maybe.map getColor (getAt 0 group) of
        Nothing ->
            False

        Just firstColor ->
            List.all (\tile -> getColor tile == firstColor) group


allNumbersTheSame : TileValueGroup -> Bool
allNumbersTheSame group =
    case Maybe.map getNumber (getAt 0 group) of
        Nothing ->
            False

        Just firstNumber ->
            List.all (\tile -> getNumber tile == firstNumber) group


allNumbersSequential : TileValueGroup -> Bool
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


toTileValue : Tile -> Maybe TileValue
toTileValue tile =
    case tile of
        Card tileValue ->
            Just tileValue

        Joker tileValueMaybe ->
            tileValueMaybe


toTileValueGroup : Group -> Maybe TileValueGroup
toTileValueGroup group =
    let
        maybeTileValueGroup =
            List.map toTileValue group
    in
    case List.member Nothing maybeTileValueGroup of
        True ->
            Nothing

        False ->
            maybeTileValueGroup
                |> List.filterMap identity
                |> Just


isValidTileValueGroup : TileValueGroup -> Bool
isValidTileValueGroup tileValueGroup =
    List.length tileValueGroup
        > 2
        && ((allColorsUnique tileValueGroup && allNumbersTheSame tileValueGroup)
                || (allColorsTheSame tileValueGroup && allNumbersSequential tileValueGroup)
           )


isValidGroup : Group -> Bool
isValidGroup group =
    case toTileValueGroup group of
        Nothing ->
            False

        Just tileValueGroup ->
            isValidTileValueGroup tileValueGroup


isValidBoard : Board -> Bool
isValidBoard board =
    List.all isValidGroup board


removeAt : Int -> List a -> List a
removeAt index list =
    splitAt index list
        |> (\( first, second ) ->
                List.concat [ first, defaultingToEmptyList (List.tail second) ]
           )



-- note: considers jokers of any variety to be equal


tileEquals : Tile -> (Tile -> Bool)
tileEquals test =
    \subject ->
        case subject of
            Card subjectTileValue ->
                case test of
                    Card testTileValue ->
                        subjectTileValue == testTileValue

                    Joker _ ->
                        False

            Joker _ ->
                case test of
                    Card tileValue ->
                        False

                    Joker tileValueMaybe ->
                        True



-- note: considers jokers of any variety to be equal


tileListElemIndex : Tile -> List Tile -> Maybe Int
tileListElemIndex test =
    findIndex (tileEquals test)



-- note: considers jokers of any variety to be equal


tileListDiff : List Tile -> List Tile -> ( List Tile, List Tile )
tileListDiff minuend subtrahend =
    List.foldl
        (\element ( leftList, rightList ) ->
            case tileListElemIndex element leftList of
                Nothing ->
                    ( leftList, rightList )

                Just leftIndex ->
                    case tileListElemIndex element rightList of
                        Nothing ->
                            -- TODO impossible state
                            ( leftList, rightList )

                        Just rightIndex ->
                            ( removeAt leftIndex leftList, removeAt rightIndex rightList )
        )
        ( minuend, subtrahend )
        subtrahend



-- note: considers jokers of any variety to be equal


containsAllTiles : List Tile -> List Tile -> Bool
containsAllTiles list1 list2 =
    let
        ( _, remaining ) =
            tileListDiff list1 list2
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


getTileValuePointValue : TileValue -> Int
getTileValuePointValue tileValue =
    getNumber tileValue
        |> numberToInt


getInitialPlayGroupPointValue : Group -> Maybe Int
getInitialPlayGroupPointValue group =
    case toTileValueGroup group of
        Nothing ->
            Nothing

        Just tvgroup ->
            case isValidTileValueGroup tvgroup of
                False ->
                    Nothing

                True ->
                    tvgroup
                        |> List.map getTileValuePointValue
                        |> List.sum
                        |> Just


getInitialPlayPointValue : List Group -> Maybe Int
getInitialPlayPointValue groups =
    let
        listOfGroupPointValueMaybes =
            groups
                |> List.map getInitialPlayGroupPointValue
    in
    case List.member Nothing listOfGroupPointValueMaybes of
        True ->
            Nothing

        False ->
            listOfGroupPointValueMaybes
                |> List.filterMap identity
                |> List.sum
                |> Just


cards : List TileValue -> List Tile
cards tileValueGroupList =
    tileValueGroupList
        |> List.map Card
