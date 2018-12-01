module Game exposing (allColorsTheSame, allColorsUnique, allNumbersSequential, allNumbersTheSame, allTiles, attemptMove, colors, containsAll, createTilesForColor, defaultNumPlayers, isValidBoard, isValidGroup, listDiff, moveTile, newGame, numbers, replaceAt, startingPlayerTileCount, takeRandomTile, takeTiles, tileDuplicates)

import List
import List.Extra exposing (elemIndex, getAt, splitAt, uniqueBy)
import ModelUtils exposing (..)
import Models exposing (Board, Color(..), GameState, Group, Number(..), PlayerHand, Tile)
import Random exposing (Seed)
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


tileDuplicates =
    2


{-| how many (Black, One) tiles are there?
-}
defaultNumPlayers =
    4


createTilesForColor : Color -> List Tile
createTilesForColor color =
    numbers
        |> List.map (\number -> ( color, number ))


allTiles : List Tile
allTiles =
    colors
        |> List.map (\color -> [ color, color ])
        |> List.concat
        |> List.map createTilesForColor
        |> List.concat


startingPlayerTileCount =
    14


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


newGame : Seed -> GameState
newGame seed =
    let
        ( unflipped1, playerHand1 ) =
            takeTiles seed allTiles startingPlayerTileCount

        ( unflipped2, playerHand2 ) =
            takeTiles seed unflipped1 startingPlayerTileCount

        ( unflipped3, playerHand3 ) =
            takeTiles seed unflipped2 startingPlayerTileCount

        ( unflipped, playerHand4 ) =
            takeTiles seed unflipped3 startingPlayerTileCount
    in
    { unflipped = unflipped
    , playerHands = [ playerHand1, playerHand2, playerHand3, playerHand4 ]
    , board = []
    , playerTurn = 0
    }


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


attemptMove : GameState -> Board -> Result String GameState
attemptMove current newBoard =
    let
        currentPlayerHand =
            defaultingToEmptyList (getAt current.playerTurn current.playerHands)

        newBoardTiles =
            List.concatMap (\a -> a) newBoard

        currentBoardTiles =
            List.concatMap (\a -> a) current.board

        ( playedTiles, _ ) =
            listDiff newBoardTiles currentBoardTiles
    in
    case containsAll currentPlayerHand playedTiles of
        False ->
            Err "Some played tiles are not in the player's hand"

        True ->
            case isValidBoard newBoard of
                False ->
                    Err "Some played groups are not valid"

                True ->
                    let
                        ( newCurrentPlayerHand, _ ) =
                            listDiff currentPlayerHand playedTiles

                        newPlayerHands =
                            replaceAt current.playerTurn newCurrentPlayerHand current.playerHands
                                -- TODO impossible state
                                |> Maybe.withDefault current.playerHands
                    in
                    Ok
                        { current
                            | board = newBoard
                            , playerHands = newPlayerHands
                            , playerTurn = nextPlayerTurn current
                        }
