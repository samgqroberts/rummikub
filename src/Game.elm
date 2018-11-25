module Game exposing (..)

import Random exposing (Seed)
import List
import List.Extra exposing (getAt, uniqueBy)
import Tuple
import Models exposing (Tile, Color(..), Number(..), Group, PlayerHand, GameState)

createTile : Color -> Number -> Tile
createTile color number = (color, number)

colors = [Black, Red, Orange, Blue]
numbers = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
           Eleven, Twelve, Thirteen]
tileDuplicates = 2 {- how many (Black, One) tiles are there? -}
numPlayers = 4

createTilesForColor : Color -> List Tile
createTilesForColor color =
  numbers
    |> List.map (\number -> (color, number))

allTiles : List Tile
allTiles = colors
  |> List.map (\color -> [color, color])
  |> List.concat
  |> List.map createTilesForColor
  |> List.concat
startingPlayerTileCount = 14

takeRandomTile : Seed -> List Tile -> (List Tile, Tile)
takeRandomTile seed tiles =
  let
    numTiles = List.length tiles
    generator = Random.int 0 (numTiles - 1)
    randomIndex = Tuple.first (Random.step generator seed)
  in
    ( List.append (List.take randomIndex tiles) (List.drop (randomIndex + 1) tiles)
    , Maybe.withDefault (Black, One) {-TODO bad!-} (getAt randomIndex tiles)
    )

-- moves a tile from the first list in the tuple to the second list
moveTile : Seed -> (List Tile, List Tile) -> (List Tile, List Tile)
moveTile seed tileLists =
  let
    (newLeft, tileTaken) = takeRandomTile seed (Tuple.first tileLists)
  in
    (newLeft, tileTaken :: (Tuple.second tileLists))

takeTiles : Seed -> List Tile -> Int -> (List Tile, List Tile)
takeTiles seed tiles numTiles =
  List.range 1 numTiles
    |> List.foldl (\_ tilesTuple -> moveTile seed tilesTuple) (tiles, [])

newGame : Seed -> GameState
newGame seed =
  let
    (unflipped1, playerHand1) = takeTiles seed allTiles startingPlayerTileCount
    (unflipped2, playerHand2) = takeTiles seed unflipped1 startingPlayerTileCount
    (unflipped3, playerHand3) = takeTiles seed unflipped2 startingPlayerTileCount
    (unflipped, playerHand4) = takeTiles seed unflipped3 startingPlayerTileCount
  in
    { unflipped = unflipped
    , playerHands = [playerHand1, playerHand2, playerHand3, playerHand4]
    , played = []
    }

getColor : Tile -> Color
getColor tile =
  Tuple.first tile

getNumber : Tile -> Number
getNumber tile =
  Tuple.second tile

colorOrdinal : Color -> Int
colorOrdinal color =
  case color of
    Black -> 0
    Red -> 1
    Orange -> 2
    Blue -> 3

numberOrdinal : Number -> Int
numberOrdinal number =
  case number of
    One -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Eleven -> 11
    Twelve -> 12
    Thirteen -> 13

allColorsUnique : Group -> Bool
allColorsUnique group =
  let
    colorsInGroup = List.map getColor group
  in
    List.length (uniqueBy colorOrdinal colorsInGroup) == List.length colorsInGroup

allColorsTheSame : Group -> Bool
allColorsTheSame group =
  case Maybe.map getColor (getAt 0 group) of
    Nothing -> False
    Just firstColor ->
      List.all (\tile -> getColor tile == firstColor) group

allNumbersTheSame : Group -> Bool
allNumbersTheSame group =
  case Maybe.map getNumber (getAt 0 group) of
    Nothing -> False
    Just firstNumber ->
      List.all (\tile -> getNumber tile == firstNumber) group

allNumbersSequential : Group -> Bool
allNumbersSequential group =
  let
    sortedNumbers = List.map getNumber group
      |> List.map numberOrdinal
      |> List.sort
  in
    case List.head sortedNumbers of
      Nothing -> True
      Just firstNum ->
        case List.head (List.reverse sortedNumbers) of
          Nothing -> True
          Just lastNum ->
            lastNum - firstNum == List.length sortedNumbers - 1

isValidGroup : Group -> Bool
isValidGroup group =
  List.length group > 2 && (
    (allColorsUnique group && allNumbersTheSame group)
    || (allColorsTheSame group && allNumbersSequential group)
  )
