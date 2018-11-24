module Game exposing (..)

import Random exposing (Seed)
import List
import List.Extra exposing (getAt)
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
