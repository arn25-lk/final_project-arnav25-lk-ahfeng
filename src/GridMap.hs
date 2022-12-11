module GridMap 
  (Place, Tile, Grid
  , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors, setNeighbors
  -- , setTraversible, setElevation, setTile
  , gridTable
  , testTable, drawTable
  ) where

import Graph (Edge(E), GraphMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Table
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U


-- | Identifier for tiles in a grid
type Place = Int
-- | A map tile with an elevation and whether it is traversible
data Tile = Tile { place :: Place, elevation :: Float, traversible :: Bool, neighbors :: [Place] } deriving (Show, Eq)

data Grid = Grid {rows :: Int, cols :: Int, tiles :: [Tile]} deriving (Show, Eq)

-- | Get the tile at the given row and column in the grid
getTile :: Grid -> Int -> Int -> Tile -- row <= rows, col <= cols
getTile grid row col = tiles grid !! (row * cols grid + col)

-- | Create an edge between two tiles
edge :: Tile -> Tile -> Edge
edge t1 t2 = E (place t1) (place t2) (abs (elevation t1 - elevation t2))

-- | Turn a grid into a graph by iterating through the grid and creating edges between traversible neighbors with weight equal to the difference in elevation
gridToGraph :: Grid -> GraphMap
gridToGraph grid = foldl (\m t -> if traversible t then Map.insert (place t) (map (edge t) (filter traversible (map (\p -> getTile grid (p `div` cols grid) (p `mod` cols grid)) (neighbors t)))) m else m) Map.empty (tiles grid)

-- | Get the places of the tiles adjacent to the given tile
adjacents :: Int -> Int -> Place -> [Place]
adjacents rows cols place = filter (\p -> p >= 0 && p < rows * cols) [place - cols, place - 1, place + 1, place + cols]

-- | Create a grid of traversible tiles with 0 elevation with the given dimensions
simpleGrid :: Int -> Int -> Grid
simpleGrid rows cols = Grid rows cols (map (\p -> Tile p 0 True (adjacents rows cols p)) [0..rows * cols - 1])
        

-- | Get the new neighbors of a tile in the grid based on traversibility
getNeighbors :: Grid -> Int -> Int -> [Place]
getNeighbors grid row col = filter (\p -> traversible (getTile grid (p `div` cols grid) (p `mod` cols grid))) (adjacents (rows grid) (cols grid) (row * cols grid + col))

-- | Set the neighbors of a tile in the grid
setNeighbors :: Grid -> Int -> Int -> [Place] -> Grid
setNeighbors grid row col newNeighbors = grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {neighbors = newNeighbors} else t) (tiles grid)}

-- | Set the traversibility of a tile in the grid
setTraversible :: Grid -> Int -> Int -> Bool -> Grid
setTraversible grid row col newTraversible = grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {traversible = newTraversible} else t) (tiles grid)}

-- | Set the elevation of a tile in the grid
setElevation :: Grid -> Int -> Int -> Float -> Grid
setElevation grid row col newElevation = grid {tiles = map (\t -> if place t == place (getTile grid row col) then t {elevation = newElevation} else t) (tiles grid)}

-- | Set the tile at the given row and column in the grid
setTile :: Grid -> Int -> Int -> Tile -> Grid
setTile grid row col newTile = grid {tiles = map (\t -> if place t == place (getTile grid row col) then newTile else t) (tiles grid)}

testTable :: Table ()
testTable =
    surroundingBorder False $
    table [ [str "test", str "table"]
          , [str "is",    str "here"]
          ]

drawTable :: Table () -> Widget ()
drawTable table = C.center $ renderTable table

drawTile :: Tile -> String
drawTile tile = if traversible tile then show (elevation tile) else "X"

gridTable :: Grid -> Table ()
gridTable grid = table (map (\r -> map (\c -> str (drawTile (getTile grid r c))) [0..cols grid - 1]) [0..rows grid - 1])

drawGrid :: Grid -> Widget ()
drawGrid grid = C.center $ renderTable (gridTable grid)