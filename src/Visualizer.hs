module Visualizer 
    ( renderSnapshots
    ) where

import Graph (Node, Edge(E), GraphMap, Path, Snapshot(..))
import GridMap ( Grid(..), Tile(..), Place
               , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors
               , setNeighbors, setTraversible, setElevation, setTile
               , gridFromList)
import Lens.Micro ((^.))
import Lens.Micro.TH
import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Table
import qualified Brick as B
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )  
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import qualified Brick.Types as B
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Brick.Widgets.Edit as E

-- | Attributes to show types of tiles
blockedAttr = attrName "blocked"
unseenAttr = attrName "unseen"
exploredAttr = attrName "explored"
selectedAttr = attrName "selected"

-- | A container for tiles with additional fields for rendering
data VisTile  = VisTile { tile :: Tile
                        , explored :: Bool
                        , selected :: Bool
                        } deriving (Show)

-- | Analog of Grid, but with VisTiles
data VisGrid = VisGrid { vRows :: Int
                       , vCols :: Int
                       , visTiles :: [VisTile]
                       } deriving (Show)

-- | Build a VisGrid without snapshot information from a Grid 
buildVisGrid :: Grid -> VisGrid
buildVisGrid g = VisGrid (rows g) (cols g) (map (\t -> VisTile t False False) (tiles g))

-- | Update the VisGrid with the information from a Snapshot
updateVisGrid :: VisGrid -> [Place] -> [Place] -> VisGrid
updateVisGrid vGrid explored selected = 
    let
        vTiles = visTiles vGrid
        vTiles' = map (\(t, i) -> 
            let
                t' = t { explored = elem i explored }
                t'' = t' { selected = elem i selected }
            in
                t'') (zip vTiles [0..])
    in
        vGrid { visTiles = vTiles' }

-- | Create a Widget for a VisTile with the correct attributes
drawVisTile :: VisTile -> Widget ()
drawVisTile (VisTile t explored selected) = case traversible t of
    False -> withAttr blockedAttr (str "X")
    True -> 
        let
            tileAttr = unseenAttr
            tileAttr' = if explored then exploredAttr else tileAttr
            tileAttr'' = if selected then selectedAttr else tileAttr'
        in
            withAttr tileAttr'' (str (show (elevation t)))

-- | Get a VisTile from a VisGrid
getVisTile :: VisGrid -> Int -> Int -> VisTile
getVisTile vGrid r c = 
    case (r >= 0 && r < vRows vGrid && c >= 0 && c < vCols vGrid) of
        True -> visTiles vGrid !! (r * vCols vGrid + c)
        False -> error "Invalid row or column"

-- | Create a Table from a VisGrid
visGridTable :: VisGrid -> Table ()
visGridTable vGrid = table $ 
  map (\r -> 
    map (\c ->  drawVisTile (getVisTile vGrid r c)) 
    [0..vCols vGrid- 1]) 
  [0..vRows vGrid - 1]

-- | Create a list of Widgets from a VisGrid
drawVisGrid :: VisGrid -> [Widget ()]
drawVisGrid vGrid = [C.center $ renderTable (visGridTable vGrid)]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (blockedAttr, V.black `on` V.black)
    , (unseenAttr, V.white `on` V.white)
    , (exploredAttr, V.white `on` V.blue)
    , (selectedAttr, V.white `on` V.red)
    ]


visualizerApp :: App VisGrid e ()
visualizerApp = App { appDraw = drawVisGrid
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent = \ev -> do
                        case ev of
                            VtyEvent (V.EvResize {}) -> return ()
                            VtyEvent (V.EvKey V.KEsc []) -> halt
                            VtyEvent (V.EvKey V.KEnter []) -> halt
                            _ -> return ()
                    , appStartEvent = return ()
                    , appAttrMap = const theMap
                    }

-- | Display a list of snapshots one at a time
renderSnapshots :: Grid -> [Snapshot] -> IO ()
renderSnapshots g (snapshot:snapshots) = do
    let
        vGrid = updateVisGrid (buildVisGrid g) (expNodes snapshot) (selNodes snapshot)
        buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
    initialVty <- buildVty
    customMain initialVty buildVty Nothing visualizerApp vGrid
    renderSnapshots g snapshots
renderSnapshots g [] = return ()