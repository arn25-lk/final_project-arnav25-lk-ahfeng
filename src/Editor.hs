{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor 
    ( 
      GridInfo(..)
    , editorApp
    , drawGrid
    , initEditor
    ) where

import GridMap ( Grid(..), Tile(..)
               , getTile, gridToGraph, adjacents, simpleGrid, getNeighbors
               , setNeighbors, setTraversible, setElevation, setTile
               , gridFromList)
import Graph (Snapshot(..))
import Visualizer (renderSnapshots)

import Control.Monad (forever, void)
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

data Name = ElevationField
          | TraversibleField
          | GridField
          | CurRowField
          | CurColField
          deriving (Eq, Ord, Show)
blockedAttr = attrName "blocked"
lowAttr = attrName "low"
mediumAttr = attrName "medium"
highAttr = attrName "high"
selectedAttr = attrName "selected"


data GridInfo = GridInfo
  { _elevationVal :: Float
  , _isTraversible :: Bool
  , _curRow :: Int
  , _curCol :: Int
  , _grid :: Grid
  }



makeLenses ''GridInfo

mkForm :: GridInfo -> Form GridInfo () Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in newForm [  label "Elevation" @@=
                   editShowableField elevationVal ElevationField
               , label "" @@=
                   checkboxField isTraversible TraversibleField "Traversible?"
               , label "Current Row" @@=
                   editShowableField curRow CurRowField
                , label "Current Column" @@=
                   editShowableField curCol CurColField
               
               ]


drawTile :: Tile -> Widget Name
drawTile t = case (traversible t, elevation t) of
    (True, elev) -> withAttr heightAttr $ (str . show) elev
    (False, _) -> withAttr blockedAttr $ str "X"
    where
        heightAttr = case elevation t of
            elev | elev < -5 -> lowAttr
                 | elev < 5 -> mediumAttr
                 | otherwise -> highAttr

gridTable :: Grid -> Table Name
gridTable grid = table $ 
  map (\r -> map (\c -> drawTile (getTile grid r c)) [0..cols grid - 1]) 
  [0..rows grid - 1]

drawGrid :: Grid -> Widget Name
drawGrid grid = C.center $ renderTable (gridTable grid)



draw :: Form GridInfo e Name -> [Widget Name]
draw f = 
  [C.vCenter $ C.hCenter form <=> C.hCenter gridVisual <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        gridVisual = B.border $ padTop (Pad 1) $ drawGrid (_grid (formState f))
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Press Tab to change field\n" <>
                     "- Set Row and Column to select tile to edit\n" <>
                     "- Row and Column must be valid integers (0-index)\n" <>
                     "  Press Space to toggle traversiblity\n" <>
                     "- Press Enter to update tile\n" <>
                     "- Press Esc to exit" 


theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  , (blockedAttr, V.white `on` V.black)
  , (lowAttr, V.white `on` V.red)
  , (mediumAttr, V.white `on` V.yellow)
  , (highAttr, V.white `on` V.green)
  ]

editorApp :: B.App (Form GridInfo e Name) e Name
editorApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = focusRingCursor formFocus
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = return ()
  , B.appAttrMap = const theMap
  }



handleEvent :: B.BrickEvent Name e -> EventM Name (Form GridInfo e Name) ()
handleEvent ev = do
    s <- B.get
    f <- B.gets formFocus
    case ev of
      VtyEvent (V.EvResize {}) -> return ()
      VtyEvent (V.EvKey V.KEsc []) -> halt
      VtyEvent (V.EvKey V.KEnter []) -> updateGrid
      _ -> do
          handleFormEvent ev
          st <- gets formState
          modify $ setFieldValid (st^.curRow >= 0 && st^.curRow < rows (st^.grid)) CurRowField
          modify $ setFieldValid (st^.curCol >= 0 && st^.curCol < cols (st^.grid)) CurColField
      where
          updateGrid = do
              s <- B.get
              if allFieldsValid s
                  then do
                    let f = formState s
                    let grid = _grid f
                    let r = _curRow f
                    let c = _curCol f
                    let tile = getTile grid r c
                    let newTile = tile {elevation = _elevationVal f, traversible = _isTraversible f}
                    let newGrid = setTile grid r c newTile
                    B.put s {formState = f {_grid = newGrid}}
                    return ()
                  else return ()




-- Placeholder for the actual snapshots  

testSnapshot1 :: Snapshot
testSnapshot1 = Snapshot 
  { expNodes = [0]
  , expEdges = []
  , selNodes = []
  , spPath = []
  , bestEstim = 0
  }

testSnapshot2 :: Snapshot
testSnapshot2 = Snapshot 
  { expNodes = [0,1,6]
  , expEdges = []
  , selNodes = [0]
  , spPath = []
  , bestEstim = 0
  }

testSnapshot3 :: Snapshot
testSnapshot3 = Snapshot 
  { expNodes = [0,1,6,2,7,11]
  , expEdges = []
  , selNodes = [0,1,6]
  , spPath = []
  , bestEstim = 0
  }

testSnapshots :: [Snapshot]
testSnapshots = [testSnapshot1, testSnapshot2, testSnapshot3]

initEditor :: Grid -> IO ()
initEditor grid = do
    chan <- newBChan 10
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    let app = editorApp
    let state = mkForm (GridInfo 0 True 0 0 grid)
    s' <- customMain initialVty buildVty (Just chan) app state 
    renderSnapshots (_grid (formState s')) testSnapshots
