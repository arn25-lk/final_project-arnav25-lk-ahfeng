module Lib
    ( someFunc
    ) where

<<<<<<< HEAD
someFunc :: String
someFunc = "Hello CIS 5520"


-- Given a graph (adjacency list) and path (list of nodes), return whether
-- the path is a valid path in the graph.
pathValid :: Graph -> Path -> Bool
pathValid g p = all (\(x,y) -> y `elem` (g ! x)) (zip p (tail p))

-- Checks whether a path is a shortest path by comparing with a known algorithm.
pathShortest :: Graph -> Node -> Node -> Path -> Bool


-- Unit Tests
-- Tests whether a path-finding algorithm returns a direct path from start
-- to end when available.
directPathValid :: PathFinder -> Graph -> Node -> Node -> Bool 
-- add edge from start to end, 

-- Tests whether a path-finding algorithm correctly returns Nothing when
-- there is no path from start to end.
testUnconnected :: PathFinder -> Graph -> Node -> Node -> Bool
=======
import Data.Map (Map)
import qualified Data.Map as Map

type Node = Int
-- data Graph = G Vertices Edges deriving Show
-- newtype Vertices = V [Node]
data Edge = E {source::Node, dest:: Node, weight::Int}
-- type Edges = [Edge]
type GraphMap = Map Node [Edge]
type Path = [Edge]

toAdjList :: Graph -> GraphMap

dijkstra :: GraphMap -> Node -> Map Node Path

bfs :: GraphMap -> Node -> Map Node Path

dfs :: GraphMap -> Node -> Map Node Path




>>>>>>> 61ed1d19b2aef82d6719fa7709e53c63f20efa9f
