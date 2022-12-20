module Graph
    ( Node, Edge(E), GraphMap, Path, Snapshot (..)
    ) where
import Data.Map (Map)
-- import Data.Stack (Stack)
import qualified Data.Map as Map
import Control.Monad.RWS.Lazy (RWS, runRWS, evalRWS, ask, tell, get, put, MonadReader, MonadWriter, MonadState)
import qualified Data.Text as T


type Node = Int
-- data Graph = G Vertices Edges deriving Show 
-- newtype Vertices = V [Node]
data Edge a = E {source::Node, dest:: Node, weight::a} deriving (Show, Eq)
-- type Edges = [Edge]
type GraphMap = Map Node [Edge Float]
type Path  = [Edge Float]



-- describe general heuristic 
class Heuristic a where
    euclid_distance  :: a -> a -> Float
    manhattan_distance :: a -> a -> Float


instance Heuristic Node where
    euclid_distance :: Node -> Node -> Float
    euclid_distance f g = sqrt ((getY g - getY f) * (getY g - getY f)  + (getX g - getX f) * (getX g - getX f))
    manhattan_distance :: Node -> Node -> Float
    manhattan_distance f g = abs (getY g - getY f) + abs (getX g - getX f)


-- Define traversible as things we can define an edge over
class Traversible a where 
    mkEdge :: (Num a, Ord a) => Node -> Node -> a -> Edge a


instance Traversible Float where
    mkEdge :: Node -> Node -> Float -> Edge Float
    mkEdge = E 

        

data SPConfig = SPConfig {
    graph :: GraphMap,
    startNodes :: [Node],
    endNode :: [Node],
    hDist :: [Float],
    heuristicFunction :: Node -> Node -> Float
} 

data Snapshot = Snapshot {
    -- Explored shortest
    expNodes :: [Node],
    -- Explored edges 
    expEdges :: Path,
    -- Shortest path
    spPath :: Path,
    -- Best estimation path length (A*)
    bestEstim :: Float,
    -- to depth
    maxDepth :: Float
} deriving (Show, Eq)


instance Semigroup Snapshot where
    (<>) a b = Snapshot {expNodes = expNodes a ++ expNodes b, expEdges = expEdges a ++ expEdges b, spPath = spPath a ++ spPath b, bestEstim = bestEstim a + bestEstim b, maxDepth = maxDepth a + maxDepth b}

instance Monoid Snapshot where 
    mempty = Snapshot {expNodes = [], expEdges = [], spPath = [], bestEstim = 0, maxDepth = 0}
    mappend a b = Snapshot {expNodes = expNodes a ++ expNodes b, expEdges = expEdges a ++ expEdges b, spPath = spPath a ++ spPath b, bestEstim = bestEstim a + bestEstim b, maxDepth = maxDepth a + maxDepth b}

class Monoid a => ExploreList a where
    push :: a -> Node -> a
    pop :: a -> Maybe (a, Node)
    isEmpty :: a -> Bool
    addFromTo :: a -> a -> a


instance ExploreList (Stack Node) where
    push = stackPush
    pop = stackPop 
    isEmpty = stackIsEmpty
    addFromTo = mappend


data GraphState = GraphST {
    visited :: [Node], 
    orderExplore :: Stack Node,
    shortestPath :: Path
} deriving (Show, Eq)

instance Semigroup GraphState where
    (<>) a b = GraphST {visited = visited a ++ visited b, orderExplore = addFromTo (orderExplore a)  (orderExplore b), shortestPath = shortestPath a ++ shortestPath b}

instance Monoid GraphState where 
    mempty = GraphST {visited = [], orderExplore = stackNew, shortestPath = []}
    mappend a b = GraphST {visited = visited a ++ visited b, orderExplore = addFromTo (orderExplore a)  (orderExplore b), shortestPath = shortestPath a ++ shortestPath b}

instance Eq (Stack Node) where
    (==) a b = case stackPeek a of 
        Nothing -> case stackPeek b of 
            Nothing -> True
            Just _ -> False
        Just a' -> case stackPeek b of 
            Nothing -> False
            Just b' -> a' == b' && a == b

-- reader shouldn't be start state, config file 
-- make rendering datatype 
newtype SPContext st = SP { context :: RWS SPConfig [Snapshot] GraphState st } deriving (Monad, 
                                                                        Functor, 
                                                                        Applicative, 
                                                                        MonadReader SPConfig,                 
                                                                        MonadWriter [Snapshot], 
                                                                        MonadState GraphState)


-- iterative deepening dfs
dfsStep ::  GraphMap -> GraphState -> SPContext ()
dfsStep graphMap graphState = do
    SPConfig {endNode = endNode} <- ask 
    GraphST {visited = visited, orderExplore = orderExplore} <- get
    let md = stackPop orderExplore
    case md of 
        Nothing -> return graphState
        Just (rest, node) -> do
            guard (node `notElem` endNode)
            let neighbors = graphMap Map.! node
            let cdls = filter (`notElem` visited) (map dest neighbors) 
            let visited' = visited ++ [node]
            let nextGraphState = GraphST {visited = visited',  orderExplore =  foldl push rest cdls }
            tell Snapshot {expNodes = cdls, expEdges = neighbors, spPath = [], bestEstim = 0, maxDepth = 0}
            put nextGraphState
            

-- | Iterative deepening depth first search

iterativeDFS :: GraphMap -> SPContext ()
iterativeDFS graphMap = undefined 



--do
--   SPConfig {startNode = startNode, iterativeDepth = depth} <- ask 
--   let visited = [startNode]


   





    


    



-- Use monad transformers with a writer monad, as hardwired state monad is hard to change
-- Store snapshots of intermediate states
-- Look into graph module for generating well formed graphs for quickcheck
-- {Can use refinement types using LiquidHaskell}
-- Look into visualization using Gloss
-- Write type class interface for top level operations (like reading and writing to json)
-- More flexible node type class for heuristic computation 

-- -- Use monad transformers with a writer monad, as hardwired state monad is hard to change
-- -- Store snapshots of intermediate states
-- -- Look into graph module for generating well formed graphs for quickcheck
-- -- {Can use refinement types using LiquidHaskell}
-- -- Look into visualization using Gloss
-- -- Write type class interface for top level operations (like reading and writing to json)
-- -- More flexible node type class for heuristic computation 

isCircular :: Path -> Bool
isCircular = traverse [] where 
    traverse visited [] =  False
    traverse visited (e:es) =  (source e `elem` visited) || traverse (source e :visited) es 



-- -- Unit Tests


        

-- Arbitrary Instances
