module Graph
    ( Node, Edge(E), GraphMap, Path, 
    ) where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.RWS.Lazy (RWS, runRWS, evalRWS, ask, tell, get, put, MonadReader, MonadWriter, MonadState, guard, lift)
import qualified Data.Text as T
import  Data.Stack 
import Control.Applicative

-- place is just index




data Node = N {name:: String, getX :: Float, getY :: Float} deriving (Show, Eq, Ord)
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
    heuristicFunction :: Node -> Node -> Float,
    maxDepth  :: Float
}

data Snapshot = Snapshot {
    -- Explored shortest
    expNodes :: [Node],
    -- Explored edges 
    expEdges :: Path,
    -- Shortest path
    spPath :: Path,
    -- Best estimation path length (A*)
    bestEstim :: Float
    
} deriving (Show, Eq)


instance Semigroup Snapshot where
    (<>) a b = Snapshot {expNodes = expNodes a ++ expNodes b, expEdges = expEdges a ++ expEdges b, spPath = spPath a ++ spPath b, bestEstim = bestEstim a + bestEstim b}

instance Monoid Snapshot where 
    mempty = Snapshot {expNodes = [], expEdges = [], spPath = [], bestEstim = 0}

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

data Queue a  = Q [a] [a] deriving Show

instance Eq (Queue Node) where 
    (==) (Q xs ys) (Q xs' ys') = xs == xs' && ys == ys'

instance Semigroup (Queue Node) where
    (<>) = addFromTo
instance Monoid (Queue Node) where 
    mempty = Q [] []

instance ExploreList (Queue Node) where
    push (Q xs ys) el = Q xs (el:ys)
    pop (Q (x:xs) ys) = Just (Q xs ys, x)
    pop (Q [] []) = Nothing
    pop (Q [] ys) = pop (Q (reverse ys) [])
    isEmpty (Q [] []) = True
    isEmpty _ = False
    addFromTo a b = case pop b of 
        Nothing -> a
        Just (b', el) -> addFromTo (push a el) b'


data GraphState = GraphST {
    previous :: Map Node Node,
    distances :: Map Node Float, 
    orderExplore :: Queue Node
} deriving (Show, Eq)

instance Semigroup GraphState where
    (<>) a b = GraphST {previous = Map.union (previous a) (previous b), 
                        distances = Map.union (distances a) (distances b),
                        orderExplore = addFromTo (orderExplore a)  (orderExplore b)
                        }


instance Monoid GraphState where 
    mempty = GraphST {previous = Map.empty,  distances = Map.empty, orderExplore = mempty}


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
-- dfsStep ::  GraphMap -> SPContext ()
-- dfsStep graphMap  graphState = do
--     SPConfig {endNode = endNode} <- ask 
--     GraphST {visited = visited, orderExplore = orderExplore} <- get
--     let md = stackPop orderExplore
--     case md of 
--         Nothing -> return ()
--         Just (rest, node) -> do
--             if node `elem` endNode then return () else do
--                 let neighbors = graphMap Map.! node
--                 let cdls = filter (`notElem` visited) (map dest neighbors) 
--                 let visited' = visited ++ [node]
--                 let nextGraphState = GraphST {visited = visited',  orderExplore =  foldl push rest cdls, shortestPath = [] }
--                 tell [Snapshot {expNodes = cdls, expEdges = neighbors, spPath = [], bestEstim = 0, maxDepth = 0}]
--                 put nextGraphState
--                 dfsStep graphMap (maxDepth - 1) nextGraphState
            

-- -- | Iterative deepening depth first search

-- iterativeDFS ::  SPContext ()
-- iterativeDFS = do
--     SPConfig {graph = graph, startNodes = startNodes, maxDepth = maxDepth} <- ask 
--     let initGraphState = GraphST {visited = [], orderExplore = foldl push stackNew startNodes, shortestPath = []}
--     put initGraphState
--     counter <- newSTRef 1 
--     iterate 
--     where 
--         iterate = do 
--             depth <- readSTRef counter
--             dfsStep graph
--             writeSTRef counter (depth + 1)
        



dijkstraStep ::  GraphMap -> SPContext () 
dijkstraStep graph = do 
    SPConfig {endNode = endNode} <- ask 
    GraphST {distances = distances, previous = previous,  orderExplore = orderExplore} <- get
    let md = pop orderExplore
    case md of 
        Nothing -> return ()
        Just (rest, node) -> do
            if node `elem` endNode then return () else do
                let dist_u = distances Map.! node
                let neighbors = graph Map.! node
                -- adjust the distances depending on the shortest path 
                (dists', prev') <- f distances previous dist_u (zip (map dest neighbors) (map weight neighbors)) 
                put GraphST {distances = dists', previous = prev', orderExplore = rest}
                tell [Snapshot {expNodes = map dest neighbors, expEdges = neighbors, spPath = [], bestEstim = 0}]
                dijkstraStep graph
                where
                    f :: Map Node Float -> Map Node Node -> Float -> [(Node, Float)]-> SPContext (Map Node Float, Map Node Node)
                    f dists prev _ [] = return (dists, prev)
                    f dists prev dist_u ((n, w):neighbor_weights) = do 
                        if dist_u + w < (dists Map.! n)                                                    
                                    then do 
                                        let dists' = Map.adjust (const(dist_u + w)) n dists 
                                        let prev' = Map.adjust (const node) n prev
                                        f dists' prev' dist_u neighbor_weights else f dists prev dist_u neighbor_weights



dijkstra :: GraphMap -> SPContext ()
dijkstra graph = do    
    SPConfig {startNodes = startNodes} <- ask
    let allNodes = Map.keys graph
    let graphStateInit = GraphST {distances = Map.fromList (map (\x -> if x `elem` startNodes then (x,0) else (x,1000)) allNodes), 
                                previous = Map.fromList (zip allNodes allNodes),
                                orderExplore = foldl push (Q [] []) allNodes }
    put graphStateInit
    dijkstraStep graph
    



-- | Returns the sum of edge weights in a path
pathLength :: Path -> Float
pathLength = sum . map weight

isCircular :: Path -> Bool
isCircular = traverse [] where 
    traverse visited [] =  False
    traverse visited (e:es) =  (source e `elem` visited) || traverse (source e :visited) es 
