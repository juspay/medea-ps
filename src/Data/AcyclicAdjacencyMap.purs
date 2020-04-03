module Data.AcyclicAdjacencyMap where

import MedeaPrelude
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Cont.Class (class MonadCont, callCC)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.State.Class (class MonadState, gets, modify)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.AdjacencyMap (AdjacencyMap)
import Data.AdjacencyMap as AdjMap
import Data.Map as Map
import Data.NonEmpty (NonEmpty(..),  (:|))
import Data.Set as Set
import Data.Tree (Tree, Forest)
import Data.Tree as Tree
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

newtype AcyclicAdjacencyMap a = AAM (AdjacencyMap a)

data AcyclicError = CycleError | DFSOrderViolation


-- TODO : instance (Ord a, Show a) => Show (AcyclicAdjacencyMap a)

empty :: forall a. AcyclicAdjacencyMap a
empty = unsafeCoerce AdjMap.empty

vertex :: forall a. AcyclicAdjacencyMap a
vertex = unsafeCoerce AdjMap.vertex

vertices :: forall a. Ord a => Array a -> AcyclicAdjacencyMap a
vertices arr = unsafeCoerce $ AdjMap.vertices arr

union :: forall a b. Ord a => Ord b => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap b -> AcyclicAdjacencyMap (Either a b)
union (AAM x) (AAM y) = AAM $ AdjMap.overlay (AdjMap.gmap Left x) (AdjMap.gmap Right y)

join :: forall a b. Ord a => Ord b => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap b -> AcyclicAdjacencyMap (Either a b)
join (AAM a) (AAM b) = AAM $ AdjMap.connect (AdjMap.gmap Left a) (AdjMap.gmap Right b)

-- coercion doesn't work here
isSubgraphOf :: forall a. Ord a => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a -> Boolean
isSubgraphOf (AAM a) (AAM b) = AdjMap.isSubgraphOf a b

isEmpty :: forall a. AcyclicAdjacencyMap a -> Boolean
isEmpty = unsafeCoerce AdjMap.isEmpty

hasVertex :: forall a. Ord a => a -> AcyclicAdjacencyMap a -> Boolean
hasVertex a aama = unsafeCoerce $ AdjMap.hasVertex a (unsafeCoerce aama)

hasEdge :: forall a. Ord a => a -> a -> AcyclicAdjacencyMap a -> Boolean
hasEdge a b aama = AdjMap.hasEdge a b (unsafeCoerce aama)

vertexCount :: forall a. AcyclicAdjacencyMap a -> Int
vertexCount = unsafeCoerce AdjMap.vertexCount

edgeCount :: forall a. AcyclicAdjacencyMap a -> Int
edgeCount = unsafeCoerce AdjMap.edgeCount

vertexArray :: forall a. AcyclicAdjacencyMap a -> Array a
vertexArray = unsafeCoerce AdjMap.vertexArray

edgeArray :: forall a. Ord a => AcyclicAdjacencyMap a -> Array (Tuple a a)
edgeArray aama = AdjMap.edgeArray (unsafeCoerce aama)

adjacencyArray :: forall a. Ord a => AcyclicAdjacencyMap a -> Array (Tuple a (Array a))
adjacencyArray a = AdjMap.adjacencyArray $ unsafeCoerce a

vertexSet :: forall a. Ord a => AcyclicAdjacencyMap a -> Set a
vertexSet = unsafeCoerce AdjMap.vertexSet

edgeSet :: forall a. Eq a => Ord a => AcyclicAdjacencyMap a -> Set (Tuple a a)
edgeSet a = AdjMap.edgeSet $ unsafeCoerce a

preSet :: forall a. Ord a => a -> AcyclicAdjacencyMap a -> Set a
preSet a aama = AdjMap.preSet a $ unsafeCoerce aama

postSet :: forall a. Ord a => a -> AcyclicAdjacencyMap a -> Set a
postSet a aama = AdjMap.postSet a $ unsafeCoerce aama 

removeVertex :: forall a. Ord a => a -> AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a
removeVertex a aama = AAM $ AdjMap.removeVertex a $ unsafeCoerce aama

removeEdge :: forall a. Ord a => a -> a -> AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a
removeEdge a b aama = AAM $ AdjMap.removeEdge a b $ unsafeCoerce aama

transpose :: forall a. Ord a => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a
transpose a = AAM $ AdjMap.transpose $ unsafeCoerce a

induce :: forall a. Ord a => (a -> Boolean) -> AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a
induce f a = AAM $ AdjMap.induce f $ unsafeCoerce a

induceJust :: forall a. Ord a => AcyclicAdjacencyMap (Maybe a) -> AcyclicAdjacencyMap a
induceJust a = AAM $ AdjMap.induceJust $ unsafeCoerce a

box :: forall a b. Ord a => Ord b => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap b -> AcyclicAdjacencyMap (Tuple a b)
box a b = AAM $ AdjMap.box (unsafeCoerce a) (unsafeCoerce b)

transitiveClosure :: forall a. Ord a => AcyclicAdjacencyMap a -> AcyclicAdjacencyMap a
transitiveClosure a = AAM $ AdjMap.transitiveClosure $ unsafeCoerce a

data NodeState = Entered | Exited

type TSS a 
  = { parent :: Map a a 
    , entry :: Map a NodeState
    , order :: Array a
    }

emptyTSS :: forall a. TSS a
emptyTSS = { parent: Map.empty, entry: Map.empty, order: [] }

-- ToUnfoldable gives no garuntee of ordering in the docs
mapToDescendingArray :: forall a k. Ord k => Ord a => Map k a -> Array (Tuple k a)
mapToDescendingArray mkay = sortBy compare' $ Map.toUnfoldable mkay

setToDescendingArray :: forall a. Ord a => Set a -> Array a
setToDescendingArray sa = sortBy compare' $ Set.toUnfoldable sa

compare' :: forall a. Ord a => a -> a -> Ordering
compare' a b = case compare a b of
  GT -> LT
  EQ -> EQ
  LT -> GT

-- we point to the cycle head because the cycle array approach used in haskell creates an infinite type 
data CycleHead a = CycleHead (Map a a)

topSort' :: forall a m. Ord a => Partial => MonadState (TSS a) m => MonadCont m => AdjacencyMap a  -> m (Either (CycleHead a) (Array a))
topSort' g = callCC $ 
  \cyclic -> do
    let 
      vertices' = map fst $ mapToDescendingArray $ unwrap g
      adjacent = setToDescendingArray <<< flip AdjMap.postSet g
      -- in haskell, dfsRoot and dfs `return ()` on the just cases where we pure empty TSS here. there may be implications if this is used for more than cycle detection
      dfsRoot x = nodeState x >>= case _ of
          Nothing -> enterRoot x *> dfs x *> exit x
          _       -> pure emptyTSS
      dfs x = for_ (adjacent x) $ \y ->
          nodeState y >>= case _ of
            Nothing -> enter x y *> dfs y *> exit y
            Just Exited -> pure emptyTSS
            Just Entered -> cyclic <<< Left <<< CycleHead =<< gets _.parent
    for_ vertices' dfsRoot
    Right <$> gets _.order
  where
    nodeState v = gets (Map.lookup v <<< _.entry)
    enter u v = modify (\({ parent, entry, order }) -> { parent: Map.insert v u parent, entry: Map.insert v Entered entry, order })
    enterRoot v = modify (\({ parent, entry, order }) -> {parent, entry: Map.insert v Entered entry, order})
    exit v = modify (\({ parent, entry, order }) -> { parent, entry: Map.alter (map leave) v entry, order: cons v order })
    leave = case _ of
        Entered -> Exited
        Exited -> unsafeThrow "Internal error: dfs search order violated"

topSort :: forall a. Ord a => AdjacencyMap a -> Either (CycleHead a) (Array a)
topSort g = unsafePartial $ runContT (evalStateT (topSort' g) emptyTSS) identity 


isAcyclic :: forall a. Ord a => AdjacencyMap a -> Boolean
isAcyclic = isRight <<< topSort

toAcyclic :: forall a. Ord a => AdjacencyMap a -> Maybe (AcyclicAdjacencyMap a)
toAcyclic x = if isAcyclic x then Just (AAM x) else Nothing


