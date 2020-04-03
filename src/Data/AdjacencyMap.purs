module Data.AdjacencyMap where

-- this is ported from the haskell library algebraic-graphs 

import MedeaPrelude
import Data.FoldableWithIndex as Fold
import Data.FunctorWithIndex as FunctorWI
import Data.Map as Map
import Data.Set as Set

-- helpers for underlying data structures
maybeToArray :: forall a. Maybe a -> Array a
maybeToArray ma = case ma of
  Nothing -> []
  Just a -> [a]

unionsWith :: forall f k v. Ord k => Foldable f => (v -> v -> v) -> f (Map k v) -> Map k v
unionsWith f = foldl (Map.unionWith f) Map.empty

mapFromSet :: forall k a. Ord k => (k -> a) -> Set k -> Map k a
mapFromSet f s = Map.fromFoldable $ zip keys values
  where
    keys = Set.toUnfoldable s
    values = map f $ keys

isSubmapOfBy :: forall a b k. Ord k => (a->b->Boolean) -> Map k a -> Map k b -> Boolean
isSubmapOfBy f m1 m2
  = Map.size m1 <= Map.size m2 && submap' f m1 m2
  
mapAdjust :: forall a k. Ord k => (a -> a) -> k -> Map k a -> Map k a
mapAdjust f k m = case Map.lookup k m of
  Nothing -> m
  Just a -> Map.insert k (f a) m

mapKeysWith :: forall a k1 k2. Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith c f = Map.fromFoldableWith c <<< Fold.foldrWithIndex (\k x xs -> cons (Tuple (f k) x) xs) []

catMaybeSet :: forall a. Ord a => Set (Maybe a) -> Set a
catMaybeSet sma = foldr foldfMaybeSet Set.empty sma

foldfMaybeSet :: forall a. Ord a => Maybe a -> Set a -> Set a
foldfMaybeSet Nothing acc = acc
foldfMaybeSet (Just a) acc = Set.insert a acc

foldfMaybeMap :: forall a k. Ord k => Maybe k -> a -> Map k a -> Map k a
foldfMaybeMap Nothing _ mka = mka
foldfMaybeMap (Just k) a mka = Map.insert k a mka

catMaybeMap :: forall k a. Ord k => Map (Maybe k) a -> Map k a
catMaybeMap mmka = Fold.foldrWithIndex foldfMaybeMap Map.empty mmka

mapFromArray :: forall a b. Ord a => Array (Tuple a b) -> Map a b
mapFromArray = Map.fromFoldable

-- Main Graph implementation

newtype AdjacencyMap a = AM (Map a (Set a))

derive instance newtypeAdjacencyMap :: Newtype (AdjacencyMap a) _

derive newtype instance eqAdjacencyMap :: Eq a => Eq (AdjacencyMap a)

-- TODO instances Ord, Show Num, semigroup, monoid NFData

empty :: forall a. AdjacencyMap a
empty = AM Map.empty 

vertex :: forall a. a -> AdjacencyMap a
vertex x = AM $ Map.singleton x Set.empty

edge :: forall a. Ord a => a -> a -> AdjacencyMap a
edge x y 
  | x == y = AM $ Map.singleton x (Set.singleton y)
  | otherwise =  AM $ Map.fromFoldable
    [ (Tuple x (Set.singleton y))
    , (Tuple y Set.empty)
    ]

overlay :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay (AM x) (AM y) = AM $ Map.unionWith Set.union x y

connect :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect (AM x) (AM y) = AM $ unionsWith Set.union $
  [x, y, mapFromSet (const $ Map.keys y) (Map.keys x) ]

vertices :: forall a. Ord a => Array a -> AdjacencyMap a
vertices = AM <<< Map.fromFoldable <<< map (\x -> Tuple x Set.empty)

edges :: forall a. Ord a => Array (Tuple a a) -> AdjacencyMap a
edges = fromAdjacencySets <<< map (map Set.singleton)

overlays :: forall a. Ord a => Array (AdjacencyMap a) -> AdjacencyMap a
overlays = AM <<< unionsWith Set.union <<< map unwrap

connects :: forall a. Ord a => Array (AdjacencyMap a) -> AdjacencyMap a
connects = foldr connect empty

submap' :: forall a b c. Ord a => (b -> c -> Boolean) -> Map a b -> Map a c -> Boolean
submap' f m1 m2
  | Map.isEmpty m1 = true
  | Map.isEmpty m2 = false
  | otherwise = allMatch
  where
    allMatch = length m1Values == length m2Lookups && (and $ f <$> m1Values <*> m2Lookups)
    m2Lookups = catMaybes $ map (\k -> Map.lookup k m2) $ m1Keys
    m1Values = catMaybes $ map (\k -> Map.lookup k m1) $ m1Keys
    m1Keys = Set.toUnfoldable $ Map.keys m1

isSubgraphOf :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> Boolean
isSubgraphOf (AM x) (AM y) = isSubmapOfBy Set.subset x y

isEmpty :: forall a. AdjacencyMap a -> Boolean
isEmpty = Map.isEmpty <<< unwrap

hasVertex :: forall a. Ord a => a -> AdjacencyMap a -> Boolean
hasVertex x = Map.member x <<< unwrap

hasEdge :: forall a. Ord a => a -> a -> AdjacencyMap a -> Boolean
hasEdge u v (AM m) = case Map.lookup u m of
  Nothing -> false
  Just vs -> Set.member v vs 

vertexCount :: forall a. AdjacencyMap a -> Int
vertexCount = Map.size <<< unwrap

edgeCount :: forall a. AdjacencyMap a -> Int
edgeCount = sum <<< map Set.size <<< unwrap

vertexArray :: forall a. AdjacencyMap a -> Array a
vertexArray = Set.toUnfoldable <<< Map.keys <<< unwrap

edgeArray :: forall a. Ord a => AdjacencyMap a -> Array (Tuple a a)
edgeArray (AM m) = do
  (Tuple x ys) <- sort $ Map.toUnfoldable m
  y <- sort $ Set.toUnfoldable ys
  pure (Tuple x y)

vertexSet :: forall a. AdjacencyMap a -> Set a
vertexSet = Map.keys <<< unwrap

edgeSet :: forall a. Eq a => Ord a => AdjacencyMap a -> Set (Tuple a a)
edgeSet = Set.fromFoldable <<< edgeArray

adjacencyArray :: forall a. Ord a => AdjacencyMap a -> Array (Tuple a (Array a))
adjacencyArray = map (map $ sort <<< Set.toUnfoldable) <<< sort <<< Map.toUnfoldable <<< unwrap

preSet :: forall a. Ord a => a -> AdjacencyMap a -> Set a
preSet x = Set.fromFoldable <<< map fst <<< filter p <<< Map.toUnfoldable <<< unwrap
  where
    p (Tuple _ set) = x `Set.member` set

findWithDefault :: forall a b. Ord b => a -> b ->  Map b a -> a
findWithDefault a b m = case Map.lookup b m of
  Nothing -> a
  Just x -> x

postSet :: forall a. Ord a => a -> AdjacencyMap a -> Set a
postSet x = findWithDefault Set.empty x <<< unwrap

path :: forall a. Ord a => Array a -> AdjacencyMap a
path xs
  | length xs <= 1 = case head $ map vertex xs of
    Nothing -> empty
    Just a -> a
  | otherwise = case tail xs of
    Nothing -> empty
    Just ys -> edges $ zip xs ys


circuit :: forall a. Ord a => Array a -> AdjacencyMap a
circuit xs 
  | null xs = empty
  | otherwise = path $ x <> ys <> x
  where
    x = maybeToArray $ head xs
    ys = maybe [] identity $ tail xs

clique :: forall a. Ord a => Array a -> AdjacencyMap a
clique = fromAdjacencySets <<< fst <<< go
  where 
    go :: Array a -> Tuple (Array (Tuple a (Set a))) (Set a)
    go as = case uncons as of
      Nothing -> Tuple [] Set.empty
      Just { head: x, tail: xs } -> let (Tuple res set) = go xs in (Tuple (cons (Tuple x set) (res)) (Set.insert x set)) 

biclique :: forall a. Ord a => Array a -> Array a -> AdjacencyMap a
biclique xs ys = AM $ mapFromSet adjacent (x `Set.union` y)
  where
    x = Set.fromFoldable xs
    y = Set.fromFoldable ys
    adjacent v = if v `Set.member` x then y else Set.empty

stars :: forall a. Ord a => Array (Tuple a (Array a)) -> AdjacencyMap a
stars = fromAdjacencySets <<< map (rmap Set.fromFoldable)

fromAdjacencySets :: forall a. Ord a => Array (Tuple a (Set a)) -> AdjacencyMap a
fromAdjacencySets ss = AM $ Map.unionWith Set.union vs es
  where
    vs = mapFromSet (const Set.empty) <<< Set.unions $ map snd ss
    es = Map.fromFoldableWith Set.union ss

-- TODO: implement `tree` and `forest`

removeVertex :: forall a. Ord a => a -> AdjacencyMap a  -> AdjacencyMap a
removeVertex x = AM <<< map (Set.delete x)<<< Map.delete x <<< unwrap

removeEdge :: forall a. Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
removeEdge x y = AM <<< mapAdjust (Set.delete y) x <<< unwrap

gmap :: forall a b. Ord a => Ord b => (a -> b) -> AdjacencyMap a -> AdjacencyMap b
gmap f = AM <<< map (Set.map f) <<< mapKeysWith Set.union f <<< unwrap

replaceVertex :: forall a. Ord a => a -> a -> AdjacencyMap a -> AdjacencyMap a
replaceVertex u v = gmap $ \w -> if w == u then v else w

mergeVertices :: forall a. Ord a => (a -> Boolean) -> a -> AdjacencyMap a -> AdjacencyMap a
mergeVertices p v = gmap $ \u -> if p u then v else u

transpose :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
transpose (AM m) = AM $ Fold.foldrWithIndex combine vs m
  where
    combine v es = Map.unionWith Set.union (mapFromSet (const $ Set.singleton v) es)
    vs = mapFromSet (const Set.empty) (Map.keys m)

induce :: forall a. Ord a => (a -> Boolean) -> AdjacencyMap a -> AdjacencyMap a
induce p = AM <<< map (Set.filter p) <<< Map.filterWithKey (\k _ -> p k) <<< unwrap
induceJust :: forall a. Ord a => AdjacencyMap (Maybe a) -> AdjacencyMap a
induceJust = AM <<< map catMaybeSet <<< catMaybeMap <<< unwrap

compose :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
compose x y = fromAdjacencySets arr
  where
    arr :: Array (Tuple a (Set a))
    arr = do
      v <- (Set.toUnfoldable vs :: Array a)
      let ys = postSet v y
      guard (Set.isEmpty ys)
      t <- Set.toUnfoldable (postSet v tx)
      pure (Tuple t ys)
    tx = transpose x
    vs = vertexSet x `Set.union` vertexSet y


box :: forall a b. Ord a => Ord b => AdjacencyMap a -> AdjacencyMap b -> AdjacencyMap (Tuple a b)
box (AM x) (AM y) = overlay (AM $ mapFromArray xs) (AM $ mapFromArray ys)
  where
    xs = do
      (Tuple a as) <- Map.toUnfoldable x
      b <- Set.toUnfoldable $ Map.keys y
      pure (Tuple (Tuple a b) (Set.map((flip Tuple) b) as))
    ys = do
      a <- Set.toUnfoldable (Map.keys x)
      (Tuple b bs) <- Map.toUnfoldable y
      pure (Tuple (Tuple a b) (Set.map(Tuple a) bs))

reflexiveClosure :: forall a. Ord a=> AdjacencyMap a -> AdjacencyMap a
reflexiveClosure (AM m) = AM $ FunctorWI.mapWithIndex (\k -> Set.insert k) m

symmetricClosure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
symmetricClosure m = overlay m $ transpose m

transitiveClosure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure old
  | old == genNew old = old
  | otherwise = transitiveClosure $ genNew old

-- this varies from the original implementation because guards close over where
-- statements in PS where the opposite is true in haskell
genNew old = overlay old (old `compose` old)

closure :: forall a. Ord a => AdjacencyMap a -> AdjacencyMap a
closure = reflexiveClosure <<< transitiveClosure

consistent :: forall a. Ord a => AdjacencyMap a -> Boolean
consistent (AM m) = referredToVertexSet m `Set.subset` Map.keys m

referredToVertexSet :: forall a. Ord a => Map a (Set a) -> Set a
referredToVertexSet m = Set.fromFoldable $ concat xs
  where
    xs = do
       (Tuple x ys) <- Map.toUnfoldable m
       y <- Set.toUnfoldable ys
       pure [x, y]

