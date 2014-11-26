-- Skew Heap
-- push : O(log n) amortize
-- pop : pop max, O(log n) amortize

data Heap a 
    = EmptyHeap
    | NodeHeap a (Heap a) (Heap a)
    deriving Show

emptyHeap :: Ord a => Heap a
emptyHeap = EmptyHeap

singletonHeap :: Ord a => a -> Heap a
singletonHeap a = NodeHeap a EmptyHeap EmptyHeap

unionHeap :: Ord a => Heap a -> Heap a -> Heap a
EmptyHeap `unionHeap` h2 = h2
h1 `unionHeap` EmptyHeap = h1
h1@(NodeHeap a1 l1 r1) `unionHeap` h2@(NodeHeap a2 l2 r2)
    | a1 <= a2 = NodeHeap a2 (h1 `unionHeap` r2) l2
    | otherwise = NodeHeap a1 (h2 `unionHeap` r1) l1

pushHeap :: Ord a => a -> Heap a -> Heap a
pushHeap a h = singletonHeap a `unionHeap` h

-- pop max
popHeap :: Ord a => Heap a -> (a, Heap a)
popHeap (NodeHeap a l r) = (a, l `unionHeap` r)

