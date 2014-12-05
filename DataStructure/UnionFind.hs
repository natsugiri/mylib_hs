-- Disjoint Set
-- link, same : O(log n a(n)) ?

type UF = Seq.Seq Int

initUF :: Int -> UF
initUF n = Seq.replicate n (-1)

rootUF :: Int -> UF -> (Int, UF)
rootUF x uf
    | p < 0 = (x, uf)
    | otherwise = (\(q, uf1) -> (q, Seq.update x q uf1)) $ rootUF p uf
    where p = Seq.index uf x

linkUF :: Int -> Int -> UF -> UF
linkUF x y uf
    | p == q = uf2
    | Seq.index uf2 p > Seq.index uf2 q = f q p
    | otherwise = f p q
    where (p, uf1) = rootUF x uf
          (q, uf2) = rootUF y uf1
          f p q = Seq.update q p $ Seq.adjust (+ Seq.index uf2 p) q uf2
              
sameUF :: Int -> Int -> UF -> (Bool, UF)
sameUF x y uf = (p == q, uf2)
    where (p, uf1) = rootUF x uf
          (q, uf2) = rootUF y uf1

countUF :: Int -> UF -> (Int, UF)
countUF x uf = (- Seq.index uf1 p, uf1)
    where (p, uf1) = rootUF x uf
