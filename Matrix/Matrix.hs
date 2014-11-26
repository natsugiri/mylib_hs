type Ary a = [a]
type Mat a = [Ary a]

idMat :: Num a => Int -> Mat a
idMat n = map (\i -> replicate i 0 ++ [1] ++ replicate (n-1-i) 0) [0..n-1]

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

subMat :: Num a => Mat a -> Mat a -> Mat a
subMat = zipWith (zipWith (-))

mulMat :: Num a => Mat a -> Mat a -> Mat a
mulMat x y = map (\p -> map (sum .zipWith (*) p) (transpose y)) x

powMat :: (Num a, Integral b) => Mat a -> b -> Mat a
powMat x y
    | y == 0       = idMat (length x)
    | y`mod`2 == 0 = powMat (mulMat x x) (y`div`2)
    | otherwise    = mulMat x (powMat x (y-1))