type Qu = Seq.Seq
    
emptyQu = Seq.empty

pushQu :: a -> Qu a -> Qu a
pushQu a qu = a <| qu

popQu :: Qu a -> (a, Qu a)
popQu qu =
    let (qu1 :> a) = viewr qu
    in (a, qu1)
