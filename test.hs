
f :: Int -> Maybe Bool
f n
    | n > 0     = Just (n == 2)
    | otherwise = Nothing

isFine = do
    t1 <- f 1
    t2 <- f (-3)
    t3 <- Nothing
    return (t1 || t2)
