module MeanStats where
    import EO

    tenGames :: Int -> (Int, (Float, Float, Float))
    tenGames seed = (wins, stats)
        where
            games = map (eOGame.eODeal) [seed..(seed+999)]
            stats = meanStats games
            wins = length (filter (== 52) games)

    -- mean, variance, sd of a list of Ints
    meanStats :: [Int]->(Float,Float,Float)
    meanStats lis = (mean,var,sqrt var)
        where
            rlis = map fromIntegral lis
            rlen = fromIntegral (length lis)
            mean = sum rlis / rlen
            var = sum (map (\n -> (n-mean)**2) rlis) / rlen
