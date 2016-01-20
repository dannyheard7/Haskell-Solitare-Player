module EO where
    import System.Random
    import Data.List
    import Data.Maybe
    import Data.Function
    import Data.Ord
    import Debug.Trace

    -- deriving enum allows list ranges e.g. [Ace ..]
    data Suit = Hearts | Diamonds | Spades | Clubs deriving (Show, Eq)
    data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine
        | Ten | Jack | Queen | King  deriving (Show, Ord, Eq, Enum)

    type Card = (Pip, Suit)
    type Deck = [Card]

    -- Foundations and Columns are a stack, first item being the top
    type Foundations = [[Card]]
    type Columns = [[Card]]
    type Reserve = [Card]

    type EOBoard = (Foundations, Columns, Reserve)

    suits :: [Suit]
    suits = [Hearts, Diamonds, Spades, Clubs]

    pack :: Deck
    pack = [(x,y) | y <- suits,  x <- [Ace ..]] -- All cards of each suit

    -- Get successor card
    sCard :: Card -> Card
    sCard (pip, suit) = (succ pip, suit)

    -- Get predecessor card
    pCard :: Card -> Card
    pCard (pip, suit) = (pred pip, suit)

    isAce :: Card -> Bool
    isAce (pip, _) = pip == Ace

    isKing :: Card -> Bool
    isKing (pip, _) = pip == King

    -- Shuffle returns the full 52 card deck in a random order depending on the given seed
    shuffle :: Int -> Deck
    shuffle seed =
        let     randList = take 52 (randoms (mkStdGen seed) :: [Int])
                zipList = zip pack randList -- Zip the random numbers and the cards
                sortedList = sortBy (\(_, x) (_, y)-> compare x y) zipList -- Sort by the random numbers
        in map fst sortedList

    -- eoDeal creates 8 columns of 6 cards, does this by using split on the pack
    -- takes a seed for the random pack
    eODeal :: Int -> EOBoard
    eODeal seed = ([], distribute [] (snd split), fst split)
        where
            deck = if seed == 0 then pack else shuffle seed
            split = splitAt 4 deck
            -- distribute - fill up each column with six cards.
            distribute founds [] = founds
            distribute [] cards = distribute [fst split] (snd split)
                where split = splitAt 6 cards
            distribute (x:xs) cards = distribute (fst split:(x:xs)) (snd split)
                where split = splitAt 6 cards

    -- Takes a board and a card - moves the card from the top of the column
    -- or from the reserve to the top of the correct foundation
    makeFoundationMove :: EOBoard -> Card -> EOBoard
    makeFoundationMove (founds, cols, res) card@(pip,_)
        -- If the card is an ace, then just add it to foundations as it won't work with map
        | pip == Ace = ([card]:founds, filter (not.null) (foldl (updateCols card) [] cols), filter (/= card) res)
        | otherwise = (map (updateFounds card) founds, filter (not.null) (foldl (updateCols card) [] cols), filter (/= card) res)
        where
            -- updateCols removes the card from the head of the column
            updateCols card acc [] = acc
            updateCols card acc (x:xs)
                | card == x = xs:acc
                | otherwise = (x:xs):acc
            -- updateFounds puts the card on the top of the correct foundation
            updateFounds card (x:xs)
                | isKing x = x:xs
                | card == sCard x = card:x:xs
                | otherwise = x:xs

    -- Gets the top card from the columns and all the reserve cards
    getPlayableCards :: EOBoard -> [Card]
    getPlayableCards (_, cols, res) = foldl addHead [] cols ++ res
        where
            addHead acc [] = acc
            addHead acc (x:xs) = x:acc

    -- Gets the foundation successors by folding over the list and getting the
    -- induvidual successor.
    getFoundationSuccs :: Foundations -> [Card]
    getFoundationSuccs = foldl getFoundSucc []
        where
            -- getFoundSucc gets the successor for a foundation, has to handle a maybe
            getFoundSucc acc (y:ys)
                | isKing y = acc
                | otherwise = sCard y:acc

    -- The movableCards are the top cards that are either the successors
    -- to the foundations or aces
    getMovableCards :: EOBoard -> [Card]
    getMovableCards board@(f, c, r) =
        let playableCards = getPlayableCards board
            foundationSuccs = getFoundationSuccs f
        in filter (\x -> x `elem` foundationSuccs || isAce x) playableCards

    -- Makes all possible moves to the foundations. Works by intersectioning the
    -- playableCards with the foundation successors and moving those cards
    toFoundations :: EOBoard -> EOBoard
    toFoundations board@(founds, _, _)
        | null movableCards = board -- No movableCards so return the board
        -- Recursively call the function  with the board after making the possible moves
        | otherwise = toFoundations $ foldl makeFoundationMove board movableCards
        where
            movableCards = getMovableCards board

    -- Gets all possible (res & col) single card to column moves
    toColMoves :: EOBoard -> [EOBoard]
    toColMoves board = foldl (\acc x -> if moveBoard x /= board then moveBoard x:acc else acc) [] playableCards
        where
            -- heads of all the cols and the cards in the reserve
            playableCards = getPlayableCards board
            moveBoard = cardToCols board

    -- Takes an eoboard and a card and if possible moves the card to the head
    -- of the column which has its predecessor at the head, removes the card from
    -- its original position and returns the new board
    cardToCols :: EOBoard -> Card -> EOBoard
    cardToCols board@(f, c, r) card
        -- If there is less than 8 columns we can start a new column with a king
        | isKing card && length c < 8 = (f, [card]:foldl (getOtherCols card) [] cols, filter (/= card) r)
        | not (isKing card || null moveCol) = (f, (card:head moveCol):foldl (getOtherCols card) [] cols, filter (/= card) r)
        | otherwise = board -- Card cannot be moved to any column
        where
            cols = filter (not.null) c
            moveCol = filter (\(x:xs) -> sCard card == x) cols
            getOtherCols _ acc [] = acc
            getOtherCols card acc (x:xs)
                | x == card = xs:acc -- Remove moved card from the original column
                | isKing card || x /= sCard card = (x:xs):acc
                | otherwise = acc

    -- Gets all possible column head to reserve moves
    toResMoves :: EOBoard -> [EOBoard]
    toResMoves (f, c, r)
        | length r < 8 = foldl (\acc card -> (f, [if x == card then xs else x:xs | (x:xs) <- c], card:r):acc) [] (map head . filter (not.null) $ c)
        | otherwise = [] -- Moves can only happen if there is free space in the reserve

    -- All the possible moves from a board are res-to-col moves, col-to-col moves and col-to-res moves
    findMoves :: EOBoard -> [EOBoard]
    findMoves board = toColMoves board ++ toResMoves board

    -- Gets all possible single moves for a board after moves to foundations and chooses the best move
    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove (_, [], []) = Nothing
    chooseMove board
        | boardScore /= evaluateBoard (fst best) && boardScore /= snd bestNext = Just (toFoundations (fst best))
        | otherwise = Nothing -- Board score is not increasing so choose no move in order to stop useless recursion
        where
            boardScore = evaluateBoard board
            best = lookAhead 3 board -- Gets the best move from this position depending on board score in three moves
            bestNext = lookAhead 1 (fst best) -- Look ahead again to stop infinite loops (res-col-res-col..)

    -- Looks ahead to depth n and chooses the move which gives the best board at that depth
    -- The possible boards are evaluated at depth 1 and the best is returned
    lookAhead :: Int -> EOBoard -> (EOBoard, Int)
    lookAhead 1 board = getBestBoard board (map (\b -> (b, evaluateBoard b)) possBoards)
        where
            possBoards = findMoves.toFoundations $ board
    lookAhead depth board = getBestBoard board bestNextLvl
        where
            -- Recurse with all the possible baords from the current position
            bestNextLvl = foldl (\acc b -> lookAhead (depth-1) b:acc) [] (findMoves.toFoundations $ board)

    -- The best board is the board with the highest score
    getBestBoard :: EOBoard -> [(EOBoard, Int)] -> (EOBoard, Int)
    getBestBoard board boards
        | not (null boards) = maximumBy (comparing snd) boards -- Gets the board with the highest score
        | otherwise = (board, -100) -- If there are no moves then return the original board

    -- Takes a board and returns an integer representing how good the board is
    -- Better boards have higher numbers of cards in the foundations, lower number of cards in reserve
    -- Columns that are in order and boards that allow future moves to the foundations
    evaluateBoard :: EOBoard -> Int
    evaluateBoard board@(f, c, r) = let
        foundCount = sum (map length f)
        toFoundCount = length (getMovableCards board)
        inOrderCols = colsInOrder c
        resCount = length r
        -- This is the eval weighting, higher weights show more important aspects of the board
        in (foundCount*25) + (toFoundCount*35) + (inOrderCols*30) + (resCount*(-4))

    -- Counts how many of the cards in the columns are in order from the bottom up.
    -- A column is in order if every card is the predecessor of the one below it
    -- and the column is based with a king
    colsInOrder :: Columns -> Int
    colsInOrder = foldl (checkInOrder 0) 0
        where
            checkInOrder _ acc [] = acc
            checkInOrder count acc (x:xs)
                | isKing x && null xs = acc + count + 1
                | not (null xs || isKing x) && sCard x == head xs = checkInOrder (count+1) acc xs
                | otherwise = acc

    -- Given a starting board, this plays EO solitare using the chooseMove
    -- function untill there are no more moves, and returns a score of how
    -- many cards are in the foundations
    eOGame :: EOBoard -> Int
    eOGame b@(_, c, r)
        | null c && null r = 52
        | isNothing move =  52 - length r - sum (map length c) -- No available moves
        | otherwise = eOGame (fromJust move)
        where
            move = chooseMove b

    -- Given a starting seed, this plays 100 games using eOGame
    -- and eODeal with the starting seed, and computes how many wins
    -- and the average score for the 100 games
    eOExpt :: Int -> (Int, Float)
    eOExpt seed = let
        games = map (eOGame.eODeal) [seed..(seed+99)]
        wins = length (filter (== 52) games)
        averageScore = fromIntegral (sum games) / 100
        in (wins, averageScore)
