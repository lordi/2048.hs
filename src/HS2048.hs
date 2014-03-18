{-# LANGUAGE OverloadedStrings #-}

-- | Quick crack at a minimal 2048 in haskell

module HS2048 where

import Control.Monad.Random
import Control.Monad.Writer
import Data.List

import qualified Data.Text as T

import System.Console.Haskeline

type Row   a = [a]
type Board a = [Row a]
data Direction = East | West | North | South deriving (Show, Eq)

data MoveOutcome = Lose | Win | Active | Invalid
data RoundResult a = RoundResult a MoveOutcome (Board a)

showBoard :: (Monoid a, Show a) => Board a -> String
showBoard = T.unpack . T.unlines . fmap formatRow
    where formatRow = T.intercalate "|" . fmap (T.center 20 ' ' . formatCell)
          formatCell x = T.pack $ show x

shiftRow :: (Monoid a, Eq a) => Row a -> Writer a (Row a)
shiftRow row = liftM (++ nothings) $ sumPairs justs
    where (justs, nothings) = partition isNotEmpty row
          sumPairs (x:y:zs) | x == y = do
            let total = x `mappend` y
            tell total
            rest <- sumPairs zs
            return $ total : (rest ++ [mempty])
          sumPairs (x:xs) = liftM (x :) $ sumPairs xs
          sumPairs [] = return []
          isNotEmpty = (/= mempty)

shiftBoard :: (Monoid a, Eq a) => Direction -> Board a -> (Board a, a)
shiftBoard direction = runWriter . case direction of
    West  -> goWest
    East  -> goEast
    North -> liftM transpose . goWest . transpose
    South -> liftM transpose . goEast . transpose
    where goWest = mapM shiftRow
          goEast = mapM $ liftM reverse . shiftRow . reverse

emptyBoard :: (Monoid a) => Int -> Board a
emptyBoard n = replicate n $ replicate n mempty

-- | coords of available spaces
available :: (Eq a, Monoid a) => Board a -> [(Int, Int)]
available = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices mempty)

--  ew
update :: Board a -> (Int, Int) -> a -> Board a
update board (x, y) val = newBoard
    where (rs, r:rs') = splitAt x board
          (cs, _:cs') = splitAt y r
          newRow = cs <> (val : cs')
          newBoard = rs <> (newRow : rs')

insertRandom :: (Monoid a, MonadRandom m, Eq a) => Board a -> a -> m (Maybe (Board a))
insertRandom board initial
    | null holes = return Nothing
    | otherwise = do
        pos <- liftM (holes !!) $ getRandomR (0, length holes - 1)
        coin <- getRandomR (0 :: Float, 1)
        let newCell = if coin < 0.9 then initial else (initial `mappend` initial)
        return . Just $ update board pos newCell
    where holes = available board

winner :: (Monoid a, Eq a) => a -> Board a -> Bool
winner winning = elem winning . concat

gameRound :: (MonadRandom m, Monoid a, Eq a) => a -> a -> Direction -> Board a -> m (RoundResult a)
gameRound initial goal direction board =
    let (newBoard, newPoints) =
            shiftBoard direction board
        result = RoundResult newPoints
        change = board /= newBoard
    in if not change 
        then return $ if null $ available newBoard
            then result Lose newBoard
            else result Invalid board
        else if winner goal newBoard
            then return $ result Win newBoard
            else do
                randoBoard <- insertRandom newBoard initial
                case randoBoard of
                    Nothing -> return $ result Lose newBoard
                    Just b  -> return $ result Active b

runGame :: (Monoid a, Show a, Eq a) => a -> a -> Board a -> a -> InputT IO ()
runGame initial goal board score = do
    liftIO . putStrLn $ showBoard board
    input <- getInputChar "wasd: "
    liftIO $ putStrLn ""

    let direction = case input of
         Just 'w' -> Just North
         Just 'a' -> Just West
         Just 's' -> Just South
         Just 'd' -> Just East
         _ -> Nothing

    case direction of
        Nothing ->
            runGame initial goal board score
        Just dir -> do
            RoundResult newPoints moveOutcome newBoard <-
                liftIO $ gameRound initial goal dir board
            let totalScore = newPoints `mappend` score
            case moveOutcome of
                Lose -> liftIO $ 
                    putStrLn $ "You lose with " ++ show totalScore ++ " points."
                Win -> liftIO $ 
                    putStrLn $ "You win with " ++ show totalScore ++ " points!"
                Active -> do
                    liftIO $ do
                        putStrLn $ "You earned " ++ show newPoints ++ " points."
                        putStrLn $ "Total score is " ++ show totalScore ++ " points."
                    runGame initial goal newBoard totalScore
                Invalid -> do
                    liftIO $ putStrLn "Invalid move, try again."
                    runGame initial goal newBoard totalScore

makeStartBoard :: (MonadRandom m, Monoid a, Eq a) => Int -> a -> m (Board a)
makeStartBoard size initial = do
    Just board  <- insertRandom (emptyBoard size) initial
    Just board' <- insertRandom board initial
    return board'

main :: IO ()
main = do
    let size = 4
        goal = Sum 1024
        initial = Sum 2

    startBoard <- makeStartBoard size initial
    putStrLn "Use 'w', 'a', 's', and 'd' to move."
    runInputT defaultSettings $ runGame initial goal startBoard mempty

