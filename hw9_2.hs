{-# LANGUAGE BangPatterns, Safe #-}
{-# Options_GHC -O2 #-}
--import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Int
import Control.Monad
import Control.Monad.ST
import Data.Array.ST.Safe
import Data.Array
import qualified Data.Array.Unboxed as UA
import Data.List
import Text.Printf


data Mark = User|Machine|Blank deriving Eq
instance Show Mark where
  show User    = "U"
  show Machine = "M"
  show Blank   = " "

type Board = [Mark]

initialBoard :: Board
initialBoard = take 9 $ repeat Blank

printBoard :: Board -> IO ()
printBoard board =
    putStrLn $ unlines $ LU.join ["-+-+-"] $ map showRow [0..2]
  where
    showRow row = [LU.join "|" $ map show $ cells row]
    cells   row = take 3 $ snd $ splitAt (row * 3) board


rows :: [[Int]]
rows = [[0,3,6],[1,4,7],[2,5,8],
        [0,1,2],[3,4,5],[6,7,8],
        [0,4,8],[2,4,6]]

main :: IO ()
main = do
    args <- getArgs
    let machineFirst = 0 < length args && "m"==args!!0
    evalStateT (play machineFirst) initialBoard >>= putStrLn

play :: Bool -> StateT Board IO String
play isMachinesTurn = do
    if isMachinesTurn then machinesTurn
                      else usersTurn
    board <- get
    liftIO $ printBoard board
    case judge board of
        Nothing     -> (play $ not isMachinesTurn)
        Just result -> return result

judge :: Board -> Maybe String
judge board
    | wonBy User board                 = return "won by user."
    | wonBy Machine board              = return "won by machine."
    | Nothing == elemIndex Blank board = return "drawn."
    | otherwise                        = Nothing

wonBy :: Mark -> Board -> Bool
wonBy mark board = any threeInRow rows
  where
    threeInRow indices = all (mark==) $ rowStates indices
    rowStates indices  = map (board!!) indices

usersTurn :: StateT Board IO Int
usersTurn = do
    position <- liftIO getDigit
    board    <- get
    if Blank /= board!!position
        then (liftIO $putStrLn "wrong position") >> usersTurn
        else updateBoard User position >> return position
  where
    getDigit = do 
        ch <- getChar
        putStrLn ""
        if elem ch ['0'..'8']
            then return (read [ch])
            else putStrLn "wrong input" >> getDigit



updateBoard :: Mark -> Int -> StateT Board IO ()
updateBoard mark pos = get >>= put.(update mark pos)

update :: Mark -> Int -> Board -> Board
update mark pos board =
    let (f, (x:xs)) = splitAt pos board in f ++ (mark:xs)


machinesTurn :: StateT Board IO Int
machinesTurn = do
    board <- get
    let idx = fromJust $ elemIndex Blank board
    updateBoard Machine idx
    return idx