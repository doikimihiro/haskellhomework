import Data.List
import Control.Monad.Trans.State.Lazy
import Data.Char
import System.Environment
import Control.Monad.IO.Class
data Mark =User1|User2|Blank deriving Eq
instance Show Mark where
    show User1="O"
    show User2 ="X"
    show Blank ="-"

type Board =[Mark]

initialBoard::Board
initialBoard =take 9 $ repeat Blank


printBoard::Board->IO()
printBoard board=
    putStrLn $(show (board!!0))++(show(board!!1))++(show(board!!2))++"\n"++(show (board!!3))++(show(board!!4))++(show(board!!5))++"\n"++(show (board!!6))++(show(board!!7))++(show(board!!8))++"\n"





rows :: [[Int]]
rows = [[0,3,6],[1,4,7],[2,5,8],  --縦
        [0,1,2],[3,4,5],[6,7,8],  --横
        [0,4,8],[2,4,6]]          --斜め




main :: IO ()
main = do
    args <- getArgs
    let isFirstuser = 0 < length args && "f"==args!!0
    evalStateT (play isFirstuser) initialBoard >>= putStrLn







play ::Bool->StateT Board IO String
play isfirstuser=do
    usersTurn isfirstuser
    board <- get
    liftIO $ printBoard board
    case judge board of
        Nothing -> (play $ not isfirstuser)
        Just result -> return result 

judge :: Board -> Maybe String
judge board
    | wonBy User1 board                 = return "won by user1."
    | wonBy User2 board              = return "won by user2."
    | Nothing == elemIndex Blank board = return "drawn." --Blankが存在しない
    | otherwise                        = Nothing


wonBy :: Mark -> Board -> Bool
wonBy mark board = any threeInRow rows
  where
    threeInRow indices = all (mark==) $ rowStates indices
    rowStates indices  = map (board!!) indices




usersTurn ::Bool->StateT Board IO Int
usersTurn isfirstuser =do
    position <- liftIO$getDigit
    board <- get
    if Blank /= board!!position
        then (liftIO $ putStrLn "wrong position")>>usersTurn isfirstuser
        else case isfirstuser of
            True-> updateBoard User1 position >>return position
            False ->updateBoard User2 position >>return position
    where 
        
    getDigit = do 
        ch <- getChar
        putStrLn ""
        if elem ch ['0'..'8']
            then return (read [ch])
            else putStrLn "wrong input" >> getDigit


updateBoard ::Mark ->Int ->StateT Board IO ()
updateBoard mark pos =get >>=put.(update mark pos)


update :: Mark -> Int -> Board -> Board
update mark pos board =
    let (f, (x:xs)) = splitAt pos board in f ++ (mark:xs) --分割したあとxを書き換える





type Position =(Int,Int)

getInteger ::String ->Int->(Int,String)
getInteger [] x =(x,[])
getInteger (c:cs) x|isDigit c=getInteger cs (10*x+digitToInt c)
                   |otherwise =(x ,cs)

makePosition ::String ->Position
makePosition s1=(x,y)
         where 
            (x,s2)=getInteger s1 0
            (y ,s3)=getInteger s2 0

valid ::Position ->Bool
valid (x,y) =(0<=x&&x<=2)&&(0<=y&&y<=2)

getPosition :: IO Int 
getPosition = do --putStr "please input: "
                 str <-getLine
                 let (x,y) =makePosition str
                 validate (x,y)
    where validate(x,y)|valid (x,y) =return (x+y)
                       |otherwise =getPosition 






