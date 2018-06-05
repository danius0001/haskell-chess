import System.Environment
import Control.Monad.State
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import System.IO
import Data.Maybe
import System.Random
import Data.List (intercalate)


import Chess.Board
import Chess.Moves

-- Parser do parsowania liter a-h
parsePosC :: Parser Char
parsePosC = oneOf "abcdefgh"

-- Parser do parsowania cyfr 1-8
parsePosN :: Parser Char
parsePosN = oneOf "12345678"

-- Typ ruchu w formacie ACN
newtype ACN = ACN (Char,Char,Char,Char)

-- Pokazanie ruchu
instance Show ACN where
  show (ACN (a,b,c,d)) = a:b:c:d:[]
  
-- Wczytuje ACN ze stringa
readACN::String -> ACN
readACN string = ACN(string!!0,string!!1,string!!2,string!!3)

-- Parsowanie ruchu
parseACN :: Parser ACN
parseACN = do
          x1 <- parsePosC
          y1 <- parsePosN
          x2 <- parsePosC
          y2 <- parsePosN
          return $ ACN (x1,y1,x2,y2)

-- Stan gry - stanem jest lista ruchów
type History = [ACN]
type Game a = ReaderT GameConfig (StateT (Board,History) IO) a


-- Wyswielanie historii gry na standarsowe wyjscie diagnostyczne
printHistory :: Show a => [a] -> IO ()
printHistory h =  do
    let history = intercalate ", " (map show h)
    hPutStrLn stderr $ "Game history = " ++ history

-- Funkcja rozgrywki
play :: String -> Game ()
play i = do
    (color, treeDepth) <- ask
    (board,history)<- lift $ get
    case parse parseACN "Parsing ACN error" i of
        Right acn -> do
            liftIO $ hPutStrLn stderr $ "Actual move = " ++ (show acn)
            lift $ put ((doMove (acnStringToMove (show acn)) board) , acn:history)
        Left _ -> fail ("Game over")
    (_, history2) <- lift $ get
    liftIO $ printHistory history2
    makeMove

-- Funkcja rozpoczynająca rozgrywkę
doPlay :: Game ()
doPlay = liftIO getContents >>= (mapM_ play) . lines

go :: Color -> Int -> Board -> History -> IO ()
go color treeDepth board history = evalStateT (runReaderT doPlay (color,treeDepth)) (board,history)

makeMove :: Game ()
makeMove = do
    (color, treeDepth) <- ask
    (board,history)<- lift $ get
    maybeNextState <- lift $ lift $ runReaderT (getNextState board) (color, treeDepth)
    case maybeNextState of
        Nothing -> do
            liftIO $ putStrLn "Nothing to do - I lost"
            liftIO $ hFlush stdout
            fail ("Game over")
        Just nextState -> do
            let nextMove = moveToACNString $ fst nextState
            let nextBoard = snd nextState
            let nextHistory = (readACN nextMove):history
            liftIO $ putStrLn nextMove
            liftIO $ hFlush stdout
            liftIO $ hPutStrLn stderr (showBoard nextBoard)
            lift $ put (nextBoard, nextHistory)

-- Główna funkcja programu
main :: IO ()
main = do
    let treeDepth = 4
    args <- getArgs
    let initialBoard = (readBoard startBoard)
    case (listToMaybe args) of
        Just "w" -> do -- białe zaczynają
            (nextBoard, nextHistory) <- evalStateT (runReaderT (do{makeMove;lift get}) (White,treeDepth)) (initialBoard,[])
            go White treeDepth nextBoard nextHistory
        _ -> 
            go Black treeDepth initialBoard [] -- domyślnie grają czarne
