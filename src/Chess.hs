module Chess where

import Control.Monad.State
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import System.IO
import Data.Maybe
import Data.List (intercalate)


import Board
import Moves

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

makeMove :: Game ()
makeMove = do
    (color, treeDepth) <- ask
    (board,history)<- lift $ get
    maybeNextState <- lift $ lift $ runReaderT (getNextState board) (color, treeDepth)
    case maybeNextState of
        Nothing -> do
            liftIO $ putStrLn "Nothing to do - I lost" >> hFlush stdout
            fail ("Game over")
        Just nextState -> do
            let nextMove = moveToACNString $ fst nextState
            let nextBoard = snd nextState
            let nextHistory = (readACN nextMove):history
            liftIO $ putStrLn nextMove >> hFlush stdout
            liftIO $ hPutStrLn stderr (showBoard nextBoard)
            lift $ put (nextBoard, nextHistory)
