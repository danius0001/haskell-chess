import System.Environment
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Text.ParserCombinators.Parsec
import System.IO

import Chess
import Board
import Moves

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

-- Główna funkcja programu
main :: IO ()
main = do
    let treeDepth = 4
    args <- getArgs
    let initialBoard = readBoard startBoard
    case (listToMaybe args) of
        Just "w" -> do -- białe zaczynają
            (nextBoard, nextHistory) <- evalStateT (runReaderT (do{makeMove;lift get}) (White,treeDepth)) (initialBoard,[])
            go White treeDepth nextBoard nextHistory
        _ -> 
            go Black treeDepth initialBoard [] -- domyślnie grają czarne
