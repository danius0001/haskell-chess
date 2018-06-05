import System.Environment
import Control.Monad.State
import Text.ParserCombinators.Parsec
import System.IO
import Data.Maybe


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

-- Stan gey - stanem jest lista ruchów
type Game a = StateT (Color,Board,[ACN]) IO a

-- Wyswielanie historii gry na standarsowe wyjscie diagnostyczne
printHistory :: Show a => [a] -> IO ()
printHistory h =  do
  hPutStrLn stderr "Game history"
  mapM_ (hPutStrLn stderr.show) h

-- Funkcja rozgrywki
play :: String -> Game ()
play i = do
  (color,board,history)<- get
  case parse parseACN "Parsing ACN error" i of
    Right acn -> (liftIO $ hPutStrLn stderr $ "actual move = " ++ (show acn)) >> put (color, (doMove (acnStringToMove (show acn)) board) , acn:history)
    Left _ -> fail ("koniec")
  (color2,board2,history2) <- get
  liftIO $ printHistory history2
  let nextState = getNextStateBoard color board2
  let board3 = snd nextState
  let moveACN = moveToACNString $ fst nextState
  put (color, board3, ((readACN moveACN):history2))
  liftIO $ putStrLn moveACN >> hFlush stdout
  liftIO $ hPutStrLn stderr $ showBoard board3

-- Funkcja rozpoczynająca rozgrywkę
doPlay :: Game ()
doPlay = liftIO getContents >>= (mapM_ play) . lines

-- Główna funkcja programu
main :: IO ()
main = do
    args <- getArgs
    case (listToMaybe args) of
        Just "w" -> putStrLn (nextMove) >> hFlush stdout >> hPutStrLn stderr (showBoard nextBoard) >> go White nextBoard [readACN(nextMove)] -- białe najpierw wykonuja ruch
        _ -> go Black (readBoard startBoard) [] -- domyślnie grają czarne
        where go color board moves = evalStateT doPlay (color,board,moves)
              nextState = getNextStateBoard White (readBoard startBoard)
              nextMove = moveToACNString(fst(nextState))
              nextBoard = snd nextState
