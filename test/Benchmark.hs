import System.Environment
import Control.Monad.State
import Control.Monad.Reader
import System.Random
import Data.Maybe
import Text.Read

import Chess
import Board
import Moves

main :: IO ()
main = do
    let treeDepth = 4
    let initialBoard = readBoard startBoard
    args <- getArgs
    let numMoves = fromMaybe 10 . fmap read $ listToMaybe args
    setStdGen $ mkStdGen 1337
    
    evalStateT (runReaderT (replicateM_ numMoves makeMove) (White,treeDepth)) (initialBoard,[])
