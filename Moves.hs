module Chess.Moves where

import Data.Maybe
import Data.Tree
import Data.Char (intToDigit,digitToInt,ord,chr)
import Chess.Board
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Reader

type Move = (Pos,Pos) -- ruch z pozycji pierwszej na drugą
type GameState = (Move,Board) --Ruch jaki został wykonany i plansza w rezultacie tego ruchu
type GameConfig = (Color, Int) -- (mój kolor, tree depth)


--Pusty ruch
emptyMove :: Move
emptyMove = ((0,0),(0,0))

-- Konwersja z ruchu na stringa w formacie ACN
moveToACNString :: Move -> String
moveToACNString move = (intToCharPos fstCol) : (intToDigit (8-fstRow)) : (intToCharPos sndCol) : (intToDigit (8-sndRow)) : []
        where intToCharPos i = chr ( i + (ord 'a')) 
              fstCol = snd (fst move)
              fstRow = fst (fst move)
              sndCol = snd (snd move)
              sndRow = fst (snd move)
        
-- Wczytuje ruch ze stringa w formacie ACN      
acnStringToMove:: String -> Move
acnStringToMove acn = ( ( 8-(digitToInt fstRow) , charPosToInt fstCol) , ( 8-(digitToInt sndRow) , charPosToInt sndCol) )
        where charPosToInt d = (ord d) - (ord 'a')
              fstCol = acn !! 0
              fstRow = acn !! 1
              sndCol = acn !! 2
              sndRow = acn !! 3

-- Dodaje wiektory
plusVector :: (Int,Int) -> (Int,Int) -> (Int,Int)
plusVector (a,b) (c,d)=(a+c,b+d)

-- Mnoży wektor przez skalar
mulVector :: Int -> (Int,Int) -> (Int,Int)
mulVector x (a,b) = (a*x,b*x)

-- Zmina jednej wartosci na liscie
updateList :: a -> Int -> [a] -> [a]
updateList _ _ [] = []
updateList element n (h:t)
            | n==0 = element:t
            | n>0 = h : (updateList element (n-1) t)


-- Ustawienie figury na dana pozycje
updateBoard :: Field -> Pos -> Board -> Board
updateBoard fig (x,y) board = updateList (updateList fig y row) x board
        where row = (board !! x)

-- Wstawia Nothing na wskazana pozycje
deleteFigure:: Pos -> Board -> Board    
deleteFigure pos board = updateBoard Nothing pos board


-- Przesuwa figure z jednej pozycji na druga
moveFigure :: Pos -> Pos -> Board -> Board
moveFigure pos1 pos2 board 
        | pos1 == pos2 = board
        | otherwise = deleteFigure pos1 board2
            where board2 = updateBoard figure pos2 board
                  figure = takeField pos1 board
            
-- Wykonuje ruch na planszy
doMove :: Move -> Board -> Board
doMove move board = moveFigure (fst move) (snd move) board

-- Sprawdza czy pole jest puste
emptyField :: Pos -> Board -> Bool
emptyField pos board = 
        case (takeField pos board) of
            Nothing -> True
            _ -> False

-- Sprawdza czy pole jest zajete przez przeciwnika
enemyField:: Pos-> Color -> Board -> Bool
enemyField pos Black board = 
        case (takeField pos board) of
            Just(Figure _ White) -> True
            _ -> False
enemyField pos White board = 
        case (takeField pos board) of
            Just(Figure _ Black) -> True
            _ -> False

-- Sprawdza czy pole jest puste lub zajete przez przeciwnika        
emptyOrEnemyField:: Pos-> Color -> Board -> Bool
emptyOrEnemyField pos color board = emptyField pos board || enemyField pos color board

-- Zwraca liste dostepnych ruchów przez figurę na danej pozycji
availableMoves :: Pos -> Board -> [Pos]
availableMoves pos board = availableFigureMoves (takeField pos board) pos board 

-- Zwraca liste dostepnych ruchów przez figurę która stoi na danej pozycji
availableFigureMoves :: Field -> Pos -> Board -> [Pos]
availableFigureMoves Nothing _ _ = []
availableFigureMoves (Just(Figure P color)) pos board = 
            -- ruch o jedno pole
           [ret | w<-(snglMvPawn color), let ret = (plusVector pos w),  inBounds ret, emptyField ret board]
            -- ruch o dwa pola
        ++ [ret | w<-(dblMvPawn color), fst pos == (dblMvRowPawn color), let ret = (plusVector pos w),  inBounds ret, emptyField (plusVector pos (head(snglMvPawn color))) board, emptyField ret board] 
            -- ruch atakujący
        ++ [ret | w<-(attackPawn color), let ret = (plusVector pos w),  inBounds ret, enemyField ret color board]
availableFigureMoves (Just(Figure N color)) pos board = filter (\newPos-> emptyOrEnemyField newPos color board) (movesGenerator pos movesKnight [1])
availableFigureMoves (Just(Figure K color)) pos board = filter (\newPos-> emptyOrEnemyField newPos color board) (movesGenerator pos movesKing [1])
availableFigureMoves (Just(Figure R color)) pos board = availableBishOrRookMoves R color pos board
availableFigureMoves (Just(Figure B color)) pos board = availableBishOrRookMoves B color pos board
availableFigureMoves (Just(Figure Q color)) pos board = (availableFigureMoves (Just(Figure B color)) pos board) ++ (availableFigureMoves (Just(Figure R color)) pos board)

-- Generacja ruchów dla Wieży i Gońca
availableBishOrRookMoves :: Type -> Color -> Pos -> Board -> [Pos]
availableBishOrRookMoves fig color pos board = concat [movesInDirection n | n<-[0..3]] -- generacja ruchów w 4 strony
            where movesInDirection n = enemy ++ empty
                        where empty = takeWhile (\newPos-> emptyField newPos board) (movesGenerator pos ([(movesBishOrRook fig) !! n]) [1..7])
                              enemy = [ret | let ret = plusVector (last list) ((movesBishOrRook fig) !! n), inBounds ret,  enemyField ret color board]
                                    where list = pos:empty

-- Zwraca liste wektorów ruchu dla Wieży lub Gońca -> mają bardzo podobna generacje
movesBishOrRook :: Type -> [Pos]
movesBishOrRook B = movesBishop
movesBishOrRook R = movesRook

-- Generator ruchów. Zapewnia ze generowane ruchy nie wyjda poza plansze
movesGenerator :: Pos -> [Pos] -> [Int] -> [(Int,Int)]
movesGenerator (x,y) vec mult = [ret | w<-vec, m<-mult, let ret = (plusVector (x,y) (mulVector m w)),  inBounds ret]

-- Sprawdza czy podana pozycja miesci sie w planszy
inBounds :: Pos -> Bool
inBounds (x,y) = x>=0 && x<8 && y>=0 && y<8

-- Możliwe wektory ruchów wieży
movesRook :: [(Int,Int)]
movesRook = [(1,0),(0,1),(-1,0),(0,-1)]

-- Możliwe wektory ruchów gońca
movesBishop :: [(Int,Int)]
movesBishop = [(1,1),(1,-1),(-1,1),(-1,-1)]

-- Możliwe wektory ruchów króla
movesKing :: [(Int,Int)]
movesKing = movesRook ++ movesBishop

-- Możliwe wektory ruchów skoczka
movesKnight :: [(Int,Int)]
movesKnight = [(-2,-1),(-2,1),(-1,2),(-1,-2),(2,1),(2,-1),(1,-2),(1,2)]

-- Możliwe wektory ruchów (o jedno pole) pionka
snglMvPawn :: Color -> [(Int,Int)]
snglMvPawn Black = [(1,0)]
snglMvPawn White = [(-1,0)]

-- Możliwe wektory ruchów (o dwa pola) pionka
dblMvPawn :: Color -> [(Int,Int)]
dblMvPawn Black = [(2,0)]
dblMvPawn White = [(-2,0)]

-- Możliwe wektory ruchów (atakujących) pionka
attackPawn :: Color -> [(Int,Int)]
attackPawn Black = [(1,1),(1,-1)]
attackPawn White = [(-1,-1),(-1,1)]

-- Warunek kiedy pionek moze wykonac dwa ruchy (nr wiersza)
dblMvRowPawn :: Color -> Int
dblMvRowPawn Black = 1
dblMvRowPawn White = 6



-- Zwraca liste mozliwych ruchow kazdej figury gracza
genAllMoves:: Color -> Board -> [(Pos,[Pos])]
genAllMoves color board = filter notNullMoves (zip allFigures figureMoves)
        where notNullMoves x = not(null(snd x))
              allFigures = getFiguresPosByColor color board
              figureMoves = map (\x-> availableMoves x board) allFigures

-- Generuje wszytskie możliwe plansze dla podanego koloru (wykonując jeden ruch)            
genAllGameStates:: Color -> Board -> [GameState]
genAllGameStates color board = [(((fst fig),pos), moveFigure (fst fig) pos board) | fig<-positions, pos<-(snd fig)]
        where positions = genAllMoves color board

-- Generuje drzewo wszystkich ruchów zaczynając od podanego koloru i stanu gry
genGameTree :: Color -> GameState -> Tree GameState
genGameTree color gameState = Node gameState [genGameTree (oppositeColor color) gstat | gstat <- gameStates] 
        where gameStates 
                | hasKing color (snd gameState) = genAllGameStates color (snd gameState)
                | otherwise = []

-- Generuje drzewo wszystkich ruchów zaczynając od podanego koloru i planszy
genGameTreeBoard :: Color -> Board -> Tree GameState
genGameTreeBoard color board = genGameTree color (emptyMove,board)

-- Bierze n poziomów z wygenerowanego drzewa stanów gry 
takeGameTree:: Int -> Tree GameState -> Tree GameState
takeGameTree 0 (Node game _) = Node game []
takeGameTree _ (Node game []) = Node game []
takeGameTree n (Node game subGames) = Node game [takeGameTree (n-1) g | g<-subGames]
              
-- Algorytm minimax dla wybierania nastepnego ruchu
minimax:: Color -> Tree GameState -> Int
minimax _ (Node game []) = boardValue (snd game)
minimax White (Node game lista) = maximum $ map (minimax Black) lista
minimax Black (Node game lista) = minimum $ map (minimax White) lista

-- Zwraca wszystkie najlepsze ruchy w danym momencie
getBestStatesFromTree :: Color -> Tree GameState -> [GameState]
getBestStatesFromTree color node = map (rootLabel . snd) (filter (\x -> (fst x) == value) (zip minmaxTab (subForest node)))
--        where minmaxTab = map (minimax (oppositeColor color)) (subForest node)
        where minmaxTab = parMap rseq (minimax (oppositeColor color)) (subForest node)
              value = if (color == White)
                        then maximum minmaxTab
                        else minimum minmaxTab

getNextStateFromTree :: Tree GameState -> ReaderT GameConfig IO (Maybe GameState)
getNextStateFromTree tree = do
    (color,_) <- ask
    let states = getBestStatesFromTree color tree
    let len = length states
    if len > 0 then do
        element <- lift $ getStdRandom $ randomR (0,len-1)
        return $ Just $ states!!element
    else
        return Nothing

getNextState :: Board -> ReaderT GameConfig IO (Maybe GameState)
getNextState board = do
    (color, treeDepth) <- ask
    let tree = takeGameTree treeDepth (genGameTreeBoard color board) -- `using` parTree2
    element <- getNextStateFromTree tree
    return element

parTree0 = r0

parTree1:: Strategy (Tree a)
parTree1 = parTraversable rseq

parTree2:: Strategy (Tree a)
parTree2 (Node h t) = do
    let h' = h `using` rpar
    let t' = t `using` parList (evalTraversable rseq)
    return $ Node h' t'
