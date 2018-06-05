module Chess.Board where

import Data.Maybe
import Data.Char (intToDigit)

data Type = R | N | B | Q | K | P deriving Show -- rodzaj figury
data Color = Black | White deriving Show -- kolor figury
data Figure = Figure Type Color deriving Show --typ określający figurę
type Field = Maybe Figure -- figura albo nic
type Board = [[Field]] -- lista list pól
type Pos = (Int,Int) -- Pozycja

instance Eq Color where
        White == White = True
        Black == Black = True
        _ == _ = False

instance Eq Type where
        R == R = True
        N == N = True
        B == B = True
        Q == Q = True
        K == K = True
        P == P = True
        _ == _ = False

instance Eq Figure where
        (Figure t1 c1) == (Figure t2 c2)  = (c1==c2) && (t1==t2)

-- Plansza startowa w postaci stringa
startBoard = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"
emptyBoard = "........\n........\n........\n........\n........\n........\n........\n........"

-- Zwraca przeciwny kolor
oppositeColor:: Color -> Color
oppositeColor Black = White
oppositeColor White = Black

-- Pokazywanie figur (w postaci symboli figur szachowych)
showFigure :: Figure -> Char
showFigure (Figure R White) = '♜'
showFigure (Figure N White) = '♞'
showFigure (Figure B White) = '♝'
showFigure (Figure Q White) = '♛'
showFigure (Figure K White) = '♚'
showFigure (Figure P White) = '♟'
showFigure (Figure R Black) = '♖'
showFigure (Figure N Black) = '♘'
showFigure (Figure B Black) = '♗'
showFigure (Figure Q Black) = '♕'
showFigure (Figure K Black) = '♔'
showFigure (Figure P Black) = '♙'

-- Pokazywanie figur(w postaci znaków)
writeFigure :: Figure -> Char
writeFigure (Figure R Black) = 'r'
writeFigure (Figure N Black) = 'n'
writeFigure (Figure B Black) = 'b'
writeFigure (Figure Q Black) = 'q'
writeFigure (Figure K Black) = 'k'
writeFigure (Figure P Black) = 'p'
writeFigure (Figure R White) = 'R'
writeFigure (Figure N White) = 'N'
writeFigure (Figure B White) = 'B'
writeFigure (Figure Q White) = 'Q'
writeFigure (Figure K White) = 'K'
writeFigure (Figure P White) = 'P'

-- Czytanie figur
readFigure :: Char -> Figure
readFigure(znak)
        | znak =='r' =  Figure R Black
        | znak =='n' =  Figure N Black
        | znak =='b' =  Figure B Black
        | znak =='q' =  Figure Q Black
        | znak =='k' =  Figure K Black
        | znak =='p' =  Figure P Black
        | znak =='R' =  Figure R White
        | znak =='N' =  Figure N White
        | znak =='B' =  Figure B White
        | znak =='Q' =  Figure Q White
        | znak =='K' =  Figure K White
        | znak =='P' =  Figure P White
        | znak =='♜' =  Figure R White
        | znak =='♞' =  Figure N White
        | znak =='♝' =  Figure B White
        | znak =='♛' =  Figure Q White
        | znak =='♚' =  Figure K White
        | znak =='♟' =  Figure P White
        | znak =='♖' =  Figure R Black
        | znak =='♘' =  Figure N Black
        | znak =='♗' =  Figure B Black
        | znak =='♕' =  Figure Q Black
        | znak =='♔' =  Figure K Black
        | znak =='♙' =  Figure P Black

-- Wartości figur
valueFigure:: Type -> Int
valueFigure(K) = 1000
valueFigure(Q) = 9
valueFigure(R) = 5
valueFigure(B) = 3
valueFigure(N) = 3
valueFigure(P) = 1

--Sprawdza czy figura jest podanego koloru		
colorFigure:: Color -> Field -> Bool
colorFigure White (Just(Figure _ White)) = True
colorFigure Black (Just(Figure _ Black)) = True
colorFigure _ _ = False

-- Sprawdza czy na podanym polu stoi jakas figura
isFigure:: Field -> Bool
isFigure Nothing = False
isFigure _ = True

-- Sprawdza czy gracz ma króla
hasKing:: Color -> Board -> Bool
hasKing color board = elem True (map isKing board)
        where isKing = elem (Just(Figure K color))

-- Pokazanie pola (symbol)
showField :: Field -> Char
showField Nothing = '.'
showField (Just x) = showFigure(x)

-- Pokazanie pola (znak)
writeField :: Field -> Char
writeField Nothing = '.'
writeField (Just x) = writeFigure(x)

-- Odczytanie pola
readField :: Char -> Field
readField('.') = Nothing
readField(znak) = Just(readFigure(znak))
 
-- Konwersja planszy do stringa (symbole szachowe) wraz z numerami pól
showBoard :: Board -> String
showBoard board = "  ABCDEFGH\n" ++ (unlines ( [((intToDigit (8-i)):' ':(map showField (board !! i)))++ (' ':(intToDigit (8-i)):[]) | i<-[0..7]])) ++ "  ABCDEFGH\n"


-- Konwersja planszy do stringa (znaki)        
writeBoard :: Board -> String
writeBoard fieldList = unlines (map printList fieldList)
        where printList fList = (map writeField fList)
   
-- Odczyt planszy ze stringa     
readBoard :: String -> Board
readBoard(boardString) = map mapBoardLine (lines boardString)
        where mapBoardLine line = map readField line

-- Zwraca pole z planszy o podanej pozycji
takeField :: Pos -> Board -> Field
takeField (x,y) board = (board !! x) !! y

-- Zwraca wszystkie figury z planszy obydwu graczy
getAllFigures:: Board -> [(Pos, Field)]
getAllFigures board = filter fig numberedBoard
        where numberedBoard = zip [(x,y) | x<-[0..7], y<-[0..7]] (concat board)
              fig x = isFigure (snd x)

-- Zwraca pozycje figur obydwu graczy
getFiguresPos:: Board -> [Pos]
getFiguresPos board = map getPos (getAllFigures board)
        where getPos = fst

-- Zwraca wszystkie figury obydwu graczy
getFiguresOnly:: Board -> [Field]
getFiguresOnly board = map getFig (getAllFigures board)
        where getFig = snd

-- Zwraca wszystkie figury z planszy w danym kolorze
getAllFiguresByColor:: Color -> Board -> [(Pos, Field)]
getAllFiguresByColor color board = filter playerColor numberedBoard
        where numberedBoard = zip [(x,y) | x<-[0..7], y<-[0..7]] (concat board)
              playerColor x = colorFigure color (snd x)

-- Zwraca pozycje figur gracza
getFiguresPosByColor:: Color -> Board -> [Pos]
getFiguresPosByColor color board = map getPos (getAllFiguresByColor color board)
        where getPos = fst

-- Zwraca wszystkie figury gracza
getFiguresOnlyByColor:: Color -> Board -> [Field]
getFiguresOnlyByColor color board = map getFig (getAllFiguresByColor color board)
        where getFig = snd

-- Oblicza wartość figur gracza
playerValue:: Color -> Board -> Int
playerValue color board = sum [valueFigure typeFig | (Just(Figure typeFig _))<-figures]
        where figures = getFiguresOnlyByColor color board

-- Oblicza wartosc planszy jako: biale - czarne
boardValue:: Board -> Int
boardValue board = foldl valBoard 0 figures
        where figures = getFiguresOnly board
              valBoard val (Just(Figure figure White)) = val + valueFigure(figure)
              valBoard val (Just(Figure figure Black)) = val - valueFigure(figure)