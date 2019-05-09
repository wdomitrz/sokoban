{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Directory
import Data.Maybe

--- Typy
-- typ świata  gracz kierunek  boxy    mapa    xDim      yDim      lvlNum  movNo lvlName
--           S c     d         b       mm      xd        yd        n       mn    ln
-- S c d b mm xd yd n mn
data State = S Coord Direction Boxes MazeMap [Integer] [Integer] Integer Integer LevelName
instance Eq State where
  (==) (S c1 d1 b1 _ _ _ n1 _ _) (S c2 d2 b2 _ _ _ n2 _ _) =
        (c1 == c2) && (d1 == d2) && (b1 == b2) && (n1 == n2)

-- Typ z informacją czy gra właśnie działa
data SSState world = StartScreen | Running world

-- Typ gry z możliwością cofania
data WithUndo a = WithUndo a [a]

-- Typ stanu gry (gra, wygrany poziom, wygrana gra)
data LevelState = Game | LevelWon | GameWon deriving Eq

-- typ rodzajów pól
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

-- typ kierunku ruchu
data Direction = R | U | L | D deriving Eq

-- typ współrzędnych, np. przydatne do między innymi delete
data Coord = C Integer Integer deriving Eq

-- Typ interakcji
data Interaction world = Interaction
        world
        (Event -> world -> world)
        (world -> SizedPicture)

-- Typ listy pudełek
type Boxes = [Coord]

-- Typ nazwy poziomu
type LevelName = String

-- Typ mapy labiryntu
type MazeMap = Coord -> Tile

-- Typ labiryntu (Uwaga - labirynt domyślnie nie zawiera pudełek)
data Maze = Maze Coord MazeMap Boxes LevelName

-- Typ obsługi zdarzeń
type Handler = Event -> State -> State

-- Typ rysowania stanu
type Draw = State -> SizedPicture



---- Funkcje do parsowania poziomów
-- Znaki na pola - UWAGA - domyślnie bez pudełek
charToTile :: Char -> Tile
charToTile c | c == '#'  = Wall
             | c == '@'  = Ground
             | c == 'p'  = Ground
             | c == '+'  = Storage
             | c == '$'  = Ground
             | c == 'b'  = Ground
             | c == '*'  = Storage
             | c == '.'  = Storage
             | c == ' '  = Ground
             | c == '-'  = Ground
             | c == '_'  = Blank
             | otherwise = Blank

-- Sprawdzenie, czy dane pole reprezentuje pudełko (na jakimś polu)
isBox :: Char -> Bool
isBox c = c == '$' || c == 'b' || c == '*'

-- Znaki oznaczające gracza
playerChars :: [Char]
playerChars = ['@', '+', 'p']

-- Funkcja mówiąca nam, czy na danym polu stoi gracz
isCharPlayer :: Char -> Bool
isCharPlayer c = elem c playerChars

-- Poruszenie współrzędnej do góry
upCoord :: Coord -> Coord
upCoord (C x y) = (C x (y + 1))

-- Poruszenie współrzędnej w prawo
rightCoord :: Coord -> Coord
rightCoord (C x y) = (C (x + 1) y)

type Map = [[Char]]

-- Bardzo fajna funkcja pomocnicza
foldTable :: (Coord -> a -> b -> b) -> b -> [[a]] -> b
foldTable modifyValue initialValue =
  let foldRow = -- iv od initial value
        foldl (\(c, res) el -> (rightCoord c, modifyValue c el res))
  in  snd . foldl (\acc@(c, _) row -> (upCoord c, snd $ foldRow acc row))
                  (C 0 0, initialValue)

-- Parser poziomów (stosuję odpowiednio foldl i foldr, żeby zachować kolejność)
mazeParser :: (Map, LevelName) -> Maze
mazeParser (mapPicture, lvlName) =
  (Maze (plr mapPicture) (createMap mapPicture) (getBoxes mapPicture) lvlName)
 where
  plr :: Map -> Coord
  plr table =
    let helper :: Coord -> Char -> Maybe Coord -> Maybe Coord
        helper c z res = if isCharPlayer z then Just c else res
    in  fromJust (foldTable helper Nothing table)

  createMap :: Map -> MazeMap -- mapa z jej obrazka
  createMap table =
    let tableToFunction :: Map -> (Coord -> Char)
        tableToFunction = foldTable
          (\c el res -> (\x -> if x == c then el else res x))
          (\_ -> '_')
    in  charToTile . (tableToFunction table)

  getBoxes :: Map -> Boxes
  getBoxes = foldTable (\c el res -> if isBox el then c : res else res) []

type Mazes = [Maze]

--- Funkcje na listach
listLength :: [a] -> Integer
listLength = foldr (\_ acc -> acc + 1) 0

-- UWAGA!!! Listy numeruję od 0
nth :: [a] -> Integer -> a
nth l n = snd $ foldr
  (\el (m, v) -> if m == n then (n + 1, el) else (m + 1, v))
  (0, undefined)
  l

allList :: (a -> Bool) -> [a] -> Bool
allList isOk = foldr (\el acc -> acc && (isOk el)) True



--- Funkcje na grafach
-- Pomocnicze typy dla grafów
type Visited a = (a -> Bool)
type Neighbours a = (a -> [a])
type Node a = a

-- Funkcja pomocnicza - DFS
dfs :: Eq a => Node a -> Neighbours a -> Visited a
dfs initial neighbours =
  let helper v visited
        | visited v = visited
        | otherwise = foldr (\u vis -> helper u vis)
                            (\u -> u == v || visited u)
                            (neighbours v)
  in  helper initial (\_ -> False)

reachable :: Eq a => Node a -> Node a -> Neighbours a -> Bool
reachable v initial neighbours = dfs initial neighbours v




--- Sprawdzanie poziomów
-- zmienia współrzędne na takie, które ą przesunięte w danym kierunku
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord d c@(C x y) | d == R    = C (x + 1) y
                          | d == U    = C x (y - 1)
                          | d == L    = C (x - 1) y
                          | d == D    = C x (y + 1)
                          | otherwise = c

-- możliwe kierunki
directions :: [Direction]
directions = [U, D, R, L]

--- Obliczanie odpowiednych wymiarów dla danego poziomu
someDim :: (Coord -> Integer) -> Maze -> [Integer]
someDim takeSome (Maze c mm _ _) =
  let
    canWalk :: Tile -> Bool
    canWalk x = (x /= Blank)

    neighbours :: Coord -> Boxes
    neighbours v = foldr
      ( \d acc ->
        let nv = adjacentCoord d v in if canWalk $ mm nv then nv : acc else acc
      )
      []
      directions

    dfsList :: Eq a => Node a -> Neighbours a -> [a]
    dfsList initial neigh =
      let helper v ans@(visited, visList)
            | visited v = ans
            | otherwise = foldr (\u res -> helper u res)
                                ((\u -> u == v || visited u), v : visList)
                                (neigh v)
      in  snd (helper initial (\_ -> False, []))

    allPossibleFields :: Boxes
    allPossibleFields = dfsList c neighbours

    minSome, maxSome :: Integer
    minSome = foldr min (takeSome c) $ map takeSome allPossibleFields
    maxSome = foldr max (takeSome c) $ map takeSome allPossibleFields
  in
    [minSome .. maxSome]

xDim :: Maze -> [Integer]
xDim = someDim (\(C x _) -> x)

yDim :: Maze -> [Integer]
yDim = someDim (\(C _ y) -> y)

-- Rysowanie Boolów
pictureOfBools :: [Bool] -> SizedPicture
pictureOfBools xs = (go 0 xs)
 where
  n = length xs
  k = findK 0 -- k is the integer square of n
  findK i | i * i >= n = i
          | otherwise  = findK (i + 1)
  go _ [] = blankSized
  go i (b:bs) =
    translated (toInteger (i `mod` k)) (toInteger (i `div` k)) (pictureOfBool b)
      & go (i + 1) bs

  pictureOfBool True  = colored '+'
  pictureOfBool False = colored '-'



--- Funkcja opisujące labirynt i jego stan początkowy i sprawdzające wygraną
-- sprawdzenie, czy gracz wygrywa (sprawdzenie, czy każde pudełko jest na pozycji Storage).
-- pod uwagę brane są tylko pudełka z listy pudełek, na którą trafiają tylko pudełka osiągalne
isWinning :: State -> Bool
isWinning (S _ _ b mm _ _ _ _ _) = allList
  ( \c ->
    let helper :: Tile -> Bool
        helper Storage = True
        helper _       = False
    in  helper $ mm c
  )
  b

levelState :: Mazes -> State -> LevelState
levelState mazes s@(S _ _ _ _ _ _ n _ _) = if isWinning s
  then if n == (listLength mazes) then GameWon else LevelWon
  else Game

-- Funkcja wyszukująca skrzynie (używa tylko skrzyń osiągalnych)
initialBoxes :: Maze -> Boxes
initialBoxes (Maze c mm b _) =
  let
    canWalk :: Tile -> Bool
    canWalk x = ((x /= Wall) && (x /= Blank))

    neighbours :: Coord -> [Coord]
    neighbours v = foldr
      ( \d acc ->
        let nv = adjacentCoord d v in if canWalk $ mm nv then nv : acc else acc
      )
      []
      directions

    mazeReachable :: Coord -> Bool
    mazeReachable who = reachable who c neighbours
  in
    filter mazeReachable b

-- dodawanie skrzyń na podanych pozycjach
addBoxes :: Boxes -> MazeMap -> MazeMap
addBoxes b mm c = if elem c b then Box else mm c

-- mapa labiryntu
initialMap :: Maze -> MazeMap
initialMap (Maze _ mm _ _) = mm

-- początkowy kierunek gracza
initialPlayerDirection :: Maze -> Direction
initialPlayerDirection _ = U

-- początkowa pozycja gracza
initialCoord :: Maze -> Coord
initialCoord (Maze c _ _ _) = c

initalName :: Maze -> String
initalName (Maze _ _ _ ln) = ln

-- początkowy świat danego labiryntu
initialWorld :: Integer -> Maze -> State
initialWorld n m = S (initialCoord m)
                     (initialPlayerDirection m)
                     (initialBoxes m)
                     (initialMap m)
                     (xDim m)
                     (yDim m)
                     n
                     0
                     (initalName m)





--- Funkcje rysujace
-- ryzowanie gracza
player :: Direction -> SizedPicture
player _ = colored '@'

-- rysowanie ściany, podłogi, miejsca na skrzynię, skrzyni i pustego pola
wall, ground, storage, box, nothing :: SizedPicture
wall = colored '#'
ground = colored ' '
storage = colored '.'
box = colored '$'
nothing = blankSized

-- rysowanie pola o konkretnym numerze
drawTile :: Tile -> SizedPicture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = nothing

-- ekran startowy
startScreen :: SizedPicture
startScreen =
  lettering "Sokoban!" & translated 0 2 (lettering informationStart)


informationNext, informationEnd, informationStart :: String
informationNext = "P - startscreen, R - play level, N - continue"
informationEnd = "P - startscreen, R - play last level again, Q - exit"
informationStart = "Press Space to start"

-- Wizualizacja progresu
progress :: Mazes -> Integer -> SizedPicture
progress mazes n =
  pictureOfBools $ map (\x -> x <= n) [1 .. (listLength mazes)]

-- informacja o wygraniu gry
-- mógłbym pisać też numer ukończonego poziomu, ale jest wymagane:
-- 'Poziom ukończony, liczba ruchów: N'
nextLevel :: Mazes -> Integer -> Integer -> LevelName -> SizedPicture
nextLevel mazes n nm ln =
  lettering
      (  "Level finished, number of moves: "
      ++ (show nm)
      ++ ", level name: "
      ++ ln
      )
    & translated 0 2 (lettering informationNext)
    & translated 0 4 (progress mazes n)

-- informacja o wygraniu poziomu
-- mógłbym pisać też numer ukończonego poziomu, ale jest wymagane:
-- 'Poziom ukończony, liczba ruchów: N'
youWon :: Mazes -> Integer -> Integer -> LevelName -> SizedPicture
youWon mazes n nm ln =
  lettering "YOU WON!!!"
    & translated
        0
        2
        ( lettering ("Number of moves: " ++ (show nm) ++ ", last level: " ++ ln)
        & translated 0 2 (lettering informationEnd)
        )
    & translated 0 6 (progress mazes n)



--- Rysowanie mapy na podstawie opisu labiryntu i jego stanu
-- rysowanie elementu labiryntu na konkretnej pozycji
drawMazeElement :: MazeMap -> Integer -> Integer -> SizedPicture
drawMazeElement mm x y = translated x y $ drawTile $ mm (C x y)

-- rysowanie danego laburyntu na polach o danych współrzędnych xowych i yowych
pictureTheMazeInRange :: MazeMap -> [Integer] -> [Integer] -> SizedPicture
pictureTheMazeInRange mm xs ys =
  pictures [ drawMazeElement mm x y | x <- xs, y <- ys ]

-- rysowanie obrazka z danymi koordynatami
atCoord :: Coord -> SizedPicture -> SizedPicture
atCoord (C x y) pic = translated x y pic

-- rysowanie pudeł
pictureOfBoxes :: Boxes -> SizedPicture
pictureOfBoxes b = pictures [ atCoord c box | c <- b ]

-- rysowanie stanu świata po wygranej
drawSuccess :: Mazes -> Draw
drawSuccess mazes (S _ _ _ _ _ _ n mn ln) = youWon mazes n mn ln

-- po przejściu poziomu
drawNextLevel :: Mazes -> Draw
drawNextLevel mazes (S _ _ _ _ _ _ n mn ln) = nextLevel mazes n mn ln

-- rysowanie stanu świata w trakcie gry
drawPlaying :: Draw
drawPlaying (S c d b mm xd yd _ _ _) =
  atCoord c (player d) & pictureOfBoxes b & pictureTheMazeInRange mm xd yd

-- rysowanie stanu świata
draw :: Mazes -> Draw
draw mazes s =
  let how :: Draw
      how | ls == LevelWon = drawNextLevel mazes
          | ls == GameWon  = drawSuccess mazes
          | otherwise      = drawPlaying
        where ls = levelState mazes s
  in  how s




--- Obsługa wejścia i ruch
-- obsługa przyciśnięć klawiszy po wygranej
handleSuccess :: Handler
handleSuccess _ s = s

handleNextLevel :: Mazes -> Handler
handleNextLevel mazes (KeyPress key) (S _ _ _ _ _ _ n _ _) | key == "n" =
  initialWorld (n + 1) (nth mazes n)
handleNextLevel _ _ s = s

-- obsługa przyciśnięć klawiszy w trakcie gry (na razie bez strzałek)
handlePlaying :: Handler
handlePlaying (KeyPress key) s | key == "w" = w' U
                               | key == "s" = w' D
                               | key == "a" = w' L
                               | key == "d" = w' R
  where w' x = controlledAdjacentCoord x s
handlePlaying _ s = s

-- resetowalne poziomy
handleLevelReset :: Mazes -> Handler -> Handler
handleLevelReset mazes _ (KeyPress key) (S _ _ _ _ _ _ n _ _) | key == "r" =
  initialWorld n (nth mazes (n - 1))
handleLevelReset _ how e s = how e s

-- obsługa przyciśnięć klawiszy
handleEvent :: Mazes -> Handler
handleEvent mazes e s =
  let how :: Handler
      how | ls == LevelWon = handleNextLevel mazes
          | ls == GameWon  = handleSuccess
          | otherwise      = handlePlaying
        where ls = levelState mazes s
  in  how e s

-- Przesuwanie pudełka z danej pozycji na inną
moveBox :: Coord -> Coord -> Boxes -> Boxes
moveBox from to b = map (\c -> if c == from then to else c) b

-- sprawdzanie ściany i pudełka
controlledAdjacentCoord :: Direction -> State -> State
controlledAdjacentCoord d s@(S c _ b mm xd yd n mn ln) =
  let helper :: Tile -> State
      helper Ground  = ns
      helper Storage = ns
      -- sprawdzanie pudełka
      helper Box =
        let helper2 :: Tile -> State
            helper2 Ground  = nbs
            helper2 Storage = nbs
            helper2 _       = sd
        in  helper2 (mazeWithBoxes nbc)
              -- Nowy możliwy stan świata z przesuniętym pudełkiem (new box state)
       where
        nbs = (S nc d nb mm xd yd n (mn + 1) ln)
        -- Nowe możliwe położenia pudełek (new boxes)
        nb  = moveBox obc nbc b
        -- Nowa możliwa pozycja pudełka (new box coord)
        nbc = adjacentCoord d obc
        -- Stara pozycja pudełka (old box coord)
        obc = nc
      helper _ = sd
  in  helper (mazeWithBoxes nc)
 where
  mazeWithBoxes = addBoxes b mm
  sd            = if s0 /= s then s1 else s0
  s0            = (S c d b mm xd yd n mn ln)
  s1            = (S c d b mm xd yd n (mn + 1) ln)
  ns            = (S nc d b mm xd yd n (mn + 1) ln)
  nc            = adjacentCoord d c




--- Funkcja świata i resetowalny świat oraz ekran startowy
-- świat gry na podstawie stanu początkowego, zmiany w czasie, zmiany po wydarzeniach i funkcji rysującej v3
etap5 :: Mazes -> IO ()
etap5 mazes = runInteraction $ Interaction
  (initialWorld 1 (nth mazes 0)) -- początkowy stan świata
  (handleLevelReset mazes $ handleEvent mazes)   -- obsługa wydarzeń (wciskania klawiszy) (z resetowalnymi poziomami)
  (draw mazes)     -- rysowanie świata

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle drawing) = Interaction state0'
                                                           handle'
                                                           drawing'
 where
  state0' = WithUndo state0 []
  handle' (KeyPress key) (WithUndo s stack) | key == "u" = case stack of
    s':stack' -> WithUndo s' stack'
    []        -> WithUndo s []
  handle' e (WithUndo s stack) | s' == s   = WithUndo s stack
                               | otherwise = WithUndo (handle e s) (s : stack)
    where s' = handle e s
  drawing' (WithUndo s _) = drawing s

-- Funkcja do dodania ekranu startowego
withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 handle drawing) = Interaction state0'
                                                                  handle'
                                                                  drawing'
 where
  state0' = StartScreen

  handle' (KeyPress key) StartScreen | key == " " = Running state0
  handle' _              StartScreen              = StartScreen
  handle' e              (Running s)              = Running (handle e s)

  drawing' StartScreen = startScreen
  drawing' (Running s) = drawing s

-- Funkcja do resetowania świata
resettable :: Interaction s -> Interaction s
resettable (Interaction state0 handle drawing) = Interaction state0
                                                             handle'
                                                             drawing
 where
  handle' (KeyPress key) _ | key == "p" = state0
  handle' e              s              = handle e s

-- Funkcja do uruchamiana interakcji
runInteraction :: Eq s => Interaction s -> IO ()
runInteraction i = interactionOf w e d
  where (Interaction w e d) = (resettable . withStartScreen . withUndo) i

-- katalog z poziomami
levels :: FilePath
levels = "levels"

-- https://stackoverflow.com/questions/50120508/understanding-splitlines-function-in-haskell
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in  pre : case suf of
        ('\r':'\n':rest) -> splitLines rest
        ('\r'     :rest) -> splitLines rest
        ('\n'     :rest) -> splitLines rest
        _                -> []

isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

readMapsFrom :: FilePath -> IO [(Map, LevelName)]
readMapsFrom levelsFolder =
  let helperForMap :: FilePath -> IO (Map, LevelName)
      helperForMap filePath = do
        mazeMapOneLine <- readFile filePath
        return (splitLines mazeMapOneLine, filePath)
  in  do
        filesToRead <- listDirectory levelsFolder
        prevDir     <- getCurrentDirectory
        setCurrentDirectory levelsFolder
        maps <- sequence (map helperForMap filesToRead)
        setCurrentDirectory prevDir
        return maps


-- główna funkcja programu
main :: IO ()
main = do
  maps <- readMapsFrom levels -- wczytanie poziomów z danego katalogu
  etap5 (map mazeParser maps) -- rozpoczęcie gry


-- Typ koloru (coś, co może zostać pokazane na ekranie (na razie Char. Może kiedyś string))
type Color = Char

-- Typ zdażenia
data Event = KeyPress String

-- Typ ekranu
type Screen = String

-- Obrazek ze stałym rozmiarem
--              x          y          color
type DrawFun = Integer -> Integer -> Color

-- Typ wymiarów
--            xSize    ySize
type Range = (Integer, Integer)

connectRanges :: Range -> Range -> Range
connectRanges (x1, y1) (x2, y2) = (max x1 x2, max y1 y2)

translateRange :: Integer -> Integer -> Range -> Range
translateRange xt yt (x, y) = (x + xt, y + yt)

allFieldsCoords :: Range -> [(Integer, Integer)]
allFieldsCoords (xSize, ySize) =
  [ (x', y') | y' <- [0 .. (ySize - 1)], x' <- [0 .. xSize] ] -- dobra kolejność rysowania

-- SizedDrawFun     wymiary obrazek
type SizedDrawFun = (Range, DrawFun)

-- Kolor tła
backgroundColor :: Color
backgroundColor = ' '

blankFun :: DrawFun
blankFun _ _ = backgroundColor

drawFunSizedToScreen :: SizedDrawFun -> Screen
drawFunSizedToScreen (range@(xSize, _), toDraw) = map
  (\(x, y) -> if x == xSize then '\n' else toDraw x y)
  (allFieldsCoords range)


type Picture = DrawFun -> DrawFun
type SizedPicture = (Range, Picture)

blank :: Picture
blank = id

blankSized :: SizedPicture
blankSized = ((0, 0), blank)

(&&&) :: Picture -> Picture -> Picture
(&&&) = (.)

(&) :: SizedPicture -> SizedPicture -> SizedPicture
(&) (r1, p1) (r2, p2) = (connectRanges r1 r2, p1 &&& p2)

toDrawFunSized :: SizedPicture -> SizedDrawFun
toDrawFunSized (range, picture) = (range, picture blankFun)

toScreen :: SizedPicture -> Screen
toScreen picture = drawFunSizedToScreen $ toDrawFunSized picture

translatedPicture :: Integer -> Integer -> Picture -> Picture
translatedPicture xt yt picture f x y =
  picture (\x' y' -> f (x' + xt) (y' + yt)) (x - xt) (y - yt)

translated :: Integer -> Integer -> SizedPicture -> SizedPicture
translated xt yt (range, picture) =
  (translateRange xt yt range, translatedPicture xt yt picture)

-- Typ obsługi zdażeń
type EventHandler world = Event -> world -> world

-- Typ rysowania
type Drawer world = world -> SizedPicture

-- Typ interaktywnego świata
type Interactive world = world
                          -- The initial state of the interaction.
                          -> EventHandler world
                          -- The event handling function, which updates the state given a user interface event.
                          -> Drawer world
                          -- The visualization function, which converts the state into a picture to display.
                          -> IO ()

interactionOf :: Interactive world
interactionOf w e d =
  let loop w' = do
        clearScreen    -- czyszczenie ekranu
        showPicture (d w') -- pokazywanie aktualnego stanu
        input <- getChar  -- sprawdzanie wejścia
        if input == 'q' -- sprawdzanie, czy chcemy zakończyć
          then clearScreen -- czyszczenie ekranu na koniec
          else loop (e (KeyPress (toString input)) w')
  in  do
        hSetBuffering stdin NoBuffering
        loop w

-- Zamiana znaku na napis
toString :: Char -> String
toString c = [c]

pictures :: [SizedPicture] -> SizedPicture
pictures = foldr (&) blankSized

clearScreen :: IO ()
clearScreen = putStr "\ESCc"

-- Pokazywanie obrazka na ekranie
showPicture :: SizedPicture -> IO ()
showPicture = showScreen . toScreen

-- Pokazywanie ekranu
showScreen :: Screen -> IO ()
showScreen = putStr

lettering :: String -> SizedPicture
lettering l =
  ( (ll, 1)
  , \f x y -> if y /= 0 || x < 0 || x >= ll then f x y else nth l (ll - 1 - x)
  ) -- Można efektywniej, ale wystarczy
  where ll = listLength l

-- pokoloranie pola na jakiś kolor
colored :: Color -> SizedPicture
colored c =
  ( (1, 1)
  , \f x y ->
    let fieldFun :: DrawFun
        fieldFun 0  0  = c
        fieldFun x' y' = f x' y'
    in  fieldFun x y
  )
