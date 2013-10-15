module Pacman where
import Environment
import Data.BitVector
import qualified Data.Text.Lazy as L
import Control.Monad.State
import Util
import System.Random
import Data.List
import qualified Data.Map as M
--OPTIMIZATION NOTE: USE AN ARRAY RATHER THAN A BUNCH OF LOCATIONS


-- Dimensions of the board
minX = 0
minY = 0
maxX = 16
maxY = 18


data Ghost = GhostA | GhostB | GhostC | GhostD deriving (Ord, Eq, Show)
type Location = (Int, Int)

data Pacman = Pacman {
  timestep :: Int, -- Number of timesteps
  score :: Integer, -- Reward
  pacmanLoc :: Location, -- Pacman's Coordinates
  resets :: Int, -- Number of Resets
  ghostLocs :: M.Map Ghost Location, -- Location of Ghosts
  ghostSniffs :: M.Map Ghost Int, -- Aggressiveness of ghosts
  poweredUp :: Bool, -- Flag whether Pacman is under the effects of a power pellet.
  powerLeft :: Int, -- How many timesteps left for Pacman to have power
  pelletLocs :: [Location], -- Location of Pellets
  powerPillLocs :: [Location], -- Location of PowerPills
  seed :: StdGen,
  reset :: Bool,
  observation :: Percept
  }


-- Display Board
dispBoard :: Pacman -> String
dispBoard e = let
  -- Board is 17x19, add extra y for newline
  board = [(x,y) | y <- [minY .. maxY] , x <- [minX .. maxX+1]]
  dispCell :: (Int, Int) -> Char
  dispCell loc@(x,y)
    | x == maxX+1 = '\n'
    | y == 8 && (x == minX || x == maxX) = 'x'
    | wall loc  = '*'
    | loc == (aGhostLoc e) = 'A'
    | loc == (bGhostLoc e) = 'B'
    | loc == (cGhostLoc e) = 'C'
    | loc == (dGhostLoc e) = 'D'
    | loc == (pacmanLoc e) = 'P'
    | elem loc (powerPillLocs e) = 'O'
    | elem loc (pelletLocs e) = '.'
    | otherwise = ' '
  in map dispCell board


manhattanDistance :: Location -> Location -> Int
manhattanDistance (x1,x2) (y1,y2) = abs (x1 - y1) + abs (x2 - y2)

setPacLoc :: (Int,Int) -> State Pacman ()
setPacLoc loc = modify (\x -> x{pacmanLoc = loc})

modifyScore :: (Integer -> Integer) -> State Pacman ()
modifyScore f = modify (\x ->x{score = f (score x)})

modifyPellets :: ([(Int,Int)] -> [(Int,Int)]) -> State Pacman ()
modifyPellets f = modify (\x -> x{pelletLocs = f $ pelletLocs x})

getGhostLoc :: Ghost -> Pacman -> Location
getGhostLoc ghost e = M.findWithDefault (error "No Ghost!") ghost (ghostLocs e)

aGhostLoc = getGhostLoc GhostA

bGhostLoc = getGhostLoc GhostB

cGhostLoc = getGhostLoc GhostC

dGhostLoc = getGhostLoc GhostD

putGhostLoc :: Ghost -> Location -> Pacman -> Pacman
putGhostLoc ghost loc e@Pacman{ghostLocs = a} =
  e{ghostLocs = M.insert ghost loc a}

getGhostSniff :: Ghost -> Pacman -> Int
getGhostSniff ghost e = M.findWithDefault (error "No Ghost!") ghost (ghostSniffs e)

putGhostSniff :: Ghost -> Int -> Pacman -> Pacman
putGhostSniff ghost sniff e@Pacman{ghostSniffs = a} =
  e{ghostSniffs = M.insert ghost sniff a}

modifyGhostSniff :: Ghost -> (Int -> Int) -> Pacman -> Pacman
modifyGhostSniff ghost f e@Pacman{ghostSniffs = a} =
  e{ghostSniffs = M.insertWith (\ x y -> f y) ghost 0 a}

movePacman :: (Int, Int) -> State Pacman ()
movePacman (x,y) = do
  modify (\x -> x{score = score x - 1})
  pac <- gets pacmanLoc
  aGhost <- gets aGhostLoc
  bGhost <- gets bGhostLoc
  cGhost <- gets cGhostLoc
  dGhost <- gets dGhostLoc
  pellets <- gets pelletLocs
  pills <- gets powerPillLocs
  power <- gets poweredUp
  let move
  -- Warp Left
        | y == 8 && (x < minX) = setPacLoc (16, y)
  -- Warp Right
        | y == 8 && (x > maxX) = setPacLoc (0,y)
  -- Invalid Move
        | (y < minY || x < minX || y > maxY
           || x > maxX || wall (x,y)) =
          do
            modifyScore (flip (-) 10)
  -- Pacman Bumps into Ghost A
        | (x,y) == aGhost = processGhost GhostA power
  -- Pacman bumps into Ghost B
        | (x,y) == bGhost = processGhost GhostB power
  -- Pacman bumps into Ghost C
        | (x,y) == cGhost = processGhost GhostC power
  -- Pacman bumps into Ghost D
        | (x,y) == dGhost = processGhost GhostD power
  -- Pacman hits a pellet
        | elem (x,y) pellets = do
          modifyScore (+10)
          modifyPellets (delete (x,y))
          setPacLoc (x,y)
          checkWin
  -- Pacman gets a pill
        | elem (x,y) pills =
          modify (\a -> a{poweredUp = True, powerLeft = 5,
                          pacmanLoc = (x,y)})
  -- Pacman moves through empty space
        | otherwise = do
            setPacLoc (x,y)
            
  move

flagReset :: State Pacman ()
flagReset = modify (\x -> x{reset = True})

processGhost :: Ghost -> Bool -> State Pacman ()
processGhost x True = do
  modifyScore (+50)
  resetGhost x

processGhost x False = do
  modifyScore (flip (-) 50)
  flagReset
  
checkWin :: State Pacman ()
checkWin = do
  pellets <- gets pelletLocs
  if null pellets
    then do
    modifyScore (+ 100)
    flagReset
    else
    return ()



wall :: Location -> Bool
wall (x,y) =
  let board = ["                 ",
               " ** *** * *** ** ",
               "O               O",
               " ** * ***** * ** ",
               "    *   *   *    ",
               "*** *** * *** ***",
               "*** *       * ***",
               "*** * * AB* * ***",
               "x   * * CD* *   x",
               "*** * ***** * ***",
               "*** *       * ***",
               "*** * ***** * ***",
               "        P        ",
               " ** *** * *** ** ",
               "O *           * O",
               "* * * ***** * * *",
               "    *   *   *    ",
               " ****** * ****** ",
               "                 "]
  in
    (x < minX) || (x > maxX) || (y < minY) || (y > maxY) || (board !! y) !!x == '*'



scatterPellets :: State Pacman [(Int,Int)]
scatterPellets = do
  g <- gets seed
  dots <- gets pelletLocs
  pills <- gets powerPillLocs
  pac <- gets pacmanLoc
  aGhost <- gets aGhostLoc
  bGhost <- gets bGhostLoc
  cGhost <- gets cGhostLoc
  dGhost <- gets dGhostLoc
  let (g0, g1) = System.Random.split g
      bits = randoms g0
      board = [(x,y) |x <- [minX .. maxX], y <- [minY .. maxY]]
      blank :: Location -> Bool
      blank loc@(x,y) =
        not (wall loc) && notElem loc dots && notElem loc pills
        && loc /= pac && loc /= aGhost && loc /= bGhost && loc /= cGhost
        && loc /= dGhost && loc /= (8,0) && loc /= (8,16)
      good ((x,y),b) = b && blank (x,y)
  modify (\x -> x{seed = g1})
  return $ map fst (filter good (zip board bits))

resetGhost :: Ghost -> State Pacman ()
resetGhost ghost = do
  resetLoc ghost
  resetSniff ghost
 where
   -- Reset ghost location
   resetLoc :: Ghost -> State Pacman ()
   resetLoc GhostA = modify (putGhostLoc GhostA (8,7))
   resetLoc GhostB = modify (putGhostLoc GhostB (9,7))
   resetLoc GhostC = modify (putGhostLoc GhostC (8,8))
   resetLoc GhostD = modify (putGhostLoc GhostD (9,8))
   -- Reset ghost aggressiveness
   resetSniff :: Ghost -> State Pacman ()
   resetSniff ghost = modify (putGhostSniff ghost 0)


resetMap :: State Pacman ()
resetMap = do
  resetGhost GhostA
  resetGhost GhostB
  resetGhost GhostC
  resetGhost GhostD
  modify (\x -> x{
             resets = resets x + 1,
             pacmanLoc = (8,12),
             poweredUp = False,
             powerLeft = 0,
             observation = 0,
             reset = False,
             powerPillLocs = [(minX,2),(maxX,2),(minX,14),(maxX,14)]
            })
  ps <- scatterPellets
  modify (\x -> x{pelletLocs = ps})

updateObservation = do
  pac@(pacX,pacY) <- gets pacmanLoc
  a <- gets aGhostLoc
  b <- gets bGhostLoc
  c <- gets cGhostLoc
  d <- gets dGhostLoc
  pels <- gets pelletLocs
  pills <- gets powerPillLocs
  pow <- gets poweredUp
  -- Check Pacman's Surroundings
  let b0 = (pacY - 1) < minY || wall(pacX, pacY - 1)
      b1 = (pacX + 1) > maxX || wall(pacX+1, pacY)
      b2 = (pacY + 1) > maxY || wall(pacX, pacY+1)
      b3 = (pacX - 1) < minX || wall(pacX-1, pacY)
      anyGhost = or . map (flip elem [a,b,c,d])
      anyPellet = or . map (flip elem pels)
      -- See if there's a ghost or pellet north
      north = takeWhile (not.wall) [(pacX,y) | y <- [pacY .. minY]]
      b4 = anyGhost north
      b11 = anyPellet north
      -- See if there's a ghost or pellet east
      east = takeWhile (not.wall) [(x, pacY) | x <- [pacX .. maxX]]
      b5 = anyGhost east
      b12 = anyPellet east
      -- See if there's a ghost or pellet south
      south = takeWhile (not.wall) [(pacX,y) | y <- [pacY .. maxY]]
      b6 = anyGhost east
      b13 = anyPellet east
      -- See if there's a ghost or pellet west
      west = takeWhile (not.wall) [(x,pacY) | x <- [pacX .. minY]]
      b7 = anyGhost west
      b14 = anyPellet west
      -- See if there's a pellet nearby
      rad4 = filter (\x -> manhattanDistance x pac <= 4) pels
      rad3 = filter (\x -> manhattanDistance x pac <= 3) rad4
      rad2 = filter (\x -> manhattanDistance x pac <= 2) rad3
      b8 = (not.null) rad2
      b9 = (not.null) rad3
      b10 = (not.null) rad4
      -- See if pacman is under effects of powerpill
      b15 = pow
  modify (\x -> x{observation = nat.fromBits $
                     [b15,b14,b13,b12,b11,b10,
                      b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]})


findPacman :: Ghost -> State Pacman Bool
findPacman ghost = do
  pacman <- gets pacmanLoc
  sniff <- gets $ getGhostSniff ghost
  loc <- gets $ getGhostLoc ghost
  if (sniff < 0 && close pacman loc)
    then do
           modify (putGhostSniff ghost 5)
           return True
    else return (sniff < 0)
  where close :: Location -> Location -> Bool
        close pacman ghost = manhattanDistance pacman ghost <= 5

moveGhost :: Ghost -> State Pacman ()
moveGhost ghost =
  do
    sniff <- gets $ getGhostSniff ghost
    found <- findPacman ghost
    if found && sniff > (-2)
      then ghostPursuitMove ghost
      else ghostRandomMove ghost
    modify $ putGhostSniff ghost (sniff - 1)

ghostPursuitMove :: Ghost -> State Pacman ()
ghostPursuitMove ghost =
  do
    pacman <- gets pacmanLoc
    power <- gets poweredUp
    loc@(oldX, oldY) <- gets $ getGhostLoc ghost
    let currentDistance = manhattanDistance pacman loc

        goodMove :: Location -> State Pacman Bool
        goodMove newLoc = do
          valid <- validGhostMove newLoc
          return $ (manhattanDistance pacman newLoc) < currentDistance
        up@(upX, upY) = (oldX, oldY-1)
        down@(downX, downY) = (oldX, oldY+1)
        left@(leftX, leftY) = (oldX - 1, oldY)
        right@(rightX, rightY) = (oldX + 1, oldY)
    goodUp <- goodMove up
    goodDown <- goodMove down
    goodLeft <- goodMove left
    goodRight <- goodMove right
    when goodLeft (modify $ putGhostLoc ghost left)
    when goodRight (modify $ putGhostLoc ghost right)
    when goodUp (modify $ putGhostLoc ghost up)
    when goodDown (modify $ putGhostLoc ghost down)
    newLoc <- gets $ getGhostLoc ghost
    when (newLoc == loc) (ghostRandomMove ghost)
    when (newLoc == pacman) (processGhost ghost power)
    

validGhostMove :: Location -> State Pacman Bool
validGhostMove loc@(x,y) = do
  ghostA <- gets aGhostLoc
  ghostB <- gets bGhostLoc
  ghostC <- gets cGhostLoc
  ghostD <- gets dGhostLoc
  return $ not (
    x < 0 || x > 16 || y < 0 || y > 16
    || wall loc || elem loc [ghostA, ghostB, ghostC, ghostD])

ghostRandomMove :: Ghost -> State Pacman ()
ghostRandomMove ghost =
  do
    g <- gets seed
    pow <- gets poweredUp
    pac <- gets pacmanLoc
    (ghostX, ghostY) <- gets $ getGhostLoc ghost
    let available = filter (not.wall) [(ghostX,ghostY-1), (ghostX+1, ghostY),
                                       (ghostX,ghostY+1), (ghostX-1, ghostY)]
    when (not.null $ available)(
      do
        let (i, g1) = randomR (0, length available - 1) g
            newLoc = available !! i
        modify (\x -> x{seed = g1})
        if newLoc == pac
          then processGhost ghost pow
          else modify $ putGhostLoc ghost newLoc)
makeMove :: Action -> State Pacman ()
makeMove a = do
  modify (\x -> x{timestep = timestep x + 1,
                  score = 200})
  (x,y) <- gets pacmanLoc
  let move :: Action -> (Int, Int)
      move 0 = (x,y-1)
      move 1 = (x+1,y)
      move 2 = (x,y+1)
      move 3 = (x-1,y)
      move _ = (x,y)
  movePacman (move a)
  moveGhost GhostA
  moveGhost GhostB
  moveGhost GhostC
  moveGhost GhostD
  updateObservation
  res <- gets reset
  when res resetMap


instance Show Pacman where
  show e@Pacman{timestep = t, resets = s} =
    let preamble = "Timestep: " ++ show t ++ "Resets: " ++ show s ++ "\n"
    in preamble ++ dispBoard e

instance Environment Pacman where
  performAction a e = execState (makeMove a) e
  getReward = score
  getObservation = observation
  makeNewEnvironment o =
    let x = Pacman{score = 0,
                   timestep = 0,
                   pacmanLoc = (0,0),
                   resets = 0,
                   ghostLocs = M.empty,
                   ghostSniffs = M.empty,
                   poweredUp = False,
                   powerLeft = 0,
                   seed = mkStdGen $ read (getRequiredOption "seed" o),
                   reset = False,
                   pelletLocs = [],
                   powerPillLocs = [],
                   observation = 0
                  }
    in execState resetMap x
  maxAction e = 3
  maxObservation e = 2^16 - 1
  maxReward e = 400
