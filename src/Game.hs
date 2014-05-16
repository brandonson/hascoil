module Game (
  startState,
  eventLoop
)where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Renderable
import Snake
import qualified SnakeMap as SM
import Data.Time
import Graphics.UI.SDL.RenderUtils as RU
import System.Random

-- Regular GameState has a snake, the time of the last move, and a Map
-- GameOver contains a GameState that it renders
-- Pause has a Gamestate (the paused game) and a boolean to indicate whether the 
--   p key has been lifted. Not sure the boolean is necessary

data GameState = GameState Snake UTCTime SM.SnakeMap | GameOver GameState | QuitGame | Restart | Pause GameState Bool

instance Renderable GameState where
  render (GameState snake _ smap) ontoSurface = do 
                                              render smap ontoSurface
                                              render snake ontoSurface
  render (GameOver state) on = render state on
  render (Pause state _) on = render state on
  render _ _ = return ()
  
secondsPerMove :: NominalDiffTime
secondsPerMove = 0.05

background :: Color
background = Color 0 0 0


        
eventLoop:: GameState -> SDL.Surface -> IO ()
eventLoop QuitGame _ = return ()
eventLoop Restart video = do
                     -- Generate a new state, start again
                     state <- startState
                     eventLoop state video
eventLoop gs video = do
              newState <- doUpdate gs
              doEventAndRender newState video
doEventAndRender :: GameState -> SDL.Surface -> IO ()
doEventAndRender gs video = do
                              event <- SDL.pollEvent
                              let nextLoopState = handleEvent gs event
                              RU.clearScreen video background
                              render nextLoopState video
                              SDL.flip video
                              eventLoop nextLoopState video
                
                
doUpdate :: GameState -> IO GameState
doUpdate gs = do
                curTime <- getCurrentTime
                doStateFinalization $ updatedStateInstructions gs curTime
                
--Used to indicate how the state should be updated.  We want state updates
--to be pure.  However, random generation is not pure.  Therefore, we
--return setup instructions to indicate whether or not food needs to be placed.

--BasicStateSetup indicates that the GameState it includes should be used.
--NewFoodSetup has a Snake, the UTCTime of this update, and the width and height of the map
data StateSetupInstructions = BasicStateSetup GameState
                              | NewFoodSetup Snake UTCTime (Int, Int)
                              
updatedStateInstructions :: GameState -> UTCTime -> StateSetupInstructions
updatedStateInstructions cs@(GameState snake time gameMap) curTime | diff >= secondsPerMove = nextState
                                                                   | otherwise = BasicStateSetup cs
                                                  where diff = diffUTCTime curTime time
                                                        updatedSnake = updateSnake snake
                                                        snakeValid = isSnakeValid updatedSnake gameMap
                                                        snakeOnFood = snakeHead snake == SM.food gameMap
                                                        nextState | not snakeValid = BasicStateSetup $ GameOver cs
                                                                  | not snakeOnFood = BasicStateSetup $ GameState updatedSnake curTime gameMap
                                                                  | otherwise = NewFoodSetup (incrementSnakeLength updatedSnake) curTime (SM.width gameMap, SM.height gameMap)
updatedStateInstructions state _ = BasicStateSetup state -- Don't touch anything other than a regular, in progress Gamestate
doStateFinalization :: StateSetupInstructions -> IO GameState
doStateFinalization (BasicStateSetup state) = return state
doStateFinalization (NewFoodSetup snake time (w,h)) = do
                                                        newFood <- genFoodCell w h $ snakeHead snake : snakeTail snake
                                                        return $ GameState snake time $ SM.SnakeMap w h newFood

genFoodCell :: Int -> Int -> [Cell] -> IO Cell
genFoodCell w h avoid = do
                          cell <- genRandomCell w h
                          forceNotOnAvoid cell
                        where forceNotOnAvoid chk | not $ chk `elem` avoid = return $ chk
                                                  | otherwise = genFoodCell w h avoid
genRandomCell :: Int -> Int -> IO Cell
genRandomCell w h = do
                      x <- getStdRandom $ randomR (0,w-1)
                      y <- getStdRandom $ randomR (0, h-1)
                      return $ Cell x y
                      
isSnakeValid :: Snake -> SM.SnakeMap -> Bool
isSnakeValid snake@(Snake _ (Cell x y) _) (SM.SnakeMap w h _) | x >= 0 && y >= 0 && x < w && y < h = not $ snakeHeadSelfCollision snake
                                                              | otherwise = False

--Handle any events.  Not really sure why Pause has a Boolean in it.  Should try removing
--that at some point
handleEvent :: GameState -> SDL.Event -> GameState
handleEvent _ Quit = QuitGame
handleEvent _ (KeyUp (Keysym SDLK_ESCAPE _ _)) = QuitGame
handleEvent _ (KeyUp (Keysym SDLK_SPACE _ _)) = Restart
handleEvent (Pause state True) (KeyDown (Keysym SDLK_p _ _)) = state
handleEvent (Pause state False) (KeyUp (Keysym SDLK_p _ _)) = Pause state True
handleEvent (Pause state False) _ = Pause state False
handleEvent gs (KeyDown (Keysym SDLK_p _ _)) = Pause gs False
handleEvent (GameState snake t m) (KeyDown (Keysym keyCode _ _)) = GameState (changeSnakeForKey snake keyCode) t m
handleEvent gs _ = gs

changeSnakeForKey :: Snake -> SDLKey -> Snake
changeSnakeForKey snake SDLK_LEFT = changeSnakeDirection snake DirLeft
changeSnakeForKey snake SDLK_UP = changeSnakeDirection snake DirUp
changeSnakeForKey snake SDLK_RIGHT = changeSnakeDirection snake DirRight
changeSnakeForKey snake SDLK_DOWN = changeSnakeDirection snake DirDown
changeSnakeForKey snake _ = snake

--Generates a random direction.  Used in creating a start state.
randomDirection :: IO Direction
randomDirection = do
                    dirNum <- (getStdRandom $ randomR (0, 3)) :: IO Int
                    return $ numToDir dirNum
                  where numToDir 0 = DirUp
                        numToDir 1 = DirDown
                        numToDir 2 = DirLeft
                        numToDir _ = DirRight
mapWidth :: Int
mapWidth = 100
mapHeight :: Int
mapHeight = 80

--Generate a randomized starting state - single snake cell (head) and a single food
--Snake is traveling in a random direction
startState :: IO GameState
startState = do
              curTime <- getCurrentTime
              dir <- randomDirection
              headCell <- genRandomCell mapWidth mapHeight
              foodCell <- genFoodCell mapWidth mapHeight [headCell]
              return $ GameState (Snake dir headCell []) curTime $ SM.SnakeMap mapWidth mapHeight foodCell
