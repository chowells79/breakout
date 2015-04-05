{-# LANGUAGE BangPatterns #-}

import Graphics.UI.SDL

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Data.Function

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Control.Concurrent

import System.Mem

import Prelude hiding (init)

data SimStep = EndStep Word32 | Shutdown deriving Show

data GameState = GameState Word32 deriving Show

main :: IO ()
main = do
    init $ SDL_INIT_VIDEO .|. SDL_INIT_TIMER

    state <- newEmptyMVar
    events <- newChan
    gameLoopDone <- newEmptyMVar

    forkIO $ gameLoop events state gameLoopDone
    mainLoop 60 state 60 events

    quit

    takeMVar gameLoopDone


mainLoop :: Word32
         -> MVar GameState
         -> Word32
         -> Chan (Either SimStep Event)
         -> IO ()
mainLoop frameRate stateVar simRate events = alloca $ \evtP -> do
    res <- windowAndRenderer "Breakout" 640 480 0 0
    let (w, r) = either error id res

    let frames = intervalPoints frameRate
        sims = intervalPoints simRate

    flip fix (frames, sims) $ \wait (f:fs, s:ss) -> do
        fix $ \moreEvents -> do
            stat <- pollEvent evtP
            case stat of
                1 -> do
                    evt <- peek evtP
                    case evt of
                        QuitEvent _ _ -> do
                            writeChan events $ Left Shutdown
                            return ()
                        _ -> do
                            writeChan events $ Right evt
                            moreEvents
                _ -> do
                    t <- getTicks
                    case (t >= f, t>= s) of
                        (_, True) -> do
                            writeChan events . Left . EndStep $ t
                            wait (f:fs, ss)
                        (True, _) -> do
                            state <- readMVar stateVar
                            renderState r state
                            wait (dropWhile (<= t) fs, s:ss)
                        _ -> do
                            threadDelay 100
                            performMajorGC
                            wait (f:fs, s:ss)

    destroyRenderer r
    destroyWindow w


renderState :: Renderer -> GameState -> IO ()
renderState renderer (GameState c) = do
    t1 <- getTicks
    let r = fromIntegral $ c `div` (256 * 256)
        g = fromIntegral $ c `div` 256
        b = fromIntegral $ c
    setRenderDrawColor renderer r g b 255
    renderClear renderer
    renderPresent renderer
    t2 <- getTicks
    print ("renderState", t2 - t1)


gameLoop :: Chan (Either SimStep Event)
         -> MVar GameState
         -> MVar ()
         -> IO ()
gameLoop events stateVar done = do
    putMVar stateVar $ GameState 0

    fix $ \loop -> do
        e <- readChan events
        case e of
            Left Shutdown    -> putStrLn "shutting down game loop"
            Left (EndStep t) -> do
                modifyMVar_ stateVar . const . return . GameState $ t
                loop
            _ -> do
                loop

    putMVar done ()


windowAndRenderer :: String -> CInt -> CInt -> Word32 -> Word32
                  -> IO (Either String (Window, Renderer))
windowAndRenderer title width height wFlags rFlags =
    withCString title $ \name -> runEitherT $ do
        w <- nullCheck $ createWindow
                         name
                         SDL_WINDOWPOS_CENTERED
                         SDL_WINDOWPOS_CENTERED
                         width
                         height
                         wFlags

        r <- nullCheck $ createRenderer w (-1) rFlags

        return (w, r)


nullCheck :: IO (Ptr a) -> EitherT String IO (Ptr a)
nullCheck a = liftIO a >>= \r ->
    if r == nullPtr then do
        err <- getError
        msg <- liftIO $ peekCString err
        clearError
        left msg
    else return r


intervalPoints :: Word32 -> [Word32]
intervalPoints dy = 0 : loop 0 0
  where
    loop x d = case (1000 + d) `quotRem` dy of
        (q, r) -> let !x' = x + q in x' : loop x' r

