{-# LANGUAGE BangPatterns #-}

import Graphics.UI.SDL

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String

import Data.Function

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Data.Bits

import Control.Concurrent

import Debug.Trace

import Prelude hiding (init)

data SimStep = EndStep Word32 | Shutdown deriving Show

main :: IO ()
main = do
    init $ SDL_INIT_VIDEO .|. SDL_INIT_TIMER

    surface <- newEmptyMVar
    events <- newChan
    gameLoopDone <- newEmptyMVar

    forkIO $ gameLoop events surface gameLoopDone
    mainLoop 60 surface 60 events

    takeMVar gameLoopDone

    quit


mainLoop :: Word32
         -> MVar (Ptr Surface)
         -> Word32
         -> Chan (Either SimStep Event)
         -> IO ()
mainLoop frameRate surface simRate events = alloca $ \evtP -> do
    res <- window "Breakout" 640 480 0
    let w = either error id res

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
                        _             -> do
                            writeChan events $ Right evt
                            moreEvents
                0 -> do
                    t <- getTicks
                    case (t >= f, t>= s) of
                        (_, True) -> do
                            writeChan events . Left . EndStep $ t
                            wait (f:fs, ss)
                        (True, _) -> do
                            renderSurface w surface
                            wait (dropWhile (<= t) fs, s:ss)
                        _ -> do
                            threadDelay 100
                            wait (f:fs, s:ss)

    destroyWindow w


renderSurface :: Window -> MVar (Ptr Surface) -> IO ()
renderSurface w surface = do
    t1 <- getTicks
    withMVar surface $ \surP -> do
        dest <- getWindowSurface w
        blitSurface surP nullPtr dest nullPtr
        updateWindowSurface w
    t2 <- getTicks
    print ("render", t2 - t1)


gameLoop :: Chan (Either SimStep Event)
         -> MVar (Ptr Surface)
         -> MVar ()
         -> IO ()
gameLoop events surface done = do
    let create = createRGBSurface 0 640 480 32 0xFF000000
                                               0x00FF0000
                                               0x0000FF00
                                               0x000000FF
    surP <- create
    fillRect surP nullPtr 0
    putMVar surface surP

    fix $ \loop -> do
        e <- readChan events
        case e of
            Left Shutdown    -> putStrLn "shutting down game loop"
            Left (EndStep t) -> do
                t1 <- getTicks
                old <- takeMVar surface
                freeSurface old
                new <- create
                fillRect new nullPtr (t * 256 + 255)
                putMVar surface new
                t2 <- getTicks
                print ("draw", t2 - t1)
                loop
            _                -> loop

    putMVar done ()


window :: String -> CInt -> CInt -> Word32 -> IO (Either String Window)
window title width height wFlags =
    withCString title $ \name -> runEitherT $ do
        nullCheck $ createWindow
                    name
                    SDL_WINDOWPOS_CENTERED
                    SDL_WINDOWPOS_CENTERED
                    width
                    height
                    wFlags


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

