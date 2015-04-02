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

import System.Mem

import Prelude hiding (init)

data SimStep = EndStep Word32 | Shutdown deriving Show

main :: IO ()
main = do
    init $ SDL_INIT_VIDEO .|. SDL_INIT_TIMER

    surface <- newEmptyMVar
    events <- newChan
    gameLoopDone <- newEmptyMVar

    res <- window "Breakout" 640 480 0
    let w = either error id res

    pixelFmt <- peek . surfaceFormat =<< peek =<< getWindowSurface w

    forkIO $ gameLoop events surface gameLoopDone pixelFmt
    mainLoop w 60 surface 60 events

    destroyWindow w
    takeMVar gameLoopDone

    quit


mainLoop :: Window
         -> Word32
         -> MVar (Ptr Surface)
         -> Word32
         -> Chan (Either SimStep Event)
         -> IO ()
mainLoop w frameRate surface simRate events = alloca $ \evtP -> do
    let frames = intervalPoints frameRate
        sims = intervalPoints simRate
    dest <- getWindowSurface w

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
                            renderSurface dest w surface
                            wait (dropWhile (<= t) fs, s:ss)
                        _ -> do
                            threadDelay 100
                            performMajorGC
                            wait (f:fs, s:ss)


renderSurface :: Ptr Surface -> Window -> MVar (Ptr Surface) -> IO ()
renderSurface dest w surface = do
    t1 <- getTicks
    withMVar surface $ \surP -> do
        blitSurface surP nullPtr dest nullPtr
    t2 <- getTicks
    updateWindowSurface w
    t3 <- getTicks
    print ("render", t2 - t1, t3 - t2)


gameLoop :: Chan (Either SimStep Event)
         -> MVar (Ptr Surface)
         -> MVar ()
         -> PixelFormat
         -> IO ()
gameLoop events surface done fmt = do
    let create = createRGBSurface 0
                                  640
                                  480
                                  (fromIntegral $ pixelFormatBitsPerPixel fmt)
                                  (pixelFormatRMask fmt)
                                  (pixelFormatGMask fmt)
                                  (pixelFormatBMask fmt)
                                  (pixelFormatAMask fmt)
    surP <- create
    fillRect surP nullPtr 0
    putMVar surface surP

    fix $ \loop -> do
        e <- readChan events
        case e of
            Left Shutdown    -> putStrLn "shutting down game loop"
            Left (EndStep t) -> do
                t1 <- getTicks
                new <- create
                fillRect new nullPtr t
                setSurfaceBlendMode new SDL_BLENDMODE_NONE
                old <- takeMVar surface
                putMVar surface new
                freeSurface old
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

