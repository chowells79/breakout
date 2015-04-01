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
                            withMVar surface $ renderSurface r
                            wait (dropWhile (<= t) fs, s:ss)
                        _ -> do
                            threadDelay 100
                            wait (f:fs, s:ss)

    destroyRenderer r
    destroyWindow w


renderSurface :: Renderer -> Ptr Surface -> IO ()
renderSurface r surP = do
    t1 <- getTicks
    setRenderDrawColor r 0 0 0 0
    renderClear r
    t <- createTextureFromSurface r surP
    renderCopy r t nullPtr nullPtr
    destroyTexture t
    renderPresent r
    t2 <- getTicks
    print $ t2 - t1


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
                old <- takeMVar surface
                freeSurface old
                new <- create
                fillRect new nullPtr (t * 256 + 255)
                putMVar surface new
                loop
            _                -> loop

    putMVar done ()


-- renderLoop :: MVar Status -> Renderer -> IO ()
-- renderLoop sem r = fix $ \loop -> do
--     t <- getTicks
--     let c = floor (128 - 128 * cos (fromIntegral t / 1000))
--     setRenderDrawColor r c c c c
--     renderClear r
--     renderPresent r

--     stat <- takeMVar sem
--     case stat of
--         Continue -> loop
--         Stop -> return ()


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

