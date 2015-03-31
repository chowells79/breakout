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

data Status = Continue | Stop

main :: IO ()
main = do
    init $ SDL_INIT_VIDEO .|. SDL_INIT_TIMER
    res <- windowAndRenderer "Game" 640 480 0 0
    let (w, r) = either error id res

    sem <- newEmptyMVar
    forkIO $ renderLoop sem r

    mainLoop 60 sem

    putMVar sem Stop

    destroyWindow w
    quit


mainLoop :: Word32 -> MVar Status -> IO ()
mainLoop frameRate sem = do
    let intervals = intervalPoints frameRate
    evtP <- malloc
    flip fix intervals $ \wait all@(next:rest) -> do
        fix $ \moreEvents -> do
            stat <- pollEvent evtP
            case stat of
                1 -> do
                    evt <- peek evtP
                    case evt of
                        QuitEvent _ _ -> return ()
                        _             -> moreEvents
                0 -> do
                    t <- getTicks
                    case t >= next of
                        True -> do
                            tryPutMVar sem Continue
                            wait rest
                        False -> do
                            threadDelay 100
                            wait all


renderLoop :: MVar Status -> Renderer -> IO ()
renderLoop sem r = fix $ \loop -> do
    t <- getTicks
    let c = floor (128 - 128 * cos (fromIntegral t / 1000))
    setRenderDrawColor r c c c c
    renderClear r
    renderPresent r

    stat <- takeMVar sem
    case stat of
        Continue -> loop
        Stop -> return ()


windowAndRenderer :: String -> CInt -> CInt -> Word32 -> Word32 ->
                     IO (Either String (Window, Renderer))
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

