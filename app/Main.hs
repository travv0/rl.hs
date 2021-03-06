{-# LANGUAGE OverloadedStrings #-}

module Main where

import Apecs (runWith)
import Lib
import qualified SDL

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        now <- SDL.time
        appLoop now now

appLoop :: Double -> Double -> System' ()
appLoop previousTime totalTime = do
    events <- SDL.pollEvents
    mapM_ handleEvent events
    let dT = totalTime - previousTime
    step dT
    newTotalTime <- SDL.time
    appLoop totalTime newTotalTime
