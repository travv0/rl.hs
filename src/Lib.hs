{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Apecs (Component (Storage), Has (..), SystemT (SystemT), asks, explInit)
import qualified Apecs as A
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import SDL (($=), (*^))
import qualified SDL
import System.Exit (exitSuccess)

newtype GRenderer = GRenderer SDL.Renderer
instance Semigroup GRenderer where (<>) = const id
instance Monoid GRenderer where mempty = undefined
instance Component GRenderer where type Storage GRenderer = A.Global GRenderer

newtype GWindow = GWindow SDL.Window
instance Semigroup GWindow where (<>) = const id
instance Monoid GWindow where mempty = undefined
instance Component GWindow where type Storage GWindow = A.Global GWindow

data SpriteType = SpritePlayer
    deriving (Eq, Ord)

newtype GSpriteBank = GSpriteBank (Map SpriteType SDL.Texture)
instance Semigroup GSpriteBank where (<>) = (<>)
instance Monoid GSpriteBank where mempty = mempty
instance Component GSpriteBank where type Storage GSpriteBank = A.Global GSpriteBank

data Player = Player deriving (Show)
instance A.Component Player where type Storage Player = A.Unique Player

newtype Position = Position (SDL.V2 Double) deriving (Show)
instance A.Component Position where type Storage Position = A.Map Position

newtype Velocity = Velocity (SDL.V2 Double) deriving (Show)
instance A.Component Velocity where type Storage Velocity = A.Map Velocity

data MovingUp = MovingUp deriving (Show)
instance Component MovingUp where type Storage MovingUp = A.Unique MovingUp
data MovingRight = MovingRight deriving (Show)
instance Component MovingRight where type Storage MovingRight = A.Unique MovingRight
data MovingDown = MovingDown deriving (Show)
instance Component MovingDown where type Storage MovingDown = A.Unique MovingDown
data MovingLeft = MovingLeft deriving (Show)
instance Component MovingLeft where type Storage MovingLeft = A.Unique MovingLeft

newtype Sprite = Sprite SpriteType
instance Component Sprite where type Storage Sprite = A.Map Sprite

newtype Size = Size (SDL.V2 Int) deriving (Show)
instance Component Size where type Storage Size = A.Map Size

A.makeWorld
    "World"
    [ ''GRenderer
    , ''GWindow
    , ''GSpriteBank
    , ''Player
    , ''Position
    , ''Velocity
    , ''MovingUp
    , ''MovingRight
    , ''MovingDown
    , ''MovingLeft
    , ''Sprite
    , ''Size
    ]

type System' a = A.System World a

playerSpeed :: Double
playerSpeed = 400

makePlayer :: System' A.Entity
makePlayer =
    A.newEntity
        ( Player
        , Position 200
        , Velocity 0
        , Sprite SpritePlayer
        , Size 32
        )

initialize :: System' ()
initialize = do
    SDL.initializeAll

    window <-
        liftIO $
            SDL.createWindow
                "My SDL Application"
                SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 480 360}
    renderer <- liftIO $ SDL.createRenderer window (-1) SDL.defaultRenderer
    _windowEty <- A.newEntity (GWindow window)
    _rendererEty <- A.newEntity (GRenderer renderer)

    playerS <- liftIO $ SDL.loadBMP "media/megumin.bmp"
    player <- liftIO $ SDL.createTextureFromSurface renderer playerS
    _spriteBankEty <- A.newEntity (GSpriteBank $ Map.fromList [(SpritePlayer, player)])

    _player <- makePlayer

    return ()

stepPosition :: Double -> System' ()
stepPosition dT = A.cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

step :: Double -> System' ()
step dT = do
    liftIO $ print dT
    stepPlayerMovement
    stepPosition dT
    render

handleEvent :: SDL.Event -> System' ()
handleEvent event = case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
        case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
            SDL.KeycodeLeft -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> A.cmap $ \Player -> MovingLeft
                SDL.Released -> A.cmap $ \Player -> A.Not @MovingLeft
            SDL.KeycodeRight -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> A.cmap $ \Player -> MovingRight
                SDL.Released -> A.cmap $ \Player -> A.Not @MovingRight
            SDL.KeycodeUp -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> A.cmap $ \Player -> MovingUp
                SDL.Released -> A.cmap $ \Player -> A.Not @MovingUp
            SDL.KeycodeDown -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> A.cmap $ \Player -> MovingDown
                SDL.Released -> A.cmap $ \Player -> A.Not @MovingDown
            SDL.KeycodeEscape -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> liftIO exitSuccess
                _ -> return ()
            _ -> return ()
    _ -> return ()

stepPlayerMovement :: System' ()
stepPlayerMovement = do
    A.cmap $ \(Player, Velocity (SDL.V2 _ _)) -> Velocity 0

    A.cmap $ \(Player, MovingLeft, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x - playerSpeed) y)

    A.cmap $ \(Player, MovingRight, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x + playerSpeed) y)

    A.cmap $ \(Player, MovingUp, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y - playerSpeed))

    A.cmap $ \(Player, MovingDown, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y + playerSpeed))

render :: System' ()
render = do
    (GRenderer renderer) <- A.get A.global
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
    A.cmapM_ $ \(Size size, Position position, Sprite sprite) -> do
        (GSpriteBank spriteBank) <- A.get A.global
        let texture = spriteBank Map.! sprite
        SDL.copyEx
            renderer
            texture
            Nothing
            ( Just
                ( SDL.Rectangle
                    (SDL.P $ fmap round position)
                    (fmap fromIntegral size)
                )
            )
            0
            Nothing
            (SDL.V2 False False)
    SDL.present renderer
