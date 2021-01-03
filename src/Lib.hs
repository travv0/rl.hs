{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Apecs (Component (Storage), Has (..), SystemT (SystemT), asks, explInit)
import qualified Apecs as A
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import SDL (($=), (^*), (^/))
import qualified SDL
import System.Exit (exitSuccess)

-- | global SDL renderer
newtype GRenderer = GRenderer SDL.Renderer

instance Semigroup GRenderer where (<>) = const id
instance Monoid GRenderer where mempty = undefined
instance Component GRenderer where type Storage GRenderer = A.Global GRenderer

-- | global SDL window
newtype GWindow = GWindow SDL.Window

instance Semigroup GWindow where (<>) = const id
instance Monoid GWindow where mempty = undefined
instance Component GWindow where type Storage GWindow = A.Global GWindow

-- | a datatype containing all available sprites in sprite bank
data SpriteType = SpritePlayer
    deriving (Eq, Ord)

-- | global sprite bank for accessing sprite textures
newtype GSpriteBank = GSpriteBank (Map SpriteType SDL.Texture)

instance Semigroup GSpriteBank where (<>) = (<>)
instance Monoid GSpriteBank where mempty = mempty
instance Component GSpriteBank where type Storage GSpriteBank = A.Global GSpriteBank

-- | countdown until move animations are done and player has control again
newtype GMoveTime = GMoveTime Double

instance Semigroup GMoveTime where (<>) = (<>)
instance Monoid GMoveTime where mempty = mempty
instance Component GMoveTime where type Storage GMoveTime = A.Global GMoveTime

-- | marks the player entity
data Player = Player deriving (Show)

instance A.Component Player where type Storage Player = A.Unique Player

{- | current position, stores both actual position on the grid and
rendered position for smoothly transitioning from one grid position to another
-}
data Position
    = Position
        (SDL.V2 Int)
        -- ^ actual position
        (SDL.V2 Double)
        -- ^ display position
    deriving (Show)

instance A.Component Position where type Storage Position = A.Map Position

-- | entity's velocity
newtype Velocity = Velocity (SDL.V2 Int) deriving (Show)

instance A.Component Velocity where type Storage Velocity = A.Map Velocity

data MovingUp = MovingUp deriving (Show)
instance Component MovingUp where type Storage MovingUp = A.Unique MovingUp
data MovingRight = MovingRight deriving (Show)
instance Component MovingRight where type Storage MovingRight = A.Unique MovingRight
data MovingDown = MovingDown deriving (Show)
instance Component MovingDown where type Storage MovingDown = A.Unique MovingDown
data MovingLeft = MovingLeft deriving (Show)
instance Component MovingLeft where type Storage MovingLeft = A.Unique MovingLeft

-- | for visible entities
newtype Visible = Visible SpriteType

instance Component Visible where type Storage Visible = A.Map Visible

{- | entity size, scaled to grid. so a size of (V2 1 1) will take up
 one cell on the grid
-}
newtype Size = Size (SDL.V2 Int) deriving (Show)

instance Component Size where type Storage Size = A.Map Size

-- | for entities that can't be passed through by other solid entities
data Solid = Solid

instance Component Solid where type Storage Solid = A.Unique Solid

A.makeWorld
    "World"
    [ ''GRenderer
    , ''GWindow
    , ''GSpriteBank
    , ''GMoveTime
    , ''Player
    , ''Position
    , ''Velocity
    , ''MovingUp
    , ''MovingRight
    , ''MovingDown
    , ''MovingLeft
    , ''Visible
    , ''Solid
    , ''Size
    ]

type System' a = A.System World a

position :: SDL.V2 Int -> Position
position pos = Position pos $ fromIntegral <$> pos

makePlayer :: System' A.Entity
makePlayer =
    A.newEntity
        ( Player
        , position 5
        , Velocity 0
        , Visible SpritePlayer
        , Solid
        , Size 1
        )

makeWall :: System' A.Entity
makeWall = A.newEntity (position 7, Visible SpritePlayer, Solid, Size 1)

initialize :: System' ()
initialize = do
    SDL.initializeAll

    window <-
        SDL.createWindow
            "My SDL Application"
            SDL.defaultWindow{SDL.windowInitialSize = SDL.V2 480 360}
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    _windowEty <- A.newEntity (GWindow window)
    _rendererEty <- A.newEntity (GRenderer renderer)
    _moveTimeEty <- A.newEntity (GMoveTime 0)

    playerSprite <- SDL.loadBMP "media/megumin.bmp"
    playerTexture <- SDL.createTextureFromSurface renderer playerSprite
    _spriteBankEty <- A.newEntity (GSpriteBank $ Map.fromList [(SpritePlayer, playerTexture)])

    _player <- makePlayer
    _wall <- makeWall

    return ()

-- | how fast transition between positions should be, smaller number is faster
turnTime :: Double
turnTime = 0.2

stepPosition :: Double -> System' ()
stepPosition dT = A.cmapM $ \(Position p dp, Velocity v) -> do
    (GMoveTime moveTime) <- A.get A.global
    if
            | moveTime <= 0 && v /= 0 -> do
                solidAtNewPos <-
                    A.cfold
                        (\b (Position op _, Solid) -> b || p + v == op)
                        False
                if solidAtNewPos
                    then return $ Position p (fromIntegral <$> p)
                    else do
                        A.global A.$= GMoveTime turnTime
                        return $ Position (p + v) (fromIntegral <$> p)
            | moveTime > 0 -> do
                A.global A.$= GMoveTime (moveTime - dT)
                return $ Position p (dp + (fromIntegral <$> v) ^/ turnTime ^* dT)
            | otherwise -> return $ Position p (fromIntegral <$> p)

step :: Double -> System' ()
step dT = do
    (GMoveTime moveTime) <- A.get A.global
    when (moveTime <= 0) $ do
        stepPlayerMovement
    stepPosition dT
    render

handleEvent :: SDL.Event -> System' ()
handleEvent event = case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
        case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
            k | k == SDL.KeycodeLeft || k == SDL.KeycodeH -> moveLeft keyboardEvent
            k | k == SDL.KeycodeRight || k == SDL.KeycodeL -> moveRight keyboardEvent
            k | k == SDL.KeycodeUp || k == SDL.KeycodeK -> moveUp keyboardEvent
            k | k == SDL.KeycodeDown || k == SDL.KeycodeJ -> moveDown keyboardEvent
            SDL.KeycodeY -> moveUp keyboardEvent >> moveLeft keyboardEvent
            SDL.KeycodeU -> moveUp keyboardEvent >> moveRight keyboardEvent
            SDL.KeycodeB -> moveDown keyboardEvent >> moveLeft keyboardEvent
            SDL.KeycodeN -> moveDown keyboardEvent >> moveRight keyboardEvent
            SDL.KeycodeEscape -> case SDL.keyboardEventKeyMotion keyboardEvent of
                SDL.Pressed -> liftIO exitSuccess
                _ -> return ()
            _ -> return ()
    SDL.WindowClosedEvent _ -> liftIO exitSuccess
    _ -> return ()
  where
    moveLeft keyboardEvent =
        case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> A.cmap $ \Player -> MovingLeft
            SDL.Released -> A.cmap $ \Player -> A.Not @MovingLeft
    moveRight keyboardEvent =
        case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> A.cmap $ \Player -> MovingRight
            SDL.Released -> A.cmap $ \Player -> A.Not @MovingRight
    moveUp keyboardEvent =
        case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> A.cmap $ \Player -> MovingUp
            SDL.Released -> A.cmap $ \Player -> A.Not @MovingUp
    moveDown keyboardEvent =
        case SDL.keyboardEventKeyMotion keyboardEvent of
            SDL.Pressed -> A.cmap $ \Player -> MovingDown
            SDL.Released -> A.cmap $ \Player -> A.Not @MovingDown

stepPlayerMovement :: System' ()
stepPlayerMovement = do
    A.cmap $ \(Player, Velocity (SDL.V2 _ _)) -> Velocity 0

    A.cmap $ \(Player, MovingLeft, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x - 1) y)

    A.cmap $ \(Player, MovingRight, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x + 1) y)

    A.cmap $ \(Player, MovingUp, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y - 1))

    A.cmap $ \(Player, MovingDown, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y + 1))

-- how big each cell on the grid is in pixels
cellSize :: SDL.V2 Int
cellSize = 32

render :: System' ()
render = do
    (GRenderer renderer) <- A.get A.global
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
    A.cmapM_ $ \(Size size, Position _ displayPosition, Visible sprite) -> do
        (GSpriteBank spriteBank) <- A.get A.global
        let texture = spriteBank Map.! sprite
        SDL.copyEx
            renderer
            texture
            Nothing
            ( Just
                ( SDL.Rectangle
                    (SDL.P $ round <$> displayPosition * fmap fromIntegral cellSize)
                    (fromIntegral <$> size * cellSize)
                )
            )
            0
            Nothing
            (SDL.V2 False False)
    SDL.present renderer
