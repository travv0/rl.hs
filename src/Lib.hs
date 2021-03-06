{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Apecs (Component (Storage), Has (..), SystemT (SystemT), asks, explInit)
import qualified Apecs as A
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Graph.AStar (aStarM)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Sum (Sum, getSum))
import SDL (($=), (^*), (^/))
import qualified SDL
import System.Exit (exitSuccess)

-- | the current state or phase of the game
data GameState
    = -- | animating position changes, etc
      Animating
    | -- | running game logic until player's next turn
      Playing
    | -- | waiting on player's action
      Waiting
    deriving (Eq)

-- | global game state (playing, paused, etc.)
newtype GState = GState GameState

instance Semigroup GState where (<>) = const id
instance Monoid GState where mempty = GState Waiting
instance Component GState where type Storage GState = A.Global GState

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

instance Semigroup GSpriteBank where
    GSpriteBank sb1 <> GSpriteBank sb2 = GSpriteBank $ sb1 <> sb2
instance Monoid GSpriteBank where mempty = GSpriteBank mempty
instance Component GSpriteBank where type Storage GSpriteBank = A.Global GSpriteBank

-- | countdown until move animations are done and player has control again
newtype GMoveTime = GMoveTime Double

instance Semigroup GMoveTime where
    GMoveTime mt1 <> GMoveTime mt2 = GMoveTime $ getSum $ Sum mt1 <> Sum mt2
instance Monoid GMoveTime where mempty = GMoveTime $ getSum (mempty :: Sum Double)
instance Component GMoveTime where type Storage GMoveTime = A.Global GMoveTime

-- | marks the player entity
data Player = Player deriving (Show)

instance A.Component Player where type Storage Player = A.Unique Player

-- | enemy's current state
data EnemyState
    = -- | won't move at all
      Sleeping
    | -- | following player
      Tracking
    deriving (Show)

-- | marks an enemy entity
newtype Enemy = Enemy EnemyState deriving (Show)

instance A.Component Enemy where type Storage Enemy = A.Map Enemy

-- | entity position
newtype Position = Position (SDL.V2 Int)
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
data Visible
    = Visible
        SpriteType
        -- ^ sprite used to display entity
        (SDL.V2 Double)
        -- ^ pixel position to draw entity at

instance Component Visible where type Storage Visible = A.Map Visible

{- | entity size, scaled to grid. so a size of (V2 1 1) will take up
 one cell on the grid
-}
newtype Size = Size (SDL.V2 Int) deriving (Show)

instance Component Size where type Storage Size = A.Map Size

-- | for entities that can't be passed through by other solid entities
data Solid = Solid

instance Component Solid where type Storage Solid = A.Map Solid

-- | ticks until entity can perform another action
newtype Cooldown = Cooldown Int deriving (Show)

instance Component Cooldown where type Storage Cooldown = A.Map Cooldown

-- | entity's health
newtype Health = Health Int deriving (Show)

instance Component Health where type Storage Health = A.Map Health

-- | weapon
data Weapon
    = Weapon
        Int
        -- ^ weapon's damage
        Int
        -- ^ weapon's cooldown
    deriving (Show)

instance Component Weapon where type Storage Weapon = A.Map Weapon

-- | entity is wielding another entity
newtype Wielding = Wielding A.Entity deriving (Show)

instance Component Wielding where type Storage Wielding = A.Map Wielding

data Action
    = MoveTo (SDL.V2 Int)
    | Attack (SDL.V2 Int)
    deriving (Show)

-- | actions to be animated
newtype Actions = Actions [Action] deriving (Show)

instance Component Actions where type Storage Actions = A.Map Actions

A.makeWorld
    "World"
    [ ''GState
    , ''GRenderer
    , ''GWindow
    , ''GSpriteBank
    , ''GMoveTime
    , ''Player
    , ''Enemy
    , ''Position
    , ''Velocity
    , ''MovingUp
    , ''MovingRight
    , ''MovingDown
    , ''MovingLeft
    , ''Visible
    , ''Solid
    , ''Size
    , ''Cooldown
    , ''Health
    , ''Weapon
    , ''Actions
    , ''Wielding
    ]

type System' a = A.System World a

type AllComponents =
    ( ( Player
      , Enemy
      , Position
      )
    , ( Velocity
      , MovingUp
      , MovingRight
      , MovingDown
      , MovingLeft
      , Visible
      , Solid
      , Size
      )
    , Cooldown
    , Health
    , Weapon
    , Actions
    , Wielding
    )

makePlayer :: SDL.V2 Int -> System' A.Entity
makePlayer pos = do
    weapon <- A.newEntity (Weapon 30 6)
    A.newEntity
        (
            ( Player
            , Position pos
            , Velocity 0
            , Visible SpritePlayer $ fromIntegral <$> pos
            , Solid
            , Size 1
            , Cooldown 0
            , Actions []
            )
        , Health 100
        , Wielding weapon
        )

makeEnemy :: SDL.V2 Int -> System' A.Entity
makeEnemy pos = do
    weapon <- A.newEntity (Weapon 20 5)
    A.newEntity
        (
            ( Enemy Sleeping
            , Position pos
            , Velocity 0
            , Visible SpritePlayer $ fromIntegral <$> pos
            , Solid
            , Size 1
            , Cooldown 0
            , Actions []
            )
        , Health 100
        , Wielding weapon
        )

makeWall :: SDL.V2 Int -> System' A.Entity
makeWall pos =
    A.newEntity
        ( Position pos
        , Visible SpritePlayer $ fromIntegral <$> pos
        , Solid
        , Size 1
        )

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

    playerSprite <- SDL.loadBMP "media/megumin.bmp"
    playerTexture <- SDL.createTextureFromSurface renderer playerSprite
    _spriteBankEty <- A.newEntity (GSpriteBank $ Map.fromList [(SpritePlayer, playerTexture)])

    player <- makePlayer 5
    liftIO $ putStrLn $ "player: " <> show player
    enemy <- makeEnemy 7
    liftIO $ putStrLn $ "enemy: " <> show enemy
    wall <- makeWall $ SDL.V2 3 7
    liftIO $ putStrLn $ "wall: " <> show wall

    return ()

-- | how fast transition between positions should be, smaller number is faster
turnTime :: Double
turnTime = 0.2

stepMovement :: System' ()
stepMovement = A.cmapM_ $ \(Position p, Velocity v, Cooldown c, Actions as, ety) -> do
    when (c == 0) $ do
        solidAtNewPos <-
            A.cfold
                (\b (Position op, Solid) -> b || p + v == op)
                False
        A.cmapM_ $ \Player -> do
            let newPos = if solidAtNewPos then p else p + v
            ety
                A.$= ( Position newPos
                     , Cooldown 5
                     , Actions $ if newPos /= p then as ++ [MoveTo newPos] else as
                     )

stepAttack :: System' ()
stepAttack = A.cmapM_ $ \(Position p, Velocity v, Cooldown c, Actions as, Wielding weapon, etyA) -> do
    isWeapon <- A.exists weapon $ A.Proxy @Weapon
    when (isWeapon && c == 0) $ do
        (Weapon damage cooldown) <- A.get weapon
        A.cmapM_
            ( \(Position op, Health h, etyD) ->
                when (v /= 0 && p + v == op) $ do
                    liftIO $ putStrLn $ show etyA <> " attacking " <> show etyD
                    etyD A.$= Health (h - damage)
                    etyA A.$= Actions (as ++ [Attack op])
                    etyA A.$= Cooldown cooldown
            )

stepHealth :: System' ()
stepHealth = A.cmapM_ $ \(Health h, ety) ->
    when (h <= 0) $ A.destroy ety $ A.Proxy @AllComponents

stepAnimateActions :: Double -> Double -> System' ()
stepAnimateActions moveTime dT = A.cmap $ \(Visible s vp, Position pos, Actions actions) ->
    let actionTurnTime = turnTime / fromIntegral (length actions)
        actionMoveTime = actionTurnTime - (turnTime - moveTime)
     in case actions of
            (action : restActions) ->
                if actionMoveTime > 0
                    then case action of
                        MoveTo p ->
                            ( Visible s (vp + (fmap fromIntegral p - vp) ^/ moveTime ^* dT)
                            , Actions actions
                            )
                        Attack p ->
                            ( Visible s (vp + (fmap fromIntegral p - vp) ^/ moveTime ^* dT ^/ 2)
                            , Actions actions
                            )
                    else
                        ( Visible s $
                            if null restActions
                                then fromIntegral <$> pos
                                else fromInteger . round <$> vp
                        , Actions restActions
                        )
            [] -> (Visible s (fromIntegral <$> pos), Actions [])

stepUpdateState :: GameState -> Double -> System' ()
stepUpdateState state moveTime = A.cmapM_ $ \(Player, Cooldown cooldown, Velocity v) ->
    case state of
        Waiting -> when (v /= 0) $ A.global A.$= GState Playing
        Playing -> when (cooldown == 0) $ do
            A.global A.$= GMoveTime turnTime
            A.global A.$= GState Animating
        Animating -> when (moveTime <= 0) $ A.global A.$= GState Waiting

stepCooldowns :: System' ()
stepCooldowns = A.cmap $ \(Cooldown c) ->
    if c > 0
        then Cooldown (c - 1)
        else Cooldown c

stepUpdateMoveTime :: Double -> Double -> System' ()
stepUpdateMoveTime moveTime dT = A.global A.$= GMoveTime (moveTime - dT)

step :: Double -> System' ()
step dT = do
    (GMoveTime moveTime) <- A.get A.global
    (GState state) <- A.get A.global
    case state of
        Waiting -> stepPlayerMovement
        Playing -> do
            stepEnemyState
            stepEnemyMovement
            stepAttack
            stepHealth
            stepMovement
            stepCooldowns
        Animating -> do
            stepAnimateActions moveTime dT
            stepUpdateMoveTime moveTime dT
    render
    stepUpdateState state moveTime

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
    A.cmap $ \(Player, Velocity _) -> Velocity 0
    A.cmap $ \(Player, MovingLeft, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x - 1) y)
    A.cmap $ \(Player, MovingRight, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 (x + 1) y)
    A.cmap $ \(Player, MovingUp, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y - 1))
    A.cmap $ \(Player, MovingDown, Velocity (SDL.V2 x y)) ->
        Velocity (SDL.V2 x (y + 1))

stepEnemyMovement :: System' ()
stepEnemyMovement = do
    A.cmapM $ \(Enemy enemyState, Velocity _, Position pos, ety) ->
        case enemyState of
            Sleeping -> ety A.$= Velocity 0
            Tracking -> moveTowardsPlayer ety pos

moveTowardsPlayer :: A.Entity -> SDL.V2 Int -> System' ()
moveTowardsPlayer ety pos =
    A.cmapM $ \(Player, Position playerPos) -> do
        path <-
            aStarM
                (neighbors playerPos)
                nodeDistance
                (heuristic playerPos)
                (return . (== playerPos))
                (return pos)
        case path of
            Just (newPos : _) -> ety A.$= Velocity (newPos - pos)
            _ -> ety A.$= Velocity 0

neighbors :: SDL.V2 Int -> SDL.V2 Int -> System' (HashSet (SDL.V2 Int))
neighbors goal pos = do
    let options = [pos + SDL.V2 dx dy | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]
    clearPositions <-
        filterM
            ( \newPos -> do
                solidAtNewPos <-
                    A.cfold
                        (\b (Position op, Solid) -> b || newPos == op)
                        False
                return $ newPos == goal || not solidAtNewPos
            )
            options
    return $ HS.fromList clearPositions

heuristic :: Monad m => SDL.V2 Int -> SDL.V2 Int -> m Double
heuristic goal curr = return $ distance curr goal

nodeDistance :: Monad m => SDL.V2 Int -> SDL.V2 Int -> m Double
nodeDistance from to = return $ distance from to

stepEnemyState :: System' ()
stepEnemyState = A.cmapM_ $ \(Enemy _, Position enemyPos, ety) ->
    A.cmapM_ $ \(Player, Position playerPos) ->
        when (distance enemyPos playerPos <= (2 :: Double)) $
            ety A.$= Enemy Tracking

distance :: (Floating a, Real b) => SDL.V2 b -> SDL.V2 b -> a
distance (SDL.V2 fromX fromY) (SDL.V2 toX toY) =
    sqrt
        ( (realToFrac (fromX - toX) ^ (2 :: Integer))
            + (realToFrac (fromY - toY) ^ (2 :: Integer))
        )

-- how big each cell on the grid is in pixels
cellSize :: SDL.V2 Int
cellSize = 32

render :: System' ()
render = do
    (GRenderer renderer) <- A.get A.global
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
    A.cmapM_ $ \(Size size, Visible sprite displayPosition) -> do
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
