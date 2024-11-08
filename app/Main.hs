{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Foldable (sequenceA_)

import System.Process.Typed (startProcess)

import Core
import Layout

-- assumptions
-- keyboard layout never changes
-- otherwise we have to re-grab keys

-- config

frameBorder :: Int
frameBorder = 3

defaultFrameStyle :: XFrameStyle
defaultFrameStyle = XFrameStyle {
        border = frameBorder,
        borderColor = 0x000000,
        backgroundColor = 0x000000
    }

selectedFrameStyle :: XFrameStyle
selectedFrameStyle = XFrameStyle {
        border = frameBorder,
        borderColor = 0xff0000,
        backgroundColor = 0x000000
    }

data Binds = Binds {
        layoutMod :: XModifier,
        vertical :: XKey,
        horizontal :: XKey,
        left :: XKey,
        right :: XKey,
        up :: XKey,
        down :: XKey,
        terminal :: XKey
    }

keybinds :: Binds
keybinds = Binds {
        layoutMod = XModShift,
        vertical = XKeyV,
        horizontal = XKeyH,
        left = XKeyLeft,
        right = XKeyRight,
        up = XKeyUp,
        down = XKeyDown,
        terminal = XKeyEnter
    }

openTerminal :: IO ()
openTerminal = void $ startProcess "xterm"

-- logic

newtype XState = XState {
        layout :: Maybe (LeafSelect XFrame)
    }

initializeState :: XState
initializeState = XState { layout = Nothing }

addFrame :: XFrame -> XWM XState ()
addFrame f = modifyState $ \ s -> s {
        layout = Just $ case layout s of
            Just l -> insertLeaf f l
            Nothing -> createLeafSelect f
    }

getFrames :: XWindow -> XWM XState [XFrame]
getFrames window = do
    previous <- getState layout
    pure $ case previous of
        Just lo -> filter
            (isFraming window)
            (getLeaves (intoTree $ includeTreeSelect lo))
        Nothing -> []

-- should only be one but who knows
removeFrames :: XWindow -> XWM XState [XFrame]
removeFrames window = do
    -- theoretically we could return removed frames in removeLeaf
    -- or do something fancier with effects
    -- ...but instead we do two steps
    -- 1. look for ones we removed to return them
    removed <- getFrames window
    -- 2. actually remove the frames from the state
    bindLayout (removeLeaf (isFraming window))
    pure removed

applyLayout :: XWM XState ()
applyLayout = do
    l <- getState layout
    case l of
        Just lo -> do
            width <- getRootWidth
            height <- getRootHeight
            let actions = useLayout
                    0 0
                    (width - frameBorder * 2) (height - frameBorder * 2)
                    drawFrame
                    (intoTree $ includeTreeSelect lo)
            sequenceA_ actions
            restyle selectedFrameStyle (getSelected lo)
        Nothing -> pure ()
    where
        drawFrame x y w h f = do
            applyFrameLayout x y w h f
            restyle defaultFrameStyle f

mapLayout :: (LeafSelect XFrame -> LeafSelect XFrame) -> XWM XState ()
mapLayout f = modifyState $ \ s -> s {
        layout = f <$> layout s
    }

bindLayout :: (LeafSelect XFrame -> Maybe (LeafSelect XFrame)) -> XWM XState ()
bindLayout f = modifyState $ \ s -> s {
        layout = layout s >>= f
    }

handleEvent :: XEvent -> XWM XState ()

handleEvent (KeyDown key) = case handleKey key handler of
    Just a -> a
    Nothing -> pure ()
    where
        handler hasMod k
            | hasMod (layoutMod keybinds) = (>> applyLayout) <$> handleLayout k
            | otherwise = Nothing
        handleLayout x
            | x == vertical keybinds = Just $ mapLayout (rotate H)
            | x == horizontal keybinds = Just $ mapLayout (rotate V)
            | x == left keybinds = Just $ mapLayout (changeFocus L)
            | x == right keybinds = Just $ mapLayout (changeFocus R)
            | x == up keybinds = Just $ mapLayout (changeFocus U)
            | x == down keybinds = Just $ mapLayout (changeFocus D)
            -- we will have to save this process and gracefully kill it
            | x == terminal keybinds = Just $ liftIO openTerminal
            | otherwise = Nothing

handleEvent (KeyUp _) = liftIO $ putStrLn "keyup"
handleEvent (ButtonDown _) = liftIO $ putStrLn "buttondown"
handleEvent (ButtonUp _) = liftIO $ putStrLn "buttonup"
handleEvent (ConfigureRequest w c) = do
    applyWindowChanges c w
    _ <- mapM_ (applyWindowChanges c) <$> getFrames w
    applyLayout

-- when we get a new window, reframe it and save that to state
-- we also have to grab some keybinds
handleEvent (MapRequest window) = do
    frame <- reframe defaultFrameStyle window
    addFrame frame
    mapWindow frame
    grabKeys
        [
            (vertical keybinds, [layoutMod keybinds]),
            (horizontal keybinds, [layoutMod keybinds]),
            (left keybinds, [layoutMod keybinds]),
            (right keybinds, [layoutMod keybinds]),
            (up keybinds, [layoutMod keybinds]),
            (down keybinds, [layoutMod keybinds]),
            (terminal keybinds, [layoutMod keybinds])
        ]
        window
    applyLayout

-- when we lose a window, the frame has to be removed
handleEvent (UnmapNotify window) = do
    fs <- removeFrames window
    mapM_ unframe fs
    applyLayout

main :: IO ()
main = do
    d <- initializeData
    _ <- runWM (listen handleEvent) d initializeState
    pure ()
