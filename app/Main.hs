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
frameBorder = 2

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
        extraMod :: XModifier,
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
        extraMod = XModControl,
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

newtype State = State {
        layout :: Maybe (LeafSelect XFrame)
    }

initializeState :: State
initializeState = State { layout = Nothing }

addFrame :: XFrame -> XWM State ()
addFrame f = modifyAppState $ \ s -> s {
        layout = Just $ case layout s of
            Just l -> insertLeaf f l
            Nothing -> createLeafSelect f
    }

getFrames :: XWindow -> XWM State [XFrame]
getFrames window = do
    previous <- getAppState layout
    pure $ case previous of
        Just lo -> filter
            (isFraming window)
            (getLeaves (intoTree $ includeTreeSelect lo))
        Nothing -> []

-- should only be one but who knows
removeFrames :: XWindow -> XWM State [XFrame]
removeFrames window = do
    -- theoretically we could return removed frames in removeLeaf
    -- or do something fancier with effects
    -- ...but instead we do two steps
    -- 1. look for ones we removed to return them
    removed <- getFrames window
    -- 2. actually remove the frames from the state
    bindLayout (removeLeaf (isFraming window))
    pure removed

applyLayout :: XWM State ()
applyLayout = do
    l <- getAppState layout
    case l of
        Just lo -> do
            width <- getRootWidth
            height <- getRootHeight
            let actions = useLayout
                    0 0
                    width height
                    drawFrame
                    (intoTree $ includeTreeSelect lo)
            sequenceA_ actions
            restyle selectedFrameStyle (getSelected lo)
            giveInputFocus (child $ getSelected lo)
        Nothing -> takeInputFocus
    where
        drawFrame x y w h f = do
            applyFrameLayout x y (w - 2 * frameBorder) (h - 2 * frameBorder) f
            restyle defaultFrameStyle f

mapLayout :: (LeafSelect XFrame -> LeafSelect XFrame) -> XWM State ()
mapLayout f = modifyAppState $ \ s -> s {
        layout = f <$> layout s
    }

bindLayout :: (LeafSelect XFrame -> Maybe (LeafSelect XFrame)) -> XWM State ()
bindLayout f = modifyAppState $ \ s -> s {
        layout = layout s >>= f
    }

handleEvent :: XEvent -> XWM State ()

handleEvent (KeyDown key) = case handleKey key handler of
    Just a -> a
    Nothing -> pure ()
    where
        act = (>> applyLayout) . mapLayout
        primary = layoutMod keybinds
        secondary = extraMod keybinds

        handler hasMod k
            | hasMod primary && hasMod secondary = handleBoth k
            | hasMod primary = handleLayout k
            | otherwise = Nothing

        handleLayout x
            -- honestly i don't know how i flipped V and H
            -- i might rethink this
            | x == vertical keybinds = Just $ act (rotate V)
            | x == horizontal keybinds = Just $ act (rotate H)
            | x == left keybinds = Just $ act (changeFocus L)
            | x == right keybinds = Just $ act (changeFocus R)
            | x == up keybinds = Just $ act (changeFocus U)
            | x == down keybinds = Just $ act (changeFocus D)
            -- we will have to save this process and gracefully kill it
            | x == terminal keybinds = Just $ liftIO openTerminal
            | otherwise = Nothing

        handleBoth x
            | x == left keybinds = Just $ act (movePane L)
            | x == right keybinds = Just $ act (movePane R)
            | x == up keybinds = Just $ act (movePane U)
            | x == down keybinds = Just $ act (movePane D)
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
            (terminal keybinds, [layoutMod keybinds]),

            (left keybinds, [layoutMod keybinds, extraMod keybinds]),
            (right keybinds, [layoutMod keybinds, extraMod keybinds]),
            (up keybinds, [layoutMod keybinds, extraMod keybinds]),
            (down keybinds, [layoutMod keybinds, extraMod keybinds])
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
    _ <- runXWM (listen handleEvent) d initializeState
    pure ()
