{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void, when, unless)
import Data.Foldable (sequenceA_)
import qualified Data.Map as M

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
        terminal :: XKey,
        workspaces :: [(XKey, Int)]
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
        terminal = XKeyEnter,
        workspaces = [
            (XKey1, 1),
            (XKey2, 2),
            (XKey3, 3),
            (XKey4, 4),
            (XKey5, 5),
            (XKey6, 6),
            (XKey7, 7),
            (XKey8, 8),
            (XKey9, 9),
            (XKey0, 10)
        ]
    }

openTerminal :: IO ()
openTerminal = void $ startProcess "xterm"

-- logic

type Layout = LeafSelect XFrame

data State = State {
        layouts :: M.Map Int Layout,
        workspace :: Int
    }

initializeState :: State
initializeState = State {
        layouts = M.empty,
        workspace = 1
    }

getCurrentLayout :: XWM State (Maybe Layout)
getCurrentLayout = do
    l <- getAppState layouts
    w <- getAppState workspace
    pure $ M.lookup w l

adjustLayout :: (Layout -> Layout) -> XWM State ()
adjustLayout f = modifyAppState $ \ s -> s {
        layouts = M.adjust f (workspace s) $ layouts s
    }

updateLayout :: (Layout -> Maybe Layout) -> XWM State ()
updateLayout f = modifyAppState $ \ s -> s {
        layouts = M.update f (workspace s) $ layouts s
    }

alterLayout :: (Maybe Layout -> Maybe Layout) -> XWM State ()
alterLayout f = modifyAppState $ \ s -> s {
        layouts = M.alter f (workspace s) $ layouts s
    }

mapLayouts :: (Layout -> Layout) -> XWM State ()
mapLayouts f = modifyAppState $ \ s -> s {
        layouts = f <$> layouts s
    }

doLayouts :: (Layout -> XWM State x) -> XWM State [x]
doLayouts f = do
    l <- getAppState layouts
    traverse f $ M.elems l

mapMaybeLayouts :: (Layout -> Maybe Layout) -> XWM State ()
mapMaybeLayouts f = modifyAppState $ \ s -> s {
        layouts = M.mapMaybe f $ layouts s
    }

setWorkspace :: Int -> XWM State ()
setWorkspace w = modifyAppState $ \s -> s {
        workspace = w
    }

-- adds frame to current layout
addFrame :: XFrame -> XWM State ()
addFrame f = alterLayout $ \case
    Just l -> Just $ insertLeaf f l
    Nothing -> Just $ createLeafSelect f

-- gets frames in all layouts
getFrames :: XWindow -> XWM State [XFrame]
getFrames window = do
    previous <- getAppState layouts
    pure $ do
        lo <- M.elems previous
        filter
            (isFraming window)
            (getLeaves (intoTree $ includeTreeSelect lo))

-- should only be one but who knows
-- note that this checks all layouts, and removes from all layouts
removeFrames :: XWindow -> XWM State [XFrame]
removeFrames window = do
    -- theoretically we could return removed frames in removeLeaf
    -- or do something fancier with effects
    -- ...but instead we do two steps
    -- 1. look for ones we removed to return them
    removed <- getFrames window
    -- 2. actually remove the frames from the state
    mapMaybeLayouts (removeLeaf (isFraming window))
    pure removed

applyLayout :: XWM State ()
applyLayout = do
    -- TODO: how do we abstract this Layout -> [XFrame]
    _ <- doLayouts (mapM hide . (getLeaves . intoTree . includeTreeSelect))
    l <- getCurrentLayout
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
            reveal f
            applyFrameLayout x y (w - 2 * frameBorder) (h - 2 * frameBorder) f
            restyle defaultFrameStyle f

handleEvent :: XEvent -> XWM State ()

handleEvent (KeyDown key) = case handleKey key handler of
    Just a -> a
    Nothing -> pure ()
    where
        act = (>> applyLayout) . adjustLayout
        switch = (>> applyLayout) . setWorkspace
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
            | Just w <- lookup x (workspaces keybinds) = Just $ switch w
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

    -- if nothing gets unmapped, no need to apply layout
    -- this resolves nontermination issues, because we unmap frames to hide
    -- them when switching workspaces
    unless (null fs) applyLayout

main :: IO ()
main = do
    d <- initializeData
    _ <- runXWM (listen handleEvent) d initializeState
    pure ()
