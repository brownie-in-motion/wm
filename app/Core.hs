{-# LANGUAGE TupleSections #-}

module Core (
    WM (..),
    XWM,
    XData (..),
    XEvent (..),
    XFrameStyle (..),
    XWindow,
    XFrame,
    XKeyData,
    XKey (..),
    XModifier (..),
    XButtonData,
    applyWindowChanges,
    applyWindowLayout,
    applyFrameLayout,
    child,
    giveInputFocus,
    takeInputFocus,
    runWM,
    readData,
    getState,
    modifyState,
    getAppState,
    modifyAppState,
    runXWM,
    getRootHeight,
    getRootWidth,
    liftIO,
    listen,
    initializeData,
    mapWindow,
    reframe,
    restyle,
    unframe,
    isFraming,
    handleKey,
    grabKeys,
) where

import Control.Monad (forever, join)
import Data.Bits ((.|.), (.&.))
import Data.Bifunctor (first, second)
import Data.Foldable (for_, sequenceA_)
import Data.Word (Word64)
import Foreign.C.Types (CULong)
import qualified Graphics.X11 as X
import Graphics.X11.Xlib.Extras (
        Event (..),
        WindowAttributes (..),
        WindowChanges (..),
        configureWindow,
        getEvent,
        getWindowAttributes,
        unmapWindow,
    )

-- general reader + state + io effect

newtype WM d s a = WM (d -> s -> IO (s, a))

instance Functor (WM d s) where
    fmap :: (a -> b) -> WM d s a -> WM d s b
    fmap f (WM g) = WM $ \ d s -> (\ ~(ns, a) -> (ns, f a)) <$> g d s

instance Applicative (WM d s) where
    pure :: a -> WM d s a
    pure a = WM $ const $ pure . (, a)

    (<*>) :: WM d s (a -> b) -> WM d s a -> WM d s b
    (<*>) (WM f) (WM x) = WM inner
        where inner d s = do
                (ns, g) <- f d s
                (nns, a) <- x d ns
                pure (nns, g a)

instance Monad (WM d s) where
    (>>=) :: WM d s a -> (a -> WM d s b) -> WM d s b
    (>>=) (WM f) g = WM inner
        where inner d s = do
                (ns, a) <- f d s
                let (WM h) = g a in h d ns

runWM :: WM d s a -> d -> s -> IO (s, a)
runWM (WM f) = f

readData :: (d -> a) -> WM d s a
readData f = WM $ \ d s -> pure (s, f d)

getState :: (s -> a) -> WM d s a
getState f = WM $ \ _ s -> pure (s, f s)

modifyState :: (s -> s) -> WM d s ()
modifyState f = WM $ \ _ s -> pure (f s, ())

liftIO :: IO a -> WM d s a
liftIO io = WM $ \ _ s -> (s,) <$> io

-- utilities for x

selectInputs :: X.Display -> X.Window -> IO ()
selectInputs d w = X.selectInput d w $ foldl1 (.|.) [
        X.keyPressMask,
        X.keyReleaseMask,
        X.buttonPressMask,
        X.buttonReleaseMask,
        X.substructureRedirectMask,
        X.substructureNotifyMask
    ]

-- specific to how we understand x

data XData = XData {
        display :: X.Display,
        root :: X.Window
    }

initializeData :: IO XData
initializeData = do
    d <- X.openDisplay ""
    selectInputs d (X.defaultRootWindow d)
    pure $ XData { display = d, root = X.defaultRootWindow d }

dimensions :: XData -> IO (Int, Int)
dimensions (XData { display = d, root = r }) = do
    wa <- getWindowAttributes d r
    pure (fromIntegral $ wa_width wa, fromIntegral $ wa_height wa)

newtype XState = XState {
        time :: X.Time
    }

type XWM s a = WM XData (XState, s) a

getAppState :: (s -> a) -> XWM s a
getAppState f = f <$> getState snd

modifyAppState :: (s -> s) -> XWM s ()
modifyAppState f = modifyState (second f)

getTime :: XWM s X.Time
getTime = getState (time . fst)

setTime :: X.Time -> XWM s ()
setTime t = modifyState $ first $ const $ XState t

runXWM :: XWM s a -> XData -> s -> IO (s, a)
runXWM wm d s = do
    ((_, r), a) <- runWM wm d (XState 0, s)
    pure (r, a)

data XEvent
    = KeyDown XKeyData
    | KeyUp XKeyData
    | ButtonDown XButtonData
    | ButtonUp XButtonData
    | ConfigureRequest XWindow XWindowChanges
    | MapRequest XWindow
    | UnmapNotify XWindow

toEvent :: Event -> Maybe (XWM s XEvent)

toEvent (KeyEvent { ev_event_type = t, ev_keycode = c, ev_state = s })
    | t == X.keyPress = Just $ do
        b <- symbol
        pure $ KeyDown (XKeyData b s)
    | t == X.keyRelease = Just $ do
        b <- symbol
        pure $ KeyUp (XKeyData b s)
    | otherwise = Nothing
    where
        symbol :: XWM s X.KeySym
        symbol = do
            d <- readData display
            liftIO $ X.keycodeToKeysym d c 0

toEvent (ButtonEvent { ev_event_type = t, ev_button = b, ev_state = s })
    | t == X.buttonPress = Just $ pure $ ButtonDown (XButtonData b s)
    | t == X.buttonRelease = Just $ pure $ ButtonUp (XButtonData b s)
    | otherwise = Nothing

toEvent e@(ConfigureRequestEvent {}) = Just
    $ pure
    $ ConfigureRequest window changes
    where
        changes :: XWindowChanges
        changes = flip XWindowChanges (ev_value_mask e) $ WindowChanges {
            wc_x = ev_x e,
            wc_y = ev_y e,
            wc_width = ev_width e,
            wc_height = ev_height e,
            wc_border_width = ev_border_width e,
            wc_sibling = ev_above e,
            wc_stack_mode = ev_detail e
        }

        window = XWindow (ev_window e)

toEvent (MapRequestEvent { ev_window = w }) = Just
    $ pure
    $ MapRequest (XWindow w)

toEvent (UnmapEvent { ev_window = w }) = Just
    $ pure
    $ UnmapNotify (XWindow w)

toEvent _ = Nothing

getAttributes :: X.Window -> XWM s WindowAttributes
getAttributes w = do
    d <- readData display
    liftIO $ getWindowAttributes d w

getRootHeight :: XWM s Int
getRootHeight = snd <$> join (readData (liftIO . dimensions))

getRootWidth :: XWM s Int
getRootWidth = fst <$> join (readData (liftIO . dimensions))

getEventTime :: Event -> Maybe X.Time
getEventTime (KeyEvent { ev_time = t }) = Just t
getEventTime (ButtonEvent { ev_time = t }) = Just t
getEventTime (CrossingEvent { ev_time = t }) = Just t
getEventTime (SelectionRequest { ev_time = t }) = Just t
getEventTime (SelectionClear { ev_time = t }) = Just t
getEventTime (PropertyEvent { ev_time = t }) = Just t
getEventTime (ScreenSaverNotifyEvent { ev_time = t }) = Just t
getEventTime _ = Nothing

listen :: (XEvent -> XWM s ()) -> XWM s ()
listen f = do
    d <- readData display
    forever $ do
        e <- liftIO (nextEvent d)
        trySetTime e
        tryEvent f e
    where
        -- in theory we only have to allocate space for one event total
        -- but that makes this function quite a bit more gross
        nextEvent :: X.Display -> IO Event
        nextEvent d = X.allocaXEvent (liftA2 (*>) (X.nextEvent d) getEvent)

        trySetTime :: Event -> XWM s ()
        trySetTime e = for_ (getEventTime e) setTime

        tryEvent :: (XEvent -> XWM s ()) -> Event -> XWM s ()
        tryEvent g e = result *> liftIO (print e)
            where result = case toEvent e of
                    Just ev -> liftIO (putStr "handled: ") >> (ev >>= g)
                    Nothing -> liftIO (putStr "ignored: ")

data XWindowChanges = XWindowChanges WindowChanges CULong

newtype XWindow = XWindow X.Window deriving (Eq, Show)
data XFrame = XFrame X.Window XWindow deriving (Eq, Show)

child :: XFrame -> XWindow
child (XFrame _ w) = w

class XWindowLike a where
    toWindow :: a -> X.Window

    mapWindow :: a -> XWM s ()
    mapWindow a = do
        d <- readData display
        liftIO $ selectInputs d (toWindow a)
        liftIO $ X.mapWindow d (toWindow a)

applyWindowChanges :: XWindowLike a => XWindowChanges -> a -> XWM s ()
applyWindowChanges (XWindowChanges changes value_mask) wl = do
    d <- readData display
    liftIO $ configureWindow d (toWindow wl) value_mask changes

applyWindowLayout :: XWindowLike a => Int -> Int -> Int -> Int -> a -> XWM s ()
applyWindowLayout x y w h a = do
    d <- readData display
    liftIO $ X.moveResizeWindow
        d
        (toWindow a)
        (fromIntegral x)
        (fromIntegral y)
        (fromIntegral w)
        (fromIntegral h)

applyFrameLayout :: Int -> Int -> Int -> Int -> XFrame -> XWM s()
applyFrameLayout x y w h fr@(XFrame _ wi) = do
    d <- readData display

    -- move AND resize the *frame*
    liftIO $ X.moveResizeWindow
        d
        (toWindow fr)
        (fromIntegral x)
        (fromIntegral y)
        (fromIntegral w)
        (fromIntegral h)

    -- only resize the *window*
    liftIO $ X.moveResizeWindow
        d
        (toWindow wi)
        0 0
        (fromIntegral w)
        (fromIntegral h)

instance XWindowLike XWindow where
    toWindow (XWindow w) = w

instance XWindowLike XFrame where
    toWindow (XFrame w _) = w

data XFrameStyle = XFrameStyle {
        border :: Int,
        borderColor :: Word64,
        backgroundColor :: Word64
    }

-- windows by default are unmapped
-- reparent will map the inner window and reparent
reframe :: XFrameStyle -> XWindow -> XWM s XFrame
reframe style w@(XWindow x) = do
    d <- readData display
    r <- readData root
    attrs <- getAttributes x
    frame <- liftIO $ do
        frame <- X.createSimpleWindow
            d
            r
            (fromIntegral $ wa_x attrs)
            (fromIntegral $ wa_y attrs)
            (fromIntegral $ wa_width attrs)
            (fromIntegral $ wa_height attrs)
            (fromIntegral $ border style)
            (borderColor style)
            (backgroundColor style)
        X.addToSaveSet d x
        X.reparentWindow d x frame 0 0
        pure frame
    mapWindow w
    pure $ XFrame frame w

restyle :: XFrameStyle -> XFrame -> XWM s ()
restyle style frame = do
    d <- readData display
    liftIO $ do
        X.setWindowBorderWidth d (toWindow frame) (fromIntegral $ border style)
        X.setWindowBorder d (toWindow frame) (borderColor style)
        X.setWindowBackground d (toWindow frame) (backgroundColor style)

unframe :: XFrame -> XWM s XWindow
unframe (XFrame f x) = do
    d <- readData display
    liftIO $ do
        unmapWindow d f
        X.destroyWindow d f
    pure x

isFraming :: XWindow -> XFrame -> Bool
isFraming x (XFrame _ w) = w == x

data XKeyData = XKeyData X.KeySym X.Modifier
data XButtonData = XButtonData X.Button X.KeyMask

data XKey
    = XKeyA
    | XKeyB
    | XKeyC
    | XKeyD
    | XKeyE
    | XKeyF
    | XKeyG
    | XKeyH
    | XKeyI
    | XKeyJ
    | XKeyK
    | XKeyL
    | XKeyM
    | XKeyN
    | XKeyO
    | XKeyP
    | XKeyQ
    | XKeyR
    | XKeyS
    | XKeyT
    | XKeyU
    | XKeyV
    | XKeyW
    | XKeyX
    | XKeyY
    | XKeyZ
    | XKeyLeft
    | XKeyRight
    | XKeyUp
    | XKeyDown
    | XKeyEnter
    deriving (Eq, Show)

data XModifier
    = XModShift
    | XModControl
    | XModMod1
    | XModMod2
    | XModMod3
    | XModMod4
    | XModMod5
    deriving (Eq, Show)

-- this is wrong, fix later
-- my headcannon is that these are layout specific
-- and that i can make X11 give me layout-aware keycodes
-- looking into it later
toXKey :: X.KeySym -> Maybe XKey
toXKey code
    | code == X.xK_a = Just XKeyA
    | code == X.xK_b = Just XKeyB
    | code == X.xK_c = Just XKeyC
    | code == X.xK_d = Just XKeyD
    | code == X.xK_e = Just XKeyE
    | code == X.xK_f = Just XKeyF
    | code == X.xK_g = Just XKeyG
    | code == X.xK_h = Just XKeyH
    | code == X.xK_i = Just XKeyI
    | code == X.xK_j = Just XKeyJ
    | code == X.xK_k = Just XKeyK
    | code == X.xK_l = Just XKeyL
    | code == X.xK_m = Just XKeyM
    | code == X.xK_n = Just XKeyN
    | code == X.xK_o = Just XKeyO
    | code == X.xK_p = Just XKeyP
    | code == X.xK_q = Just XKeyQ
    | code == X.xK_r = Just XKeyR
    | code == X.xK_s = Just XKeyS
    | code == X.xK_t = Just XKeyT
    | code == X.xK_u = Just XKeyU
    | code == X.xK_v = Just XKeyV
    | code == X.xK_w = Just XKeyW
    | code == X.xK_x = Just XKeyX
    | code == X.xK_y = Just XKeyY
    | code == X.xK_z = Just XKeyZ
    | code == X.xK_Left = Just XKeyLeft
    | code == X.xK_Right = Just XKeyRight
    | code == X.xK_Up = Just XKeyUp
    | code == X.xK_Down = Just XKeyDown
    | code == X.xK_Return = Just XKeyEnter
    | otherwise = Nothing

fromXKey :: XKey -> X.KeySym
fromXKey XKeyA = X.xK_a
fromXKey XKeyB = X.xK_b
fromXKey XKeyC = X.xK_c
fromXKey XKeyD = X.xK_d
fromXKey XKeyE = X.xK_e
fromXKey XKeyF = X.xK_f
fromXKey XKeyG = X.xK_g
fromXKey XKeyH = X.xK_h
fromXKey XKeyI = X.xK_i
fromXKey XKeyJ = X.xK_j
fromXKey XKeyK = X.xK_k
fromXKey XKeyL = X.xK_l
fromXKey XKeyM = X.xK_m
fromXKey XKeyN = X.xK_n
fromXKey XKeyO = X.xK_o
fromXKey XKeyP = X.xK_p
fromXKey XKeyQ = X.xK_q
fromXKey XKeyR = X.xK_r
fromXKey XKeyS = X.xK_s
fromXKey XKeyT = X.xK_t
fromXKey XKeyU = X.xK_u
fromXKey XKeyV = X.xK_v
fromXKey XKeyW = X.xK_w
fromXKey XKeyX = X.xK_x
fromXKey XKeyY = X.xK_y
fromXKey XKeyZ = X.xK_z
fromXKey XKeyLeft = X.xK_Left
fromXKey XKeyRight = X.xK_Right
fromXKey XKeyUp = X.xK_Up
fromXKey XKeyDown = X.xK_Down
fromXKey XKeyEnter = X.xK_Return

fromXModifier :: XModifier -> X.KeyMask
fromXModifier XModShift = X.shiftMask
fromXModifier XModControl = X.controlMask
fromXModifier XModMod1 = X.mod1Mask
fromXModifier XModMod2 = X.mod2Mask
fromXModifier XModMod3 = X.mod3Mask
fromXModifier XModMod4 = X.mod4Mask
fromXModifier XModMod5 = X.mod5Mask

fromXModifiers :: [XModifier] -> X.KeyMask
fromXModifiers = foldl1 (.|.) . map fromXModifier

hasModifier :: X.Modifier -> XModifier -> Bool
hasModifier m XModShift = m .&. X.shiftMask /= 0
hasModifier m XModControl = m .&. X.controlMask /= 0
hasModifier m XModMod1 = m .&. X.mod1Mask /= 0
hasModifier m XModMod2 = m .&. X.mod2Mask /= 0
hasModifier m XModMod3 = m .&. X.mod3Mask /= 0
hasModifier m XModMod4 = m .&. X.mod4Mask /= 0
hasModifier m XModMod5 = m .&. X.mod5Mask /= 0

handleKey :: XKeyData -> ((XModifier -> Bool) -> XKey -> Maybe a) -> Maybe a
handleKey (XKeyData code m) f = f (hasModifier m) =<< toXKey code

grabKeys :: XWindowLike a => [(XKey, [XModifier])] -> a -> XWM s ()
grabKeys keys window = do
    d <- readData display
    let w = toWindow window
    let grabOne k ms = do
            code <- X.keysymToKeycode d (fromXKey k)
            X.grabKey
                d
                code
                (fromXModifiers ms)
                w
                False
                X.grabModeAsync
                X.grabModeAsync
    let actions = map (uncurry grabOne) keys
    liftIO $ sequenceA_ actions

giveInputFocus :: XWindowLike a => a -> XWM s ()
giveInputFocus w = do
    d <- readData display
    t <- getTime
    liftIO $ X.setInputFocus d (toWindow w) X.revertToParent t
 
    -- plenty of spec compliance to be done here
    -- for example:
    -- - messaging the children with TAKE_FOCUS, if they support it
    -- - setting children WM state
    -- - setting root WM state

takeInputFocus :: XWM s ()
takeInputFocus = do
    d <- readData display
    r <- readData root
    t <- getTime
    liftIO $ X.setInputFocus d r X.revertToParent t
