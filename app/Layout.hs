module Layout where

import Data.Maybe (mapMaybe)

data NonEmpty a = NonEmpty a [a] deriving (Eq, Show)

singleton :: a -> NonEmpty a
singleton a = NonEmpty a []

prepend :: [a] -> NonEmpty a -> NonEmpty a
prepend [] (NonEmpty a as) = NonEmpty a as
prepend (a : as) (NonEmpty b bs) = NonEmpty a (as ++ b : bs)

append :: NonEmpty a -> [a] -> NonEmpty a
append (NonEmpty a as) bs = NonEmpty a (as ++ bs)

includeList :: NonEmpty a -> [a]
includeList (NonEmpty a as) = a : as

fromList :: [a] -> Maybe (NonEmpty a)
fromList (x : xs) = Just (NonEmpty x xs)
fromList [] = Nothing

instance Functor NonEmpty where
    fmap f (NonEmpty a as) = NonEmpty (f a) (fmap f as)

instance Foldable NonEmpty where
    foldMap f (NonEmpty a as) = foldMap f (a : as)

data Orientation = H | V deriving (Eq, Show)
data Direction = L | R | U | D deriving (Eq, Show)

data LeafData a = LeafData {
        leafSize :: Rational,
        leafValue :: a
    } deriving (Eq, Show)

-- note the looseness in our representation: you can have nested
-- singleton roots that logically represent the same layout
data RootData a = RootData {
        rootOrientation :: Orientation,
        rootSize :: Rational,
        rootChildren :: NonEmpty (SizedTree a)
    } deriving (Eq, Show)

data SizedTree a = Leaf (LeafData a) | Root (RootData a) deriving (Eq, Show)

data ImmediateContext a = ImmediateContext {
        contextOrientation :: Orientation,
        contextSize :: Rational,
        contextLeft :: [SizedTree a],
        contextRight :: [SizedTree a]
    } deriving (Eq, Show)

-- this is the guy who's actually exposed: we can only select leaves
data LeafSelect a = LeafSelect {
        leafSelectData :: LeafData a,
        leafSelectContext :: NonEmpty (ImmediateContext a)
    } deriving (Eq, Show)

-- this one is instrumental, because we sometimes have subtrees selected
data TreeSelect a = TreeSelect {
        treeSelect :: SizedTree a,
        treeSelectContext :: NonEmpty (ImmediateContext a)
    }

includeTreeSelect :: LeafSelect a -> TreeSelect a
includeTreeSelect ls = TreeSelect {
        treeSelect = Leaf $ leafSelectData ls,
        treeSelectContext = leafSelectContext ls
    }

createLeafSelect :: a -> LeafSelect a
createLeafSelect value = LeafSelect {
        leafSelectData = LeafData { leafSize = 1, leafValue = value },
        leafSelectContext = singleton ImmediateContext {
                contextOrientation = V,
                contextSize = 1,
                contextLeft = [],
                contextRight = []
            }
    }

insertLeaf :: a -> LeafSelect a -> LeafSelect a
insertLeaf value ls = LeafSelect {
        leafSelectData = LeafData {
                -- same proportion as the selected window
                leafSize = leafSize oldFocus,
                leafValue = value
            },
        leafSelectContext = NonEmpty
            -- move the selected window into the left
            hd { contextLeft = Leaf oldFocus : contextLeft hd }
            -- don't touch the windows on the right
            tl
    }
    where
        oldFocus = leafSelectData ls
        NonEmpty hd tl = leafSelectContext ls

getSelected :: LeafSelect a -> a
getSelected = leafValue . leafSelectData

rotate :: Orientation -> LeafSelect a -> LeafSelect a
rotate o ls = LeafSelect {
        -- add a root above the selected window with the same size
        leafSelectContext = prepend
            [
                ImmediateContext {
                        contextOrientation = o,
                        contextSize = leafSize $ leafSelectData ls,
                        contextLeft = [],
                        contextRight = []
                    }
            ]
            (leafSelectContext ls),

        -- set the current window to fill the entire root
        leafSelectData = (leafSelectData ls) { leafSize = 1 }
    }

buildRoot :: SizedTree a -> ImmediateContext a -> SizedTree a
buildRoot d c = Root $ RootData {
        rootOrientation = contextOrientation c,
        rootSize = contextSize c,
        rootChildren = prepend
            (reverse $ contextLeft c)
            (NonEmpty d (contextRight c))
    }

ascend :: TreeSelect a -> Either (SizedTree a) (TreeSelect a)
ascend ts = case treeSelectContext ts of
    NonEmpty c [] -> Left $ buildRoot (treeSelect ts) c
    NonEmpty c (nc : ncs) -> Right $ TreeSelect {
            treeSelect = buildRoot (treeSelect ts) c,
            treeSelectContext = NonEmpty nc ncs
        }

intoTree :: TreeSelect a -> SizedTree a
intoTree ls = case ascend ls of
    Left d -> d
    Right ts -> intoTree ts

size :: SizedTree a -> Rational
size (Leaf ld) = leafSize ld
size (Root rd) = rootSize rd

share :: Int -> NonEmpty Rational -> NonEmpty Int
share s rats = NonEmpty (hd + remainder) tl
    where
        total = sum rats
        scale x = x * fromIntegral s / total
        scaled@(NonEmpty hd tl) = round . scale <$> rats
        remainder = s - sum scaled

getLeaves :: SizedTree a -> [a]
getLeaves (Leaf ld) = pure (leafValue ld)
getLeaves (Root rd) = includeList (rootChildren rd) >>= getLeaves

-- given a function that needs a leaf's position and size, and a tree,
-- apply the function to all leaves in the tree
useLayout
    :: Int -> Int -> Int -> Int
    -> (Int -> Int -> Int -> Int -> a -> b)
    -> SizedTree a
    -> [b]
useLayout x y w h f (Leaf (LeafData _ a)) = [f x y w h a]
useLayout x y w h f (Root (RootData V _ cs)) = concat rec
    where
        heights = includeList $ share h $ size <$> cs
        ys = map (+ y) $ 0 : scanl1 (+) heights
        contexts = includeList cs
        rec = map (\ (nh, ny, cx) -> useLayout x ny w nh f cx) $ zip3
            heights
            ys
            contexts
useLayout x y w h f (Root (RootData H _ cs)) = concat rec
    where
        widths = includeList $ share w $ size <$> cs
        xs = map (+ x) $ 0 : scanl1 (+) widths
        contexts = includeList cs
        rec = map (\ (nw, nx, cx) -> useLayout nx y nw h f cx) $ zip3
            widths
            xs
            contexts

-- imagine we're looking at a hole now: suppose our focus was just deleted
-- try our best to focus on another leaf
-- of course, this is not always possible
trySelectTree :: NonEmpty (ImmediateContext a) -> Maybe (TreeSelect a)
trySelectTree (NonEmpty ctx ctxs) = case (contextLeft ctx, contextRight ctx) of
    -- arbitrary left priority
    (l : ls, _) -> Just $ TreeSelect {
            treeSelect = l,
            treeSelectContext = NonEmpty
                (ctx { contextLeft = ls })
                ctxs
        }
    (_, r : rs) -> Just $ TreeSelect {
            treeSelect = r,
            treeSelectContext = NonEmpty
                (ctx { contextRight = rs })
                ctxs
        }
    ([], []) -> fromList ctxs >>= trySelectTree

-- every finite tree has a leaf (cool story)
-- for now... we just pick the first leaf
pickLeaf :: TreeSelect a -> LeafSelect a
pickLeaf ts = case treeSelect ts of
    Leaf ld -> LeafSelect {
            leafSelectData = ld,
            leafSelectContext = treeSelectContext ts
        }
    Root rd -> let NonEmpty r rs = rootChildren rd in pickLeaf $ TreeSelect {
            treeSelect = r,
            treeSelectContext = flip prepend
                (treeSelectContext ts)
                $ pure ImmediateContext {
                            contextOrientation = rootOrientation rd,
                            contextSize = rootSize rd,
                            contextLeft = [],
                            contextRight = rs
                        }
        }

-- more generally we could take f : a -> Maybe b rather than f : a -> Bool
-- but currently there's no need for that

removeLeafFromTree :: (a -> Bool) -> SizedTree a -> Maybe (SizedTree a)
removeLeafFromTree f leaf@(Leaf (LeafData { leafValue = lv }))
    | f lv = Nothing
    | otherwise = Just leaf
removeLeafFromTree a (Root root@(RootData { rootChildren = rc })) =
    case mapMaybe (removeLeafFromTree a) (includeList rc) of
        x : xs -> Just $ Root $ root { rootChildren = NonEmpty x xs }
        [] -> Nothing

removeLeafFromContext
    :: (a -> Bool)
    -> ImmediateContext a
    -> ImmediateContext a
removeLeafFromContext f ctx = ctx {
        contextLeft = mapMaybe (removeLeafFromTree f) (contextLeft ctx),
        contextRight = mapMaybe (removeLeafFromTree f) (contextRight ctx)
    }

removeLeaf :: (a -> Bool) -> LeafSelect a -> Maybe (LeafSelect a)
removeLeaf f (LeafSelect { leafSelectData = ld, leafSelectContext = lc })
    -- our leaf matches, so remove it and select a new leaf
    -- then try it again
    | f (leafValue ld) = trySelectTree lc >>= removeLeaf f . pickLeaf

    -- in this case, we're done with the leaf for good. so we traverse up and
    -- remove stuff. observe that we never have to delete a context, because
    -- every context will be nonempty. this is witnessed by the fact that
    -- we made it into this branch, so our focus will definitely be kept
    | otherwise = Just $ LeafSelect {
            leafSelectData = ld,
            leafSelectContext = fmap (removeLeafFromContext f) lc
        }

changeFocusHelper :: Direction -> TreeSelect a -> Maybe (TreeSelect a)
changeFocusHelper d ts = case attemptMove of
    Just tree -> Just tree
    Nothing -> case ascend ts of
        Left _ -> Nothing
        Right up -> changeFocusHelper d up
    where
        allowedOrientation = case d of { L -> H; R -> H; U -> V; D -> V }
        attemptMove = let NonEmpty c _ = treeSelectContext ts in
            if contextOrientation c == allowedOrientation
                then
                    if d == L || d == U
                        then shiftLeft ts
                        else shiftRight ts
                else Nothing

shiftLeft :: TreeSelect a -> Maybe (TreeSelect a)
shiftLeft TreeSelect {
    treeSelect = focus,
    treeSelectContext = NonEmpty c cs
} = case contextLeft c of
    [] -> Nothing
    l : ls -> Just $ TreeSelect {
        treeSelect = l,
        treeSelectContext = NonEmpty
            (
                c {
                    contextLeft = ls,
                    contextRight = focus : contextRight c
                }
            )
            cs
    }

shiftRight :: TreeSelect a -> Maybe (TreeSelect a)
shiftRight TreeSelect {
    treeSelect = focus,
    treeSelectContext = NonEmpty c cs
} = case contextRight c of
    [] -> Nothing
    r : rs -> Just $ TreeSelect {
        treeSelect = r,
        treeSelectContext = NonEmpty
            (
                c {
                    contextLeft = focus : contextLeft c,
                    contextRight = rs
                }
            )
            cs
    }

-- give our best attempt to move the focus in the given direction
-- if we can't, just stay in place
changeFocus :: Direction -> LeafSelect a -> LeafSelect a
changeFocus d l = maybe l pickLeaf (changeFocusHelper d (includeTreeSelect l))
