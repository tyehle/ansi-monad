{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.ANSI.Monad where

import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Sequence (Seq, Seq(..), (><))
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import System.Console.ANSI.Types


data SGRState = SGRState
  { bold :: Bool
  , faint :: Bool
  , italicized :: Bool
  , underlining :: Underlining
  , blinkSpeed :: BlinkSpeed
  , visible :: Bool
  , inverted :: Bool
  , foreground :: Maybe ANSIColor
  , background :: Maybe ANSIColor
  }
  deriving (Show, Read, Eq, Ord)


data ANSIColor = Color ColorIntensity Color
               | RGBColor (Colour Float)
               | PaletteColor Word8
               deriving (Show, Read, Eq)

instance Ord ANSIColor where
  compare (Color i1 c1) (Color i2 c2) = compare (i1, c1) (i2, c2)
  compare (Color _ _) _ = LT
  compare _ (Color _ _) = GT
  compare (RGBColor c1) (RGBColor c2) = comparing ((\(RGB r g b) -> (r, g, b)) . toSRGB24) c1 c2
  compare (RGBColor _) _ = LT
  compare _ (RGBColor _) = GT
  compare (PaletteColor w1) (PaletteColor w2) = compare w1 w2


defaultState :: SGRState
defaultState = SGRState
  { bold = False
  , faint = False
  , italicized = False
  , underlining = NoUnderline
  , blinkSpeed = SlowBlink
  , visible = True
  , inverted = False
  , foreground = Nothing
  , background = Nothing
  }


applySGR :: SGR -> SGRState -> SGRState
applySGR Reset _ = defaultState
applySGR (SetConsoleIntensity BoldIntensity) s = s{bold=True}
applySGR (SetConsoleIntensity FaintIntensity) s = s{faint=True}
applySGR (SetConsoleIntensity NormalIntensity) s = s{bold=False, faint=False}
applySGR (SetItalicized i) s = s{italicized=i}
applySGR (SetUnderlining u) s = s{underlining=u}
applySGR (SetBlinkSpeed b) s = s{blinkSpeed=b}
applySGR (SetVisible v) s = s{visible=v}
applySGR (SetSwapForegroundBackground i) s = s{inverted=i}
applySGR (SetColor Foreground i c) s = s{foreground=Just $ Color i c}
applySGR (SetColor Background i c) s = s{background=Just $ Color i c}
applySGR (SetPaletteColor Foreground i) s = s{foreground=Just $ PaletteColor i}
applySGR (SetPaletteColor Background i) s = s{background=Just $ PaletteColor i}
applySGR (SetRGBColor Foreground c) s = s{foreground=Just $ RGBColor c}
applySGR (SetRGBColor Background c) s = s{background=Just $ RGBColor c}


codesForState :: SGRState -> [SGR]
codesForState s = codes ++ colorCodes
  where
    codes :: [SGR]
    codes = [ Reset
            , SetConsoleIntensity $ if bold s then BoldIntensity else NormalIntensity
            , SetConsoleIntensity $ if faint s then FaintIntensity else NormalIntensity
            , SetItalicized $ italicized s
            , SetUnderlining $ underlining s
            , SetBlinkSpeed $ blinkSpeed s
            , SetVisible $ visible s
            , SetSwapForegroundBackground $ inverted s
            ]

    colorCode :: ConsoleLayer -> ANSIColor -> SGR
    colorCode l (Color i c) = SetColor l i c
    colorCode l (PaletteColor c) = SetPaletteColor l c
    colorCode l (RGBColor c) = SetRGBColor l c

    colorCodes :: [SGR]
    colorCodes = catMaybes [ colorCode Foreground <$> foreground s
                           , colorCode Background <$> background s
                           ]


bfs :: forall a b. (Ord a, Show b) => a -> (a -> Bool) -> (a -> [(a, b)]) -> Maybe [b]
bfs start done step = go Map.empty (Seq.singleton (start, Nothing))
  where
    go :: Map a (Maybe (a, b)) -> Seq (a, Maybe (a, b)) -> Maybe [b]
    go _ Seq.Empty = Nothing
    go finished ((state, lineage) :<| fringe)
      | done state = Just $ reverse $ buildPath finished' state
      | Map.member state finished = go finished fringe
      | otherwise = go finished' fringe'
      where
        finished' = Map.insert state lineage finished
        fringe' :: Seq (a, Maybe (a, b))
        fringe' = fringe >< Seq.fromList [(state', Just (state, edge)) | (state', edge) <- step state] -- (zip (step state) (repeat $ Just state))

    buildPath :: Map a (Maybe (a, b)) -> a -> [b]
    buildPath parents end = case Map.lookup end parents of
      Nothing -> error "corrupted path"
      Just Nothing -> []
      Just (Just (parent, edge)) -> edge : buildPath parents parent


codePlan :: SGRState -> SGRState -> [SGR]
codePlan source target = case bfs source (== target) step of
  Just codes -> codes
  Nothing -> error $ "No path between " ++ show source ++ " and " ++ show target
  where
    step :: SGRState -> [(SGRState, SGR)]
    step state = [(applySGR code state, code) | code <- codesForState target]


compress :: SGRState -> [SGR] -> [SGR]
compress start codes = codePlan start end
  where
    end = foldl' (flip applySGR) start codes
