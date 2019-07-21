{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

-- import Control.Monad.State
-- import Data.List (intercalate)
-- import Data.Word (Word8)

main :: IO ()
main = putStrLn "Hi"

-- data CSICode = ED Int
--              | SGR SGRCode
--              deriving Show

-- data SGRCode = Reset
--              | SetBold | SetFaint | UnsetBoldFaint
--              | SetItalic | UnsetItalic
--              | SetUnderline | UnsetUnderline
--              | SetFGBasic BasicColor | SetBGBasic BasicColor
--              | SetFG8 Word8 | SetBG8 Word8
--              | SetFG24 Word8 Word8 Word8
--              | SetBG24 Word8 Word8 Word8
--              deriving Show

-- data Color = Color4 BasicColor
--            | Color8 Word8
--            | Color24 Word8 Word8 Word8
--            deriving Show

-- data BasicColor = Black
--                 | Red | Green | Blue
--                 | Yellow | Cyan | Magenta
--                 | White
--                 | BrightBlack
--                 | BrightRed | BrightGreen | BrightBlue
--                 | BrightYellow | BrightCyan | BrightMagenta
--                 | BrightWhite
--                 deriving Show

-- data AnsiState = AnsiState
--   { partial :: String
--   , bold :: Bool
--   , faint :: Bool
--   , italic :: Bool
--   , underline :: Bool
--   , fg :: Maybe Color
--   , bg :: Maybe Color
--   }


-- appendString :: String -> AnsiState -> AnsiState
-- appendString text s@AnsiState{partial=prev} =
--   s{ partial = prev ++ text }


-- applyCode :: CSICode -> AnsiState -> AnsiState
-- applyCode code state = updateState code withText
--   where
--     withText = appendString (serialize code) state

--     updateState :: CSICode -> AnsiState -> AnsiState
--     updateState (ED _) _ = undefined
--     updateState (SGR Reset) s = s{fg=Nothing, bg=Nothing}
--     updateState (SGR SetBold) s = s{bold=True}
--     updateState (SGR SetFaint) s = s{faint=True}
--     updateState (SGR UnsetBoldFaint) s = s{faint=False, bold=False}
--     updateState (SGR SetItalic) s = s{italic=True}
--     updateState (SGR UnsetItalic) s = s{italic=False}
--     updateState (SGR SetUnderline) s = s{underline=True}
--     updateState (SGR UnsetUnderline) s = s{underline=False}
--     updateState (SGR (SetFGBasic c)) s = s{fg=Just (Color4 c)}
--     updateState (SGR (SetFG8 c)) s = s{fg=Just (Color8 c)}
--     updateState (SGR (SetFG24 r g b)) s = s{fg=Just (Color24 r g b)}
--     updateState (SGR (SetBGBasic c)) s = s{bg=Just (Color4 c)}
--     updateState (SGR (SetBG8 c)) s = s{bg=Just (Color8 c)}
--     updateState (SGR (SetBG24 r g b)) s = s{bg=Just (Color24 r g b)}


-- initialAnsiState :: AnsiState
-- initialAnsiState = AnsiState
--   { partial = ""
--   , bold = False
--   , faint = False
--   , italic = False
--   , underline = False
--   , fg = Nothing
--   , bg = Nothing
--   }


-- newtype AnsiBuilder a = AnsiBuilder (State AnsiState a)
--   deriving (Functor, Applicative, Monad)


-- colorOffset :: BasicColor -> Int
-- colorOffset Black   = 0
-- colorOffset Red     = 1
-- colorOffset Green   = 2
-- colorOffset Yellow  = 3
-- colorOffset Blue    = 4
-- colorOffset Magenta = 5
-- colorOffset Cyan    = 6
-- colorOffset White   = 7
-- colorOffset BrightBlack   = 60
-- colorOffset BrightRed     = 61
-- colorOffset BrightGreen   = 62
-- colorOffset BrightYellow  = 63
-- colorOffset BrightBlue    = 64
-- colorOffset BrightMagenta = 65
-- colorOffset BrightCyan    = 66
-- colorOffset BrightWhite   = 67


-- serialize :: CSICode -> String
-- serialize (ED _) = undefined
-- serialize (SGR Reset) = "\ESC[m"
-- serialize (SGR SetBold) = "\ESC[1m"
-- serialize (SGR SetFaint) = "\ESC[2m"
-- serialize (SGR UnsetBoldFaint) = "\ESC[22m"
-- serialize (SGR SetItalic) = "\ESC[3m"
-- serialize (SGR UnsetItalic) = "\ESC[23m"
-- serialize (SGR SetUnderline) = "\ESC[4m"
-- serialize (SGR UnsetUnderline) = "\ESC[24m"
-- serialize (SGR (SetFGBasic c)) = "\ESC[" ++ show n ++ "m"
--   where n = 30 + colorOffset c
-- serialize (SGR (SetFG8 c)) = "\ESC[38;5;" ++ show c ++ "m"
-- serialize (SGR (SetFG24 r g b)) = "\ESC[38;2;" ++ code ++ "m"
--   where code = intercalate ";" $ map show [r,g,b]
-- serialize (SGR (SetBGBasic c)) = "\ESC[" ++ show n ++ "m"
--   where n = 40 + colorOffset c
-- serialize (SGR (SetBG8 c)) = "\ESC[48;5;" ++ show c ++ "m"
-- serialize (SGR (SetBG24 r g b)) = "\ESC[48;2;" ++ code ++ "m"
--   where code = intercalate ";" $ map show [r,g,b]


-- ansiPrint :: String -> AnsiBuilder ()
-- ansiPrint text = AnsiBuilder $ modify (appendString text)

-- ansiPrintLn :: String -> AnsiBuilder ()
-- ansiPrintLn text = AnsiBuilder $ modify (appendString $ text++"\n")


-- ansiCode :: CSICode -> AnsiBuilder ()
-- ansiCode = AnsiBuilder . modify . applyCode


-- reset :: AnsiBuilder ()
-- reset = ansiCode $ SGR Reset


-- setFG :: BasicColor -> AnsiBuilder ()
-- setFG = setFGColor . Color4

-- setFG8 :: Word8 -> AnsiBuilder ()
-- setFG8 = setFGColor . Color8

-- setFGrgb :: Word8 -> Word8 -> Word8 -> AnsiBuilder ()
-- setFGrgb r g b = setFGColor $ Color24 r g b

-- setFGColor :: Color -> AnsiBuilder ()
-- setFGColor (Color4 color) = ansiCode $ SGR (SetFGBasic color)
-- setFGColor (Color8 code) = ansiCode $ SGR (SetFG8 code)
-- setFGColor (Color24 r g b) = ansiCode $ SGR (SetFG24 r g b)


-- setBG :: BasicColor -> AnsiBuilder ()
-- setBG = setBGColor . Color4

-- setBG8 :: Word8 -> AnsiBuilder ()
-- setBG8 = setBGColor . Color8

-- setBGrgb :: Word8 -> Word8 -> Word8 -> AnsiBuilder ()
-- setBGrgb r g b = setBGColor $ Color24 r g b

-- setBGColor :: Color -> AnsiBuilder ()
-- setBGColor (Color4 color) = ansiCode $ SGR (SetBGBasic color)
-- setBGColor (Color8 code) = ansiCode $ SGR (SetBG8 code)
-- setBGColor (Color24 r g b) = ansiCode $ SGR (SetBG24 r g b)


-- execAnsiBuilder :: AnsiBuilder a -> String
-- execAnsiBuilder (AnsiBuilder state) = partial finalState
--   where
--     finalState = execState state initialAnsiState
