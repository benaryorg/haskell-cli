module Main where

import Data.List
import System.IO

import System.Console.ANSI

main :: IO ()
main = do
		hSetBuffering stdin NoBuffering
		hSetBuffering stdout NoBuffering
		hSetEcho stdin False
		interact cli

data State = State
           { ret :: Bool
           , ps1 :: String
           , esc :: Bool
           , text :: String
           , pos :: Int
           }
    deriving Show

data Event = Return
        | AfterReturn
		| CharAt Int Char
		| CursorTo Int
		| Esc Bool

typableChars :: [Char]
typableChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

initState :: State
initState = State
		{ ret = False
		, ps1 = "> "
		, esc = False
		, text = ""
		, pos = 0
		}

renderSep :: String
renderSep = setCursorColumnCode 0 ++ clearLineCode

cli :: String -> String
cli = intercalate renderSep . map render . scanl process initState

process :: State -> Char -> State
process state ch = foldl update state (events state ch)

events :: State -> Char -> [Event]
events state@State
	{ ret = ret
	, pos = pos
	, text = text
	, esc = esc
	} ch = events'
	where
		events'
			| ret =
				[AfterReturn] ++ events initState { ps1 = (ps1 state) } ch
			| esc && ch == 'D' =
				[ Esc False
				, CursorTo (max 0 $ pos - 1)
				]
			| esc && ch == 'C' =
				[ Esc False
				, CursorTo (min (length text) $ pos + 1)
				]
			| esc && ch == '[' = []
			| esc =
				[ Esc False
				]
			| ch == '\ESC' =
				[ Esc True
				]
			| ch == '\n' =
				[
					Return
				]
			| ch `elem` typableChars =
				[ CharAt pos ch
				, CursorTo $ pos + 1
				]
			| otherwise = []

update :: State -> Event -> State
update state (CharAt pos ch) = let (a,b) = splitAt pos (text state) in state { text = a++(ch:b) }
update state (CursorTo pos) = state { pos = pos }
update state (Esc v) = state { esc = v }
update state (Return) = state { ret = True}
update state (AfterReturn) = initState { ps1 = (ps1 state) }

render :: State -> String
render state@State
	{ ret = ret
	, ps1 = ps1
	, pos = pos
	, text = text
	} 
	| debug = show state
	| ret = realrender ++ "\n" ++ ps1
	| otherwise = realrender
	where
		realrender = ps1 ++ text ++ setCursorColumnCode (pos + length ps1)
		debug = False

