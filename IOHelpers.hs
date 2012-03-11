module IOHelpers
( getIntGrid
, getLines
, getAndProcess
, takeIntGrid
) where

import Helpers (split)
import Control.Monad (liftM)

dropReturn [] = []
dropReturn ('\r':ns) = dropReturn ns
dropReturn (n:ns) = n:(dropReturn ns)

takeIntGrid = liftM (parseIntLines . map dropReturn . split '\n') getContents

parseIntLines :: [String] -> [[Int]]
parseIntLines [] = []
parseIntLines [[]] = []
parseIntLines (l:ls) = (result:rest) where
  result = parseIntLine l
  rest = parseIntLines ls

getIntGrid = do
  putStrLn "Paste the big grid, then blank line"
  lines <- getLines
  return $ parseIntLines lines

parseIntLine :: String -> [Int]
parseIntLine = map read . split ' '

getLines :: IO [[Char]]
getLines = do
  line <- getLine
  case line of
    "" -> return []
    "\r" -> return []
    otherwise -> do
      let sLine = takeWhile (/='\r') line
      rest <- getLines
      return (sLine:rest)

getAndProcess :: (Show b) => IO a -> (a -> b) -> IO ()
getAndProcess getter processor = (liftM processor getter) >>= print

