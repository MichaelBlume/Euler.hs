module IOHelpers
( getIntGrid
, getLines
, getAndProcess
) where

import Helpers (split)

getIntGrid = do
  putStrLn "Paste the big grid, then blank line"
  lines <- getLines
  return $ map parseIntLine lines where
    parseIntLine :: String -> [Int]
    parseIntLine = map read . split ' '

getLines :: IO [[Char]]
getLines = do
  line <- getLine
  if null line
      then return []
      else do
          rest <- getLines
          return (line:rest)

getAndProcess :: (Show b) => IO a -> (a -> b) -> IO ()
getAndProcess getter processor = do
  intermediate <- getter
  print $ processor intermediate

