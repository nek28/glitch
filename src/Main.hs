module Main where

import System.Environment
import qualified Data.ByteString.Char8 as BT
import Data.Glitch (randomGlitch, randomSortBytes)
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let inputName = head args
    inputImg <- BT.readFile inputName
    glitched <- foldM (flip ($)) inputImg [ randomSortBytes
                                          , randomSortBytes 
                                          , randomGlitch ]
    let newName = mconcat ["glitched_", inputName]
    BT.writeFile newName glitched
    putStrLn "all done!"