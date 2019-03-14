module Data.Glitch 
    ( 
        randomGlitch
    ,   randomSortBytes
    ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BT
import System.Random (randomRIO)
import Control.Monad (foldM)

--we treat a JPEG image as a ByteString of Char8's 

intToChar :: Int -> Char
intToChar n = toEnum safeN
    where
        safeN = n `mod` 256

intToBT :: Int -> ByteString
intToBT n = BT.pack [intToChar n]

type Position = Int
--glitchABit recieves a byte in a file and a number to replace the byte with
glitchABit :: Position -> Int -> ByteString -> ByteString
glitchABit pos byte img = 
    mconcat [ beforeByte, glitched, afterByte ]
    where
        (beforeByte, afterWithGl) = BT.splitAt pos img
        afterByte = BT.tail afterWithGl
        glitched = intToBT byte

--uses glitchABit and a IO-supplied random number to glitch a random bit in an image
randomGlitch :: ByteString -> IO ByteString
randomGlitch img = do
    let length = BT.length img
    randomPos <- randomRIO (1, length)
    randomByte <- randomRIO (1, 256)
    let glitched = glitchABit randomPos randomByte img
    return glitched

--another way to glitch is to sort a substring of bytes
--usually, a small substring is enough, larger ones lead to a wipeout of a
--large portion of the picture

type Length = Int
--sortSubBytes does not check for validity of position and length
sortSubBytes :: Position -> Length -> ByteString -> ByteString
sortSubBytes pos len img = 
    mconcat [ beforePos, BT.sort subStr, afterSort ]
    where
        (beforePos, rest) = BT.splitAt pos img
        (subStr, afterSort) = BT.splitAt len rest

randomSortBytes :: ByteString -> IO ByteString
randomSortBytes img = do
    let length = BT.length img
    let lenToSort = 25
    randomPos <- randomRIO (1, length - lenToSort)
    let glitched = sortSubBytes randomPos lenToSort img
    return glitched