module LMOR
  ( extractText
  , branchTargets
  , newElf
  , search
  ) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Elf
import Data.Int
import Data.Maybe
import Data.Word
import System.Exit
import System.IO
import System.Process
import System.Timeout
--import System.Environment
--import Text.Disassembler.X86Disassembler
import Text.Printf

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [file] -> analyze file
--     _ -> return ()
-- 
-- analyze :: FilePath -> IO ()
-- analyze file = do
--   f <- B.readFile file
--   let (text, addr, reconstruct) = extractText f
--   printf "%08x\n" addr
--   print $ length $ branchTargets text
--   {-
--   r <- disassembleList (B.unpack $ B.take 100 text)
--   case r of
--     Left e -> print e
--     Right i -> mapM_ print i
--     -}

-- | Return the .text section, the address of the section, and a function to reconstruct the elf file with a modified .text section.
extractText :: ByteString -> (ByteString, Word64, Int, ByteString -> ByteString)
extractText file = (textData, elfSectionAddr text, B.length before, reconstruct)
  where
  elf = parseElf file
  text = case [ s | s <- elfSections elf, elfSectionName s == ".text", elem SHF_EXECINSTR $ elfSectionFlags s ] of
    [s] -> s
    _   -> error "Single .text section not found."
  textData = elfSectionData text
  (before, rest) = B.breakSubstring textData file
  reconstruct text = B.concat [before, text, B.drop (B.length textData) rest]


-- | Possible branch targets.
branchTargets :: ByteString -> [Int]
branchTargets program = mapMaybe f [0 .. B.length program - 1]
  where
  f :: Int -> Maybe Int
  f i
    | B.index program i == 0xe8 && i < B.length program - 4 && address >= 0 && address < B.length program = Just address
    | otherwise = Nothing
    where
    bytes = [ B.index program $ i + j | j <- [4, 3 .. 1] ]
    word :: Word32
    word = foldl1 (.|.) [ shiftL (fromIntegral b) s | (b, s) <- zip bytes [24, 16 .. 0] ]
    address = (fromIntegral (fromIntegral word :: Int32) :: Int) + i + 1

-- | Creates a new elf given a list of return points.
newElf :: ByteString -> [Int] -> ByteString
newElf f i = r $ setReturns p i
  where
  (p, _, _, r) = extractText f

-- | Sets a return at a given index.
setReturn :: ByteString -> Int -> ByteString
setReturn program i = B.concat [before, B.singleton 0xc3, B.drop 1 after]
  where
  (before, after) = B.splitAt i program

setReturns :: ByteString -> [Int] -> ByteString
setReturns = foldl setReturn

-- | Search for canidates.
search :: FilePath -> Int -> ((ExitCode, String, String) -> IO Bool) -> [String] -> Int -> [[Int]] -> IO [[Int]]
search exe textLoc valid args timeoutUS targets = filterM f targets
  where
  f :: [Int] -> IO Bool
  f i = do
    h <- openFile exe ReadWriteMode
    chars <- mapM (writeC3 h) i
    hFlush h
    r <- timeout timeoutUS $ readProcessWithExitCode exe args "" >>= valid
    mapM_ (restore h) $ zip i chars
    hClose h
    case r of
      Nothing -> return False
      Just a  -> do
        when a $ printf "%s : ** PASS **\n" (show i)
        return a

  writeC3 :: Handle -> Int -> IO Char
  writeC3 h i = do
    hSeek h AbsoluteSeek $ fromIntegral $ textLoc + i
    c <- hGetChar h
    hSeek h AbsoluteSeek $ fromIntegral $ textLoc + i
    hPutChar h $ chr 0xc3
    return c

  restore :: Handle -> (Int, Char) -> IO ()
  restore h (i, c) = do
    hSeek h AbsoluteSeek $ fromIntegral $ textLoc + i
    hPutChar h c

