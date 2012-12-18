module LMOR
  ( writeTextSection
  , callTargets
  , Branch (..)
  , branches
  , search
  , modifyBinary
  , modifyBinary_
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Elf
import Data.Int
import Data.Maybe
import qualified Data.IntSet as S
import qualified Data.Set as Set
import Data.Word
import System.Exit
import System.IO
import System.Process
import Text.Printf

textSection :: ByteString -> (ByteString, Int)
textSection file = (textData, B.length $ fst $ B.breakSubstring textData file)
  where
  elf = parseElf file
  text = case [ s | s <- elfSections elf, elfSectionName s == ".text", elem SHF_EXECINSTR $ elfSectionFlags s ] of
    [s] -> s
    _   -> error "Single .text section not found."
  textData = elfSectionData text

writeTextSection :: FilePath -> FilePath -> IO ()
writeTextSection a b = do
  a <- B.readFile a
  let (text, _) = textSection a
  B.writeFile b text

callTargets :: FilePath -> IO [Int]
callTargets file = do
  file <- B.readFile file
  let (textData, textLoc) = textSection file
  return $ map (textLoc +) $ S.toList $ S.fromList $ mapMaybe (f textData) [0 .. B.length textData - 1]
  where
  f :: ByteString -> Int -> Maybe Int
  f program i
    | B.index program i == 0xe8 && i < B.length program - 4 && address >= 0 && address < B.length program = Just address
    | otherwise = Nothing
    where
    bytes = [ B.index program $ i + j | j <- [4, 3 .. 1] ]
    word :: Word32
    word = foldl1 (.|.) [ shiftL (fromIntegral b) s | (b, s) <- zip bytes [24, 16 .. 0] ]
    address = (fromIntegral (fromIntegral word :: Int32) :: Int) + i + 1

data Branch = JE | JNE | JE' | JNE' deriving (Show, Eq, Ord)

branches :: FilePath -> IO [(Int, Branch)]
branches file = do
  file <- B.readFile file
  let (textData, textLoc) = textSection file
  return [ (i + textLoc, b) | (i, b) <- Set.toList $ Set.fromList $ mapMaybe (f textData) [0 .. B.length textData - 1] ]
  where
  f :: ByteString -> Int -> Maybe (Int, Branch)
  f program i
    | B.index program i == 0x74 = Just (i, JE)
    | B.index program i == 0x75 = Just (i, JNE)
    | otherwise = Nothing

search :: FilePath -> ((ExitCode, String, String) -> IO Bool) -> [String] -> [[(Int, Char)]] -> IO ()
search exe valid args targets = mapM_ f $ zip [1 ..] targets
  where
  f :: (Int, [(Int, Char)]) -> IO ()
  f (i, mods) = do
    mods' <- modifyBinary exe mods
    printf "%d. %s" i (show mods)
    hFlush stdout
    r <- readProcessWithExitCode exe args "" >>= valid
    modifyBinary_ exe mods'
    printf "%s\n" (if r then ": ** PASS **" else "")
    hFlush stdout

modifyBinary :: FilePath -> [(Int, Char)] -> IO [(Int, Char)]
modifyBinary exe mods = withBinaryFile exe ReadWriteMode $ \ h -> mapM (f h) mods
  where
  f :: Handle -> (Int, Char) -> IO (Int, Char)
  f h (i, c) = do
    hSeek h AbsoluteSeek $ fromIntegral i
    c' <- hGetChar h
    hSeek h AbsoluteSeek $ fromIntegral i
    hPutChar h c
    return (i, c')

modifyBinary_ :: FilePath -> [(Int, Char)] -> IO ()
modifyBinary_ exe mods = withBinaryFile exe ReadWriteMode $ \ h -> mapM_ (f h) mods
  where
  f :: Handle -> (Int, Char) -> IO ()
  f h (i, c) = do
    hSeek h AbsoluteSeek $ fromIntegral i
    hPutChar h c

