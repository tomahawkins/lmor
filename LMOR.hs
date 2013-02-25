module LMOR
  ( writeTextSection
  , callTargets
  , branches
  , search
  , modifyBinary
  , modifyBinary'
  ) where

import Control.Monad (when)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord, chr)
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

branches :: FilePath -> IO [Int]
branches file = do
  file <- B.readFile file
  let (textData, textLoc) = textSection file
  return [ i + textLoc | i <- Set.toList $ Set.fromList $ mapMaybe (f textData) [0 .. B.length textData - 1] ]
  where
  f :: ByteString -> Int -> Maybe Int
  f program i
    | B.index program i == 0x74 = Just i
    | B.index program i == 0x75 = Just i
    | B.index program i == 0x0f && B.index program (i + 1) == 0x84 = Just i
    | B.index program i == 0x0f && B.index program (i + 1) == 0x85 = Just i
    | otherwise = Nothing

search :: FilePath -> ((ExitCode, String, String) -> IO Bool) -> FilePath -> [String] -> [Int] -> IO ()
search exe valid cmd args targets = mapM_ f $ zip [1 ..] targets
  where
  f :: (Int, Int) -> IO ()
  f (i, m) = do
    modifyBinary exe [m]
    printf "%d. %d\n" i m
    hFlush stdout
    r <- readProcessWithExitCode cmd args "" >>= valid
    modifyBinary exe [m]
    when r $ putStrLn "** PASS **"
    hFlush stdout

modifyBinary :: FilePath -> [Int] -> IO ()
modifyBinary exe m = withBinaryFile exe ReadWriteMode $ \ h -> mapM_ (f h) m
  where
  f :: Handle -> Int -> IO ()
  f h i = do
    hSeek h AbsoluteSeek $ fromIntegral i
    c <- hGetChar h
    case ord c of
      0x74 -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h $ chr 0x75
      0x75 -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h $ chr 0x74
      0x0f -> do
        c <- hGetChar h
        case ord c of
          0x84 -> do
            hSeek h AbsoluteSeek $ fromIntegral $ i + 1
            hPutChar h $ chr 0x85
          0x85 -> do
            hSeek h AbsoluteSeek $ fromIntegral $ i + 1
            hPutChar h $ chr 0x84
          _ -> error $ printf "Expected JE or JNE, but got something else at 0x%x\n" i
      _ -> error $ printf "Expected JE or JNE, but got something else at 0x%x\n" i

modifyBinary' :: FilePath -> [(Int, Word8)] -> IO ()
modifyBinary' exe mods = withBinaryFile exe ReadWriteMode $ \ h -> mapM_ (f h) mods
  where
  f :: Handle -> (Int, Word8) -> IO ()
  f h (i, c) = do
    hSeek h AbsoluteSeek $ fromIntegral i
    hPutChar h $ chr $ fromIntegral c

