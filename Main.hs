module Main (main) where

import Data.Elf
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Harpy.X86Disassembler
import System.Environment
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> analyze file
    _ -> return ()

analyze :: FilePath -> IO ()
analyze file = do
  f <- B.readFile file
  let (text, addr, reconstruct) = extractText f
  printf "%08x\n" addr
  r <- disassembleList (B.unpack $ B.take 100 text)
  case r of
    Left e -> print e
    Right i -> mapM_ print i

-- | Return the .text section, the address of the section, and a function to reconstruct the elf file with a modified .text section.
extractText :: ByteString -> (ByteString, Word64, ByteString -> ByteString)
extractText file = (textData, elfSectionAddr text, reconstruct)
  where
  elf = parseElf file
  text = case [ s | s <- elfSections elf, elfSectionName s == ".text", elem SHF_EXECINSTR $ elfSectionFlags s ] of
    [s] -> s
    _   -> error "Single .text section not found."
  textData = elfSectionData text
  (before, rest) = B.breakSubstring textData file
  reconstruct text = B.concat [before, text, B.drop (B.length textData) rest]


