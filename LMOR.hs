module LMOR
  ( writeTextSection
  --, callTargets
  --, branches
  --, search
  , modifyBinary
  , setByte
  , invertBranch
  , takeBranch
  , passBranch
  , CallGraph (..)
  , callGraph
  , hashFun
  , gdbTrace
  ) where

--import Control.Monad (when)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord, chr)
import Data.Elf
import Data.Hashable (hash)
--import Data.Int
import Data.List
--import Data.Maybe
import qualified Data.IntMap as M
--import qualified Data.IntSet as S
--import qualified Data.Set as Set
import Data.Word
--import System.Exit
import System.IO
--import System.Process
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

{-
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
    modifyBinary exe [invertBranch m]
    printf "%d. %d\n" i m
    hFlush stdout
    r <- readProcessWithExitCode cmd args "" >>= valid
    modifyBinary exe [invertBranch m]
    when r $ putStrLn "** PASS **"
    hFlush stdout
-}

modifyBinary :: FilePath -> [Handle -> IO ()] -> IO ()
modifyBinary bin mods = withBinaryFile bin ReadWriteMode $ \ h -> sequence_ [ f h | f <- mods ]

setByte :: Int -> Word8 -> Handle -> IO ()
setByte addr byte h = do
  hSeek h AbsoluteSeek $ fromIntegral addr
  hPutChar h $ chr $ fromIntegral byte

invertBranch :: Int -> Handle -> IO ()
invertBranch i h = do
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
        a -> error $ printf "Expected JE or JNE, but got 0x%02x at 0x%x\n" a i
    a -> error $ printf "Expected JE or JNE, but got 0x%02x at 0x%x\n" a i

takeBranch :: Int -> Handle -> IO ()
takeBranch i h = do
  hSeek h AbsoluteSeek $ fromIntegral i
  a <- hGetChar h
  b <- hGetChar h
  case (a, b) of
    (a, b)
      | elem a [je, jne] -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h jmp
      | a == ext && elem b [jeq, jneq, ja] -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hGetChar h
        hGetChar h
        a0 <- hGetChar h >>= return . ord
        a1 <- hGetChar h >>= return . ord
        a2 <- hGetChar h >>= return . ord
        a3 <- hGetChar h >>= return . ord
        let addr = foldl1 (.|.) [ shiftL a n | (n, a) <- zip [0, 8 ..] [a0, a1, a2, a3] ]  
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h jmpq
        sequence_ [ hPutChar h $ chr $ shiftR (addr + 1)  n .&. 0xff | n <- [0, 8, 16, 24] ]
        hPutChar h nop
    _ -> error $ printf "Expected branch, but got something else at 0x%x.\n" i

passBranch :: Int -> Handle -> IO ()
passBranch i h = do
  hSeek h AbsoluteSeek $ fromIntegral i
  a <- hGetChar h
  b <- hGetChar h
  case (a, b) of
    (a, b)
      | elem a [je, jne] -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h nop
        hPutChar h nop
      | a == ext && elem b [jeq, jneq, ja] -> do
        hSeek h AbsoluteSeek $ fromIntegral i
        hPutChar h nop
        hPutChar h nop
        hPutChar h nop
        hPutChar h nop
        hPutChar h nop
        hPutChar h nop
    _ -> error $ printf "Expected branch, but got something else at 0x%x.\n" i

nop  = chr 0x90
je   = chr 0x74
jne  = chr 0x75
jeq  = chr 0x84
jneq = chr 0x85
jmp  = chr 0xeb
jmpq = chr 0xe9
--xor  = chr 0x31
--eax  = chr 0xc0
ext  = chr 0x0f
ja   = chr 0x87



{-
callGraph :: [(FilePath, String)] -> IO ()
callGraph libs = do
  funs <- sequence [ functions lib name | (lib, name) <- libs ] >>= return . shrink . concat
  dir <- getCurrentDirectory
  createDirectoryIfMissing False $ dir ++ "/callGraph"
  setCurrentDirectory $ dir ++ "/callGraph"
  a <- getDirectoryContents (dir ++ "/callGraph")
  sequence_ [ removeFile a | a <- a, isSuffixOf ".html" a ]
  sequence_ [ writeFile (name l f ++ ".html") $ html funs (l, f, calls) | (l, f, calls) <- funs ]
  where
  {-
  graph :: [(String, String, [String])] -> String
  graph g' = "digraph callGraph {\n" ++ unlines (
    [ printf "  \"%s.%s\";" l f | (l, f, _) <- g ] ++
    [ printf "  \"%s.%s\" -> \"%s.%s\";" callerL callerF calleeL calleeF | (callerL, callerF, calls) <- g, (calleeL, calleeF, _) <- g, elem calleeF calls ]
    ) ++ "}"
    where
    g = shrink g'
    -}

  shrink :: [(String, String, [String])] -> [(String, String, [String])]
  shrink g = [ (m, f, c') | (m, f, c) <- g, let c' = filter (flip elem funcs) c, elem f calls || not (null c')  ]
    where
    funcs = [ f | (_, f, _) <- g ]
    calls = concat [ filter (flip elem funcs) calls | (_, _, calls) <- g ]

  html :: [(String, String, [String])] -> (String, String, [String]) -> String
  html g (lib, fun, calls) = unlines
    [ printf "<h2>Library: %s</h2>" lib
    , printf "<h2>Function: %s</h2>" fun
    , printf "<h3>Calls</h3>"
    , printf "<ul>"
    , unlines [ printf "  <li><a href=\"%s.html\">%s.%s</a></li>" (name l f) l f | callee <- calls, (l, f, _) <- g, callee == f ]
    , printf "</ul>"
    , printf "<h3>Callers</h3>"
    , printf "<ul>"
    , unlines [ printf "  <li><a href=\"%s.html\">%s.%s</a></li>" (name l f) l f | (l, f, c) <- g, elem fun c ]
    , printf "</ul>"
    ]

  name :: String -> String -> String
  name lib fun = take 200 $ lib ++ "." ++ fun
-}

data CallGraph = CallGraph
  { cgFunctions :: M.IntMap (String, String)  -- Library name, function name.
  , cgCallGraph :: [(Int, Int)]
  } deriving (Show, Read)

callGraph :: [FilePath] -> IO CallGraph
callGraph libs = do
  funs <- sequence [ parseAsm lib (libName lib) | lib <- libs ] >>= return . concat
  let functions = M.fromList [ (hash f, (l, f)) | (l, f, _) <- funs ]
  return CallGraph
    { cgFunctions = functions
    , cgCallGraph = nub $ concat [ [ (hash f, hash call) | call <- calls, M.member (hash call) functions ] | (_, f, calls) <- funs ]
    }
  where
  libName :: FilePath -> String
  libName = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse

  parseAsm :: FilePath -> String -> IO [(String, String, [String])]
  parseAsm file lib = readFile file >>= return . f1 . lines
    where
    f1 :: [String] -> [(String, String, [String])]
    f1 a = case a of
      [] -> []
      a : rest
        | isSuffixOf "@plt>:" a -> f1 rest
        | isSuffixOf     ">:" a -> (lib, reverse $ drop 2 $ reverse $ drop 18 a, calls) : f1 rest'
        | otherwise             -> f1 rest
        where
        (calls, rest') = f2 rest
  
    f2 :: [String] -> ([String], [String])
    f2 a = case a of
      [] -> ([], [])
      a : rest
        | isSuffixOf ">:" a -> ([], a : rest)
        | isPrefixOf "callq" line && notElem '+' line && last line == '>' -> (fun : funs, rest')
        | otherwise -> f2 rest
        where
        fun = takeWhile (not . flip elem "@>") $ tail $ dropWhile (/= '<') line
        (funs, rest') = f2 rest
        line = drop 32 a

hashFun :: String -> Int
hashFun = hash

tracePoints :: (String -> Bool) -> FilePath -> IO [(String, Int, String)]  -- Library, address, label.
tracePoints pred file = readFile file >>= return . f1 "" 0 . dropWhile (/= "Disassembly of section .text:") . lines
  where
  lib = reverse $ takeWhile (/= '/') $ drop 2 $ reverse file
  f1 :: String -> Int -> [String] -> [(String, Int, String)]
  f1 label labelAddr a = case a of
    [] -> []
    a : rest
      | isSuffixOf ">:" a && notElem '.' label' && pred label' -> (lib, addr, label') : f1 label' addr rest
      -- | any (flip isPrefixOf $ drop 32 a) ["je", "jne"] -> (lib, addr, label, addr - labelAddr) : f1 label labelAddr rest
      | otherwise -> f1 label labelAddr rest
      where
      addr = read $ ("0x" ++) $ filter (/= ':') $ head $ words a
      label' = reverse $ drop 2 $ reverse $ drop 1 $ words a !! 1

gdbTrace :: (String -> Bool) -> [FilePath] -> IO String
gdbTrace pred libs = mapM (tracePoints pred) libs >>= return . ("set breakpoint pending on\n" ++) . concatMap format . concat
  where
  format :: (String, Int, String) -> String
  format (lib, addr, label) = unlines
    [ printf "break %s" label
    , printf "commands"
    , printf "printf \"%s  %x\\n\"" lib addr
    , printf "continue"
    , printf "end"
    ]
















