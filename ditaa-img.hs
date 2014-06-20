-- | Convert Markdown with Ditaa plain-text diagrams to Markdown with images.
--
-- Ditaa diagram format in github-flavored markdown:
--
-- @
-- ```ditaa
--
--   +-----+    +------+
--   |hello|--->|world!|
--   +-----+    +------+
--
--```
--
-- Pandoc scripting: http://johnmacfarlane.net/pandoc/scripting.html
-- Ditaa: http://ditaa.sourceforge.net/
--
-- DITAA_CMD environment variable defines how Ditaa should be called.
-- INPUT and OUTPUT should be used as placeholders for the file names.
-- If not set, the default value is @ditaa INPUT OUTPUT@.


import Control.Monad (when)
import Data.Digest.Adler32 (adler32)
import System.Console.GetOpt
import System.Environment (getEnvironment, getArgs)
import System.Exit (ExitCode(..), exitSuccess)
import System.IO
import GHC.IO.Handle (hDuplicate)
import System.IO.Temp (withSystemTempFile, openTempFile)
import System.Process (runProcess, waitForProcess)
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BS


usage = unlines $
 [ "Usage: ditaa-img [options] [INPUT.MD]"
 , ""
 , "Convert Markdown with Ditaa diagrams (ASCII diagrams) to Markdown with images."
 , "Image files are generated and saved in the current directory."
 , "If INPUT.MD file is not specified, read Markdown from stdin."
 , ""
 , "Set DITAA_CMD environment variable to control how Ditaa is invoked."
 , "The default command is \"ditaa INPUT OUTPUT\". INPUT and OUTPUT are placeholders."
 , ""
 , "For more information about Ditaa see http://ditaa.sf.net/"
 , ""
 , "Options:"]


getEnvDefault :: String -> String -> IO String
getEnvDefault varName defaultValue = do
   maybeValue <- return . lookup varName =<< getEnvironment
   return $ maybe defaultValue id maybeValue


-- | Replace a pattern in a string.
replace :: String -> String -> String -> String
replace pattern substitution s =
    let plen = length pattern
        rsubstitution = reverse substitution
    in  go plen rsubstitution "" s
  where
    go _ rsubstitution rs' [] = reverse rs'
    go plen rsubstitution rs' s@(c:cs) =
        let match = all (uncurry (==)) $ zip pattern s
        in  if match
            then go plen rsubstitution (rsubstitution ++ rs') (drop plen s)
            else go plen rsubstitution (c:rs') cs


-- | Like system but redirect all stdout to stderr
system2 :: String -> IO ExitCode
system2 cmd = do
  let (exe:args) = words cmd
  stderr2 <- hDuplicate stderr
  phandle <- runProcess exe args Nothing Nothing
             Nothing (Just stderr2) (Just stderr2)
  waitForProcess phandle


-- | Generate a new image file.
generateImage :: String -> String -> String -> IO ExitCode
generateImage ditaaCommand imagename code =
  withSystemTempFile (imagename ++ "..txt") $ \tmpname tmphandle -> do
     let cmd = replace "INPUT" tmpname .
               replace "OUTPUT" imagename $ ditaaCommand
     hPutStrLn tmphandle code
     hClose tmphandle
     system2 cmd


createImageBlock :: String -> String -> IO (Maybe Block)
createImageBlock ditaaCommand code = do
  let filename = printf "ditaa-%08x" $ adler32 $ BS.pack code
  let imagename = filename ++ ".png"
  let newcode = Para [Image [] (imagename, "")]
  r <- generateImage ditaaCommand imagename code
  case r of
    ExitSuccess -> return (Just newcode)
    _ -> do
      hPutStrLn stderr $ "ditaa exit: " ++ show r
      return Nothing


-- | Replace a code block with .ditaa class with an image block.
replaceDitaaCodeBlock :: String -> Block -> IO Block
replaceDitaaCodeBlock ditaaCmd cb@(CodeBlock (_, classes, _) code)
  | "ditaa" `elem` classes  = do
      maybeImageBlock <- createImageBlock ditaaCmd code
      return $ maybe cb id maybeImageBlock
  | otherwise               = return cb
replaceDitaaCodeBlock _ otherblock = return otherblock


convertMd2Md :: String -> String -> IO String
convertMd2Md ditaaCmd src = do
  let doc = readMarkdown def {readerExtensions = pandocExtensions} src
  newDoc <- walkM (replaceDitaaCodeBlock ditaaCmd) doc
  return $ writeMarkdown def {writerExtensions = pandocExtensions} newDoc



data Flags = HelpFlag | Output String
             deriving (Show, Eq)


options = [ Option "h" ["help"] (NoArg HelpFlag) "show usage"
          , Option "o" ["output"] (ReqArg Output "FILENAME")
                       "save new Markdown to file" ]


getInput :: [String] -> Maybe String
getInput (filename:_) = Just filename
getInput _ = Nothing


getOutput :: [Flags] -> Maybe String
getOutput [] = Nothing
getOutput (Output s:_) = Just s
getOutput (_:flags) = getOutput flags


-- opens a new file or uses stdin, DOES NOT close handles
withInputFile :: Maybe String -> (Handle -> IO a) -> IO a
withInputFile maybeFilename action =
    case maybeFilename of
      (Just filename) -> do
          h <- openFile filename ReadMode
          action h
      Nothing ->
          action stdin


-- opens a new file or uses stdout, DOES NOT close handles
withOutputFile :: Maybe String -> (Handle -> IO a) -> IO a
withOutputFile maybeFilename action =
    case maybeFilename of
      (Just filename) -> do
          h <- openFile filename WriteMode
          action h
      Nothing ->
          action stdout


main = do
    rawArgs <- getArgs
    let (flags, args, errMsgs) = getOpt Permute options rawArgs
    when (HelpFlag `elem` flags) $ do
         hPutStrLn stderr $ usageInfo usage options
         exitSuccess
    ditaaCmd <- getEnvDefault "DITAA_CMD" "ditaa INPUT OUTPUT"
    input <- withInputFile (getInput args) $ \h -> hGetContents h
    output <- convertMd2Md ditaaCmd input
    withOutputFile (getOutput flags) $ \h -> hPutStr h output >> hClose h
