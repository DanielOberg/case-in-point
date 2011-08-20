module Main where

import System.Environment
import Text.Regex
import Text.Regex.Base.RegexLike
import Text.Regex.Posix.Wrap
import Data.String.Utils

import Control.Monad.Error (throwError)
import Data.Maybe
import Data.List


import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import System.Process
import System.Exit

main = do
  args <- getArgs
  case args of
       [] -> putStrLn "Usage: ./TestAndCommit file_with_comments.hs"
       _ -> do
         results <- mapM readTestCasesFromFile args
         let errors = catMaybes (concat results)
         case errors of
              [] -> commit args
              _  -> return (ExitFailure 1)
         return ()

commit files = do
  system $ "git commit -e -i " ++ (unwords files)

readTestCasesFromFile :: FilePath -> IO ([Maybe String])
readTestCasesFromFile f = do
                system $ "ghc --make " ++ f
                putStrLn f
                contents <- readFile f
                let regex = makeRegexOpts compExtended defaultExecOpt "[:space:]*[\\-]+.*>(.*)\n[\\-]+[:space:]*\n"
                let matches = matchRegex regex contents
                case matches of
                     Just matches' -> do
                              results <- mapM (tryTestCase f) matches'
                              return results
                     Nothing -> do
                              return $ [Just "f"]

tryTestCase :: FilePath -> String -> IO (Maybe String)
tryTestCase f str = do
                session <- testCase (formatTestCase str) f
                case session of
                      Left e -> do
                        putStrLn $ "Compilation error: " ++ (formatTestCase str) ++ (show e)
                        return $ Just (f ++ " :" ++ str)
                      Right True -> do
                                      putStrLn $ "Passed: " ++ strip (head $ lines str)
                                      return (Nothing)
                      Right False -> do
                                      putStrLn $ "Failed"
                                      return $ Just (f ++ " :" ++ strip (head $ lines str))



formatTestCase str = replace "--" "==" $ strip $ replace "\n" " " str


testCase str file = runInterpreter $ do
  set [languageExtensions := [ExtendedDefaultRules]]
  loadModules [file]
  modules <- getLoadedModules
  setImports $ ["Prelude"] ++ modules
  result <- interpret str (as :: Bool)
  return result


-- | Duplicate some text an abitrary number of times.
--
-- text  - The String to be duplicated.
-- count - The Integer number of times to duplicate the text.
--
-- Examples
--
--   > multiplex "Tom" 4
--   "TomTomTomTom"
--
-- Returns the duplicated String.
multiplex text count = concat $ take count $ repeat text

