module Main (main, test, makeAndTest, tryTestCase) where

import System.Environment
import Text.Regex.PCRE
import Text.Regex.PCRE.Wrap
import Data.String.Utils

import Control.Monad.Error (throwError)
import Data.Maybe
import Data.List


import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

import System.Process
import System.Exit
import Control.Monad
import Data.List.Split

main :: IO ()
main = do
  args <- getArgs
  let no_git = "--no-git" `elem` args
  let no_make = "--no-make" `elem` args
  case args of
       [] -> showHelp
       _ -> do
         results <- mapM (if no_make then test else makeAndTest) args
         let errors = catMaybes (concat results)
         case errors of
              [] -> if no_git then return (ExitSuccess) else gitcommit args
              _  -> do
                mapM_ (putStrLn . (++ "\n")) errors 
                return (ExitFailure 1)
         return ()

gitcommit :: [String] -> IO ExitCode
gitcommit files = system $ "git commit -e -i " ++ unwords files

ghcmake :: [Char] -> IO ExitCode
ghcmake f = system $ "ghc --make " ++ f

-- | Compiles (running ghc --make), extracts and tests all the examples in a file
--
-- f - filepath
--
-- Returns Returns a list of Nothing on success otherwise a descriptive string with the error.
makeAndTest :: FilePath -> IO [Maybe String]
makeAndTest f = do
  compiler_result <- ghcmake f
  guard (compiler_result == ExitSuccess)
  test f

-- | Extracts and tests all the examples in a file
--
-- f - filepath
--
-- Returns Returns a list of Nothing on success otherwise a descriptive string with the error.
test :: FilePath -> IO [Maybe String]
test f = do
  contents <- readFile f
  let regex = "(?sx) [\\s]* -- [\\s]+ >+ (.+?) -- [\\s]* \\n"
  let matches = (contents =~ regex) :: [[String]]
  mapM (tryTestCase f . breakIntoTestCase . last) matches

-- | Tries to run the test case
--
-- f - filepath to the module in which the example will run
-- (example, expected_result) - tuple with the string to be run and its expected value
--
-- Returns Nothing if correct otherwise it returns a descriptive string with the error.
tryTestCase :: FilePath -> (String, String) -> IO (Maybe String)
tryTestCase f c@(example, expected_result) = do
  session <- runTestCase f c
  case session of
       Left e -> return $ Just ("Compilation error: \n" ++ f ++ " :" ++ 
                        combineIntoEqualTest example expected_result ++ 
                        " make sure it works in ghci.")
       Right Nothing -> return Nothing
       Right (Just result_that_is_wrong) -> return $ Just ("Failed: \n" ++ 
                        f ++ ":" ++ " " ++ example ++ 
                        "\n returns: \n" ++ result_that_is_wrong ++ 
                        if not (null expected_result) 
                           then "\n not: \n" ++ expected_result 
                           else "\n so go ahead and fill it out")

-- | Runs the text case given
--
-- file - filename in which the context the example will be run (top level module)
-- (example, expected_result) - tuple with the string to be run and its expected value
--
-- Returns Nothing if correct and otherwise the result wrapped in Just
runTestCase file (example, expected_result) = runInterpreter $ do
  loadModules ['*':file]
  modules <- getLoadedModules
  setTopLevelModules modules
  is_correct <- interpret (combineIntoEqualTest example expected_result) (as :: Bool)
  case is_correct of
       True -> return Nothing
       False -> do
         wrong_result <- eval example
         return (Just wrong_result)

-- | Parses the given str
--
-- str  - The String to be used
--
-- Examples
--
--   >>> breakIntoTestCase "multiplex \"Tom\" 4  \n-- \"TomTomTomTom\""
--   ("multiplex \"Tom\" 4","\"TomTomTomTom\"")
--
-- Returns a tuple (example, expected result)
breakIntoTestCase :: String -> (String, String)
breakIntoTestCase str = (example, result)
  where 
    splitted = splitOn "\n--" str
    example  = fixstr (head splitted)
    result   = unwords (map fixstr (tail splitted))
    fixstr s = strip $ replace "\n" " " s


-- | Print help text
showHelp :: IO ()
showHelp = do
  putStrLn "CIP (Case In Point)\n"
  putStrLn "SYNOPSIS"
  putStrLn "     cip [--no-git] [--no-make] haskell_source\n"
  putStrLn "DESCRIPTION"
  putStrLn "     Tests your Haddock (REPL) examples and calls git on success."
  putStrLn "     Prevent cip from commiting by --no-git.:"
  putStrLn "     You can also prevent cip from running ghc make by --no-make\n"
  putStrLn "EXAMPLES"
  putStrLn "     cip my_source.hs"
  putStrLn "     cip my_source.hs --no-git --no-make\n"
  return ()

-- | Simple function that adds a equality sign between two strings
--
-- Examples
--
--   >>> combineIntoEqualTest "cat" "dog" 
--   "cat == dog"
--
combineIntoEqualTest :: [Char] -- ^ example
                     -> [Char] -- ^ expected result
                     -> [Char] -- ^ equality sign inserted between the strings
combineIntoEqualTest example expected_result = example ++ " == " ++ expected_result


-- | Duplicate some text an abitrary number of times.
--
-- text  - The String to be duplicated.
-- count - The Integer number of times to duplicate the text.
--
-- Examples
--
--   >>> multiplex "Tom" 4
--   "TomTomTomTom"
--
-- Returns the duplicated String.
multiplex text count = concat $ replicate count text

