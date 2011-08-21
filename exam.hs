module Main where

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

main = do
  args <- getArgs
  case args of
       [] -> putStrLn "Usage: ./exam file_with_comments.hs"
       _ -> do
         results <- mapM readTestCasesFromFile args
         let errors = catMaybes (concat results)
         case errors of
              [] -> commit args
              _  -> do
                    mapM (putStrLn . (++ "\n")) errors 
                    return (ExitFailure 1)
         return ()

commit files = system $ "git commit -e -i " ++ unwords files

readTestCasesFromFile :: FilePath -> IO [Maybe String]
readTestCasesFromFile f = do
                compiler_result <- system $ "ghc --make " ++ f
                guard (compiler_result == ExitSuccess)
                --putStrLn f
                contents <- readFile f
                let regex = "(?sx) [\\s]* -- [\\s]+ >+ (.+?) -- [\\s]* \\n"
                let matches = (contents =~ regex) :: [[String]]
                --putStr (show (map last matches))
                mapM (tryTestCase f) (map last matches)

tryTestCase :: FilePath -> String -> IO (Maybe String)
tryTestCase f str = do
                let (example, expected_result) = breakIntoTestCase str
                session <- testCase (breakIntoTestCase str) f
                case session of
                      Left e -> do
                        --putStrLn $ "Compilation error: " ++ combineIntoEqualTest example expected_result ++ " make sure it works in ghci."
                        --putStrLn (show e)
                        return $ Just ("Compilation error: \n" ++ f ++ " :" ++ combineIntoEqualTest example expected_result ++ 
                                       " make sure it works in ghci.")
                      Right Nothing -> do
                                      --putStrLn $ "Passed: " ++ strip (head $ lines str)
                                      return Nothing
                      Right (Just result_that_is_wrong) -> do
                                      --putStrLn $ "Failed:" ++ strip (head $ lines str)
                                      return $ Just ("Failed: \n" ++ f ++ ":" ++ " " ++ example ++ "\n returns: \n" ++ result_that_is_wrong ++ 
                                                      if not (null expected_result) then "\n not: \n" ++ expected_result else 
                                                      "\n so go ahead and fill it out")

-- | Converts the string comment to a equal test
--
-- str  - The String to be used
--
-- Examples
--
--   >>> breakIntoTestCase "multiplex \"Tom\" 4  \n-- \"TomTomTomTom\""
--   ("multiplex \"Tom\" 4","\"TomTomTomTom\"")
--
-- Returns the duplicated String.
breakIntoTestCase :: String -> (String, String)
breakIntoTestCase str = (example, result)
            where 
              splitted = splitOn "\n--" str
              example = fixstr (head splitted)
              result = unwords (map fixstr (tail splitted))
              fixstr s = (strip $ replace "\n" " " s)

combineIntoEqualTest example expected_result = example ++ " == " ++ expected_result

testCase (example, expected_result) file = runInterpreter $ do
  loadModules ["*" ++ file]
  modules <- getLoadedModules
  setTopLevelModules modules
  is_correct <- interpret (combineIntoEqualTest example expected_result) (as :: Bool)
  case is_correct of
       True -> return Nothing
       False -> do
              wrong_result <- (eval example)
              return (Just wrong_result)



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

