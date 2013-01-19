{-# LANGUAGE TupleSections #-}
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
     args <- getArgs
     path <- if null args
             then (splitOn ":" . fromJust . lookup "PATH") `fmap` getEnvironment
             else (return . head $ args) >>= fmap (splitOn ":") . readFile 
     ls <- runLs path
     ex <- getExecs ls
     input@(~(tar:args)) <- fmap (splitOn " ") (runDmenu . map fst $ ex)
     let pathLookup = lookup tar ex
         execPath = if isJust pathLookup
                    then (fromJust pathLookup) ++ tar
                    else []
     unless (null input || null execPath) (void $ forkOS (void $ rawSystem execPath args))

runDmenu :: [String] -> IO String
runDmenu = (readProcessWithExitCode "dmenu" [] . sanitise >=> return . sanitiseInit . snd3)

runLs :: [String] -> IO [(String, String)]
runLs path = concat `fmap` mapM (\f -> readProcessWithExitCode "ls" (f:["-1"]) "" >>= return . map (f ++ "/",) . words . snd3) path

getExecs :: [(String, String)] -> IO [(String, String)]
getExecs ls = map (uncurry $ flip (,)) `fmap` filterM (isExecutable . uncurry (++)) ls

sanitise :: [String] -> String
sanitise [] = []
sanitise (l:ls) = l ++ ('\n':sanitise ls)

sanitiseInit :: String -> String
sanitiseInit "" = ""
sanitiseInit s  = init s

isExecutable :: String -> IO Bool
isExecutable f = do
             exists <- doesFileExist f
             if exists
             then executable `fmap` getPermissions f
             else return False

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn e ls = if null r
               then [n]
               else n:(splitOn e (tail r))
        where
                (n, r) = break (flip elem e) ls
