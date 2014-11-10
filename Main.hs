module Main where
import Data.Maybe ( maybe )
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO ( putStrLn )

-- wai-extra for GZip compression
-- wai-tls for HTTPS
-- wai-middleware-static maybe for serving the files themselves
-- System.Console.GetOpt for commandline arguments
-- Directory listing?

data Options = Options {
    optPort :: Int }
  deriving ( Show )

defOptions :: Options
defOptions = Options {
    optPort = 3000 }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['p'] ["port"] (OptArg (\p opt -> opt { optPort = maybe 3000 read p }) "NUM") "Port to listen on"
 ]

usageHeader :: String
usageHeader = "Usage: sws [OPTIONS...] [DIRECTORY]"

serve :: Options -> String -> IO ()
serve opts dir = print opts

main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> putStrLn (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
