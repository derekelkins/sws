module Main where
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe ( maybe )
import Data.Monoid ( (<>) )
import System.Console.GetOpt ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) )
import System.Environment ( getArgs )
import System.IO ( putStrLn )
import Network.HTTP.Types.Status ( status404 )
import Network.Wai ( Application, responseLBS )
import Network.Wai.Middleware.Static ( staticPolicy, addBase, isNotAbsolute, noDots, Policy )
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, def, GzipFiles(GzipCompress) )
import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Network.Wai.Handler.WarpTLS as WarpTLS

-- wai-extra for GZip compression
-- wai-tls for HTTPS
-- wai-middleware-static maybe for serving the files themselves
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

basePolicy :: Policy
basePolicy = noDots <> isNotAbsolute

-- TODO: Add directory listing.
app404 :: Application
app404 req k = k (responseLBS status404 [] LBS.empty)

serve :: Options -> String -> IO ()
serve opts dir = do
    putStrLn $ "Listening on port " ++ show (optPort opts)
    Warp.run (optPort opts) $ gzip def { gzipFiles = GzipCompress } $ staticPolicy (addBase dir <> basePolicy) $ app404

main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> putStrLn (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
