module Main where
import Control.Applicative ( liftA2 ) -- base
import Control.Monad ( when ) -- base
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import Data.List ( sort ) -- base
import Data.Maybe ( fromMaybe, maybe ) -- base
import Data.Monoid ( (<>) ) -- base
import Data.String ( fromString ) -- base
import System.Console.GetOpt ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) ) -- base
import System.Directory ( getDirectoryContents, doesDirectoryExist, getModificationTime  ) -- directory
import System.Environment ( getArgs ) -- base
import System.FilePath ( (</>) ) -- filepath
import System.IO ( putStrLn ) -- base
import Network.HTTP.Types.Status ( status200, status403, status404 ) -- http-types
import Network.Wai ( Application, Middleware, rawPathInfo, responseLBS ) -- wai
import qualified Network.Wai.Handler.Warp as Warp -- warp
import qualified Network.Wai.Handler.WarpTLS as Warp -- warp-tls
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, def, GzipFiles(GzipIgnore, GzipCompress) ) -- wai-extra
import Network.Wai.Middleware.HttpAuth ( basicAuth, AuthSettings ) -- wai-extra
import Network.Wai.Middleware.Local ( local ) -- wai-extra
import Network.Wai.Middleware.RequestLogger ( logStdout ) -- wai-extra
import Network.Wai.Middleware.Static ( staticPolicy, addBase, isNotAbsolute, noDots, Policy ) -- wai-middleware-static

-- Maybe future things: virtual hosts, caching, PUT/POST/DELETE support, dropping permissions, client certificates
-- Not future things: CGI etc of any sort

data Options = Options {
    optPort :: Int,

    optHelp :: Bool,
    optVerbose :: Bool,

    optCompress :: Bool,

    optDirectoryListings :: Bool,

    optLocalOnly :: Bool,

    -- Basic authentication options
    optRealm :: String,
    optUserName :: BS.ByteString, -- some default could be chosen
    optPassword :: BS.ByteString, -- maybe we could generate and display one rather than requiring this

    -- HTTPS options
    optHTTPS :: Bool,
    optCertificate :: FilePath,
    optKeyFile :: FilePath,
    optAllowHTTP :: Bool }

defOptions :: Options
defOptions = Options {
    optPort = 3000,
    optHelp = False,
    optVerbose = False, -- TODO: Set the logging settings for the various middlewares.
    optCompress = True,
    optDirectoryListings = True,
    optLocalOnly = False,
    optRealm = "",
    optUserName = BS.empty,
    optPassword = BS.empty,
    optHTTPS = False,
    optCertificate = "",
    optKeyFile = "",
    optAllowHTTP = False } -- TODO: AllowHTTP doesn't work.  A bug in warp-tls?

options :: [OptDescr (Options -> Options)]
options = [
    Option "p" ["port"] (OptArg (\p opt -> opt { optPort = maybe (optPort defOptions) read p }) "NUM") 
        ("Port to listen on. (Default: " ++ (show $ optPort defOptions) ++ ")"),
    Option "h?" ["help"] (NoArg (\opt -> opt { optHelp = True })) 
        "Print usage.",
    Option "V" ["verbose"] (NoArg (\opt -> opt { optVerbose = True })) 
        "Print diagnostic output.",
    Option "l" ["local"] (NoArg (\opt -> opt { optLocalOnly = True })) 
        "Only accept connections from localhost.",
    Option "z" ["gzip", "compress"] (NoArg (\opt -> opt { optCompress = True })) 
        "Enable compression. (Default)",
    Option "" ["no-compress"] (NoArg (\opt -> opt { optCompress = False })) 
        "Disable compression.",
    Option "" ["no-listings"] (NoArg (\opt -> opt { optDirectoryListings = False })) 
        "Don't list directory contents.",
    Option "r" ["realm"] (OptArg (\r opt -> opt { optRealm = fromMaybe "" r }) "REALM") 
        "Set the authentication realm. It's recommended to use this with HTTPS. (Default: \"\")",
    Option "" ["password"] (OptArg (\pw opt -> opt { optPassword = fromString $ fromMaybe "" pw }) "PASSWORD") 
        "Require the given password.  It's recommended to use this with HTTPS.",
    Option "u" ["username"] (OptArg (\u opt -> opt { optUserName = fromString $ fromMaybe "" u }) "USERNAME") 
        "Require the given username.  It's recommended to use this with HTTPS.",
    Option "s" ["secure"] (NoArg (\opt -> opt { optHTTPS = True })) 
        "Enable HTTPS.", 
    Option "" ["certificate"] (OptArg (\f opt -> opt { optCertificate = fromMaybe "" f }) "FILE")
        "Require the given username.  It's recommended to use this with HTTPS.",
    Option "" ["key-file"] (OptArg (\f opt -> opt { optKeyFile = fromMaybe "" f }) "FILE")
        "Require the given username.  It's recommended to use this with HTTPS.",
    Option "" ["allow-http"] (NoArg (\opt -> opt { optAllowHTTP = True })) 
        "Allow HTTP access when HTTPS is enabled. (Not working.)",
    Option "" ["disallow-http"] (NoArg (\opt -> opt { optAllowHTTP = False })) 
        "Disallow HTTP access when HTTPS is enabled. (Default)"
 ]

validateOptions :: Options -> Maybe String
validateOptions opts 
    | BS.null (optPassword opts) && not (BS.null $ optUserName opts) = Just "Password is required to enable Basic Authentication."
    | not (BS.null $ optPassword opts) && BS.null (optUserName opts) = Just "Username is required to enable Basic Authentication."
    | optHTTPS opts && (null (optCertificate opts) || null (optKeyFile opts)) = Just "Certificate and key are required to enable HTTPS"
    | otherwise = Nothing

usageHeader :: String
usageHeader = "Usage: sws [OPTIONS...] [DIRECTORY]"

basePolicy :: Policy
basePolicy = noDots <> isNotAbsolute

enableIf :: Bool -> (a -> a) -> a -> a
enableIf True f = f
enableIf    _ _ = id

-- TODO: Make this less fugly.
directoryListing :: Options -> FilePath -> Middleware -- TODO: Handle exceptions.  Note, this isn't critical.  It will carry on.
directoryListing opts baseDir app req k = do
    let path = baseDir </> CBS.unpack (BS.tail $ rawPathInfo req) -- TODO: This unpack is ugly.
    b <- doesDirectoryExist path
    if not b then app req k else do
        when (optVerbose opts) $ putStrLn $ "Rendering listing for " ++ path
        html <- fmap container (mapM (\p -> fileDetails p (path </> p)) =<< fmap sort (getDirectoryContents path))
        k (responseLBS status200 [] html)
  where fileDetails label f = liftA2 (renderFile label f) (doesDirectoryExist f) (getModificationTime f)
        renderFile label path isDirectory modTime = LBS.concat $ map fromString [
            "<tr><td>", if isDirectory then "d" else "f", "</td><td><a href=\"/", path, "\">", label, "</a></td><td>", show modTime, "</td></tr>"
          ]
        container xs
          = fromString "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><title>sws</title><style type=\"text/css\">a, a:active {text-decoration: none; color: blue;}a:visited {color: #48468F;}a:hover, a:focus {text-decoration: underline; color: red;}body {background-color: #F5F5F5;}h2 {margin-bottom: 12px;}table {margin-left: 12px;}th, td { font: 90% monospace; text-align: left;}th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}td {padding-right: 14px;}td.s, th.s {text-align: right;}div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;}div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}</style></head><body><div class=\"list\"><table><tr><td></td><td>Name</td><td>Last Modified</td></tr>"
         <> LBS.concat xs
         <> fromString "</table></div><div class=\"foot\">sws 0.1.0.0</div></body></html>"

app404 :: Application
app404 req k = k (responseLBS status404 [] (fromString "File Not Found"))

serve :: Options -> String -> IO ()
serve (Options { optHelp = True }) _ = putStrLn $ usageInfo usageHeader options
serve opts dir =
    case validateOptions opts of
        Just err -> putStrLn err
        Nothing -> do
            putStrLn $ "Listening on port " ++ show (optPort opts)
            runner
                $ enableIf (optVerbose opts) logStdout
                $ enableIf (optLocalOnly opts) (local (responseLBS status403 [] LBS.empty))
                $ enableIf (not $ BS.null $ optPassword opts) 
                    (basicAuth (\u p -> return $ optUserName opts == u && optPassword opts == p) 
                               (fromString $ optRealm opts))
                $ enableIf (optCompress opts) (gzip def { gzipFiles = GzipCompress })
                $ staticPolicy (basePolicy <> addBase dir) 
                $ enableIf (optDirectoryListings opts) (directoryListing opts dir)
                $ app404
  where runner | optHTTPS opts = Warp.runTLS tlsSettings (Warp.setPort (optPort opts) Warp.defaultSettings)
               | otherwise = Warp.run (optPort opts)
            where tlsSettings = (Warp.tlsSettings (optCertificate opts) (optKeyFile opts)) { 
                    Warp.onInsecure = if optAllowHTTP opts then Warp.AllowInsecure else Warp.DenyInsecure (fromString "Use HTTPS") } 

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> putStrLn (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
