module Main where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import Data.Maybe ( fromMaybe, maybe )
import Data.Monoid ( (<>) )
import Data.String ( fromString ) -- for AuthRealm
import System.Console.GetOpt ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) )
import System.Environment ( getArgs )
import System.IO ( putStrLn )
import Network.HTTP.Types.Status ( status403, status404 ) -- http-types
import Network.Wai ( Application, responseLBS ) -- wai
import Network.Wai.Middleware.Static ( staticPolicy, addBase, isNotAbsolute, noDots, Policy ) -- wai-middleware-static
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, def, GzipFiles(GzipIgnore, GzipCompress) ) -- wai-extra
import Network.Wai.Middleware.HttpAuth ( basicAuth, AuthSettings ) -- wai-extra
import Network.Wai.Middleware.Local ( local ) -- wai-extra
import Network.Wai.Middleware.RequestLogger ( logStdout ) -- wai-extra
import qualified Network.Wai.Handler.Warp as Warp -- warp
import qualified Network.Wai.Handler.WarpTLS as Warp -- warp-tls

-- Directory listing?

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

app404 :: Application
app404 req k = k (responseLBS status404 [] LBS.empty)

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
                -- $ directoryListing -- TODO
                $ app404
  where runner | optHTTPS opts = Warp.runTLS tlsSettings (Warp.setPort (optPort opts) Warp.defaultSettings)
               | otherwise = Warp.run (optPort opts)
            where tlsSettings = (Warp.tlsSettings (optCertificate opts) (optKeyFile opts)) { 
                    Warp.onInsecure = if optAllowHTTP opts then Warp.AllowInsecure else Warp.DenyInsecure (fromString "Use HTTPS") } 

main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> putStrLn (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
