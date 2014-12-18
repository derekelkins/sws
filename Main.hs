module Main where
import Control.Applicative ( liftA2 ) -- base
import Control.Monad ( when ) -- base
import Control.Monad.IO.Class ( liftIO ) -- transformers
import Control.Monad.Trans.Resource ( withInternalState, runResourceT ) -- resourcet
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import Data.Foldable ( forM_ ) -- base
import Data.Int ( Int64 ) -- base
import Data.List ( sort ) -- base
import Data.Maybe ( fromMaybe, maybe ) -- base
import Data.Monoid ( (<>) ) -- base
import Data.String ( fromString ) -- base
import System.Console.GetOpt ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) ) -- base
import System.Directory ( getDirectoryContents, doesDirectoryExist, getModificationTime, copyFile  ) -- directory
import System.Environment ( getArgs ) -- base
import System.FilePath ( (</>) ) -- filepath
import System.IO ( putStrLn ) -- base
import Network.HTTP.Types.Status ( status200, status403, status404 ) -- http-types
import Network.HTTP.Types.Method ( methodPost ) -- http-types
import Network.Wai ( Application, Middleware, requestMethod, rawPathInfo, responseLBS ) -- wai
import qualified Network.Wai.Handler.Warp as Warp -- warp
import qualified Network.Wai.Handler.WarpTLS as Warp -- warp-tls
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, def, GzipFiles(GzipIgnore, GzipCompress) ) -- wai-extra
import Network.Wai.Middleware.HttpAuth ( basicAuth, AuthSettings ) -- wai-extra
import Network.Wai.Middleware.Local ( local ) -- wai-extra
import Network.Wai.Middleware.RequestLogger ( logStdout ) -- wai-extra
import Network.Wai.Middleware.Static ( staticPolicy, addBase, isNotAbsolute, noDots, Policy, tryPolicy ) -- wai-middleware-static
import Network.Wai.Parse ( tempFileBackEnd, parseRequestBody, fileName, fileContent ) -- wai-extra

-- For certificate generation.
import Crypto.PubKey.RSA ( generate  ) -- crypto-pubkey
import Crypto.PubKey.RSA.PKCS15 ( sign ) -- crypto-pubkey
import Crypto.PubKey.HashDescr ( hashDescrSHA256 ) -- crypto-pubkey
import Crypto.Random ( EntropyPool, createEntropyPool, cprgCreate, cprgGenerate, SystemRNG ) -- crypto-random
import Data.ASN1.OID ( getObjectID ) -- asn1-types
import Data.ASN1.Types ( toASN1 ) -- asn1-types
import Data.ASN1.BinaryEncoding ( DER(DER) ) -- asn1-encoding
import Data.ASN1.Encoding ( encodeASN1' ) -- asn1-encoding
import qualified Data.PEM as PEM -- pem
import qualified Data.X509 as X509 -- x509
import qualified Data.Hourglass as HG -- hourglass
import qualified System.Hourglass as HG -- hourglass

-- Future things: case insensitive matching, optionally add CORS headers
-- Maybe future things: virtual hosts, caching, DELETE support, dropping permissions, client certificates
-- Not future things: CGI etc of any sort, "extensibility"
--
vERSION :: String
vERSION = "0.3.0.0"

rsaPublicExponent :: Integer
rsaPublicExponent = 65537

rsaSizeInBytes :: Int
rsaSizeInBytes = 256 -- Corresponds to 2048 bit encryption

certExpiryInDays :: Int64
certExpiryInDays = 30

generateCert :: Options -> HG.DateTime -> EntropyPool -> (Warp.TLSSettings, SystemRNG)
generateCert opts now ep = ((Warp.tlsSettingsMemory (PEM.pemWriteBS pemCert) (PEM.pemWriteBS pemKey)) {
                                Warp.onInsecure = Warp.DenyInsecure (fromString "Use HTTPS") }, g')
    where later = HG.timeAdd now (HG.Hours (24*certExpiryInDays))
          (bs, g) = cprgGenerate 8 (cprgCreate ep) -- generate 8 random bytes for the serial number
          ((pk, sk), g') = generate g rsaSizeInBytes rsaPublicExponent
          serialNum = BS.foldl' (\a w -> a*256 + fromIntegral w) 0 bs
          cn = getObjectID X509.DnCommonName
          o = getObjectID X509.DnOrganization
          dn = X509.DistinguishedName [(cn, fromString (optHost opts)), (o, fromString "sws generated")]
          sigAlg = X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA
          cert = X509.Certificate {
                  X509.certVersion = 0, -- 0 means v1 ...
                  X509.certSerial = serialNum,
                  X509.certSignatureAlg = sigAlg,
                  X509.certIssuerDN = dn,
                  X509.certValidity = (HG.timeAdd now (HG.Hours (-24)), later), 
                  X509.certSubjectDN = dn,
                  X509.certPubKey = X509.PubKeyRSA pk,
                  X509.certExtensions = X509.Extensions Nothing
              }
          signFunc xs = (either (error . show) id (sign Nothing hashDescrSHA256 sk xs), sigAlg, ())
          certBytes = X509.encodeSignedObject $ fst $ X509.objectToSignedExact signFunc cert
          keyBytes = encodeASN1' DER (toASN1 sk [])
          pemCert = PEM.PEM (fromString "CERTIFICATE") [] certBytes  -- This is a mite silly.  Wrap in PEM just to immediately unwrap...
          pemKey = PEM.PEM (fromString "RSA PRIVATE KEY") [] keyBytes

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
    optHost :: String,
    optCertificate :: FilePath,
    optKeyFile :: FilePath,
    optAllowUploads :: Bool }

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
    optHTTPS = False, -- TODO: Add option for the URL the certificate should be made for.
    optHost = "localhost", 
    optCertificate = "",
    optKeyFile = "",
    optAllowUploads = False }

options :: [OptDescr (Options -> Options)]
options = [
    Option "p" ["port"] (OptArg (\p opt -> opt { optPort = maybe (optPort defOptions) read p }) "NUM") 
        ("Port to listen on. (Default: " ++ (show $ optPort defOptions) ++ ")"),
    Option "h?" ["help", "version"] (NoArg (\opt -> opt { optHelp = True })) 
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
    Option "H" ["host"] (OptArg (\host opt -> opt { optHost = fromMaybe (optHost defOptions) host }) "HOST") 
        ("Host name to use for generated certificate. (Default: " ++ optHost defOptions ++ ")"),
    Option "" ["certificate"] (OptArg (\f opt -> opt { optCertificate = fromMaybe "" f }) "FILE")
        "The path to the server certificate.  Required for HTTPS.",
    Option "" ["key-file"] (OptArg (\f opt -> opt { optKeyFile = fromMaybe "" f }) "FILE")
        "The path to the private key for the certificate.  Required for HTTPS.",
    Option "w" ["allow-uploads", "writable"] (NoArg (\opt -> opt { optAllowUploads = True }))
        "Allow files to be uploaded."
 ]

validateOptions :: Options -> Maybe String
validateOptions opts 
    | BS.null (optPassword opts) && not (BS.null $ optUserName opts) = Just "Password is required to enable Basic Authentication."
    | not (BS.null $ optPassword opts) && BS.null (optUserName opts) = Just "Username is required to enable Basic Authentication."
    -- | optHTTPS opts && (null (optCertificate opts) || null (optKeyFile opts)) = Just "Certificate and key are required to enable HTTPS"
    | otherwise = Nothing

usageHeader :: String
usageHeader = "Usage: sws [OPTIONS...] [DIRECTORY]\nVersion: " ++ vERSION

basePolicy :: Policy
basePolicy = noDots <> isNotAbsolute

enableIf :: Bool -> (a -> a) -> a -> a
enableIf True f = f
enableIf    _ _ = id

update :: Options -> Policy -> Middleware
update opts policy app req k = do
    when (requestMethod req == methodPost) $ do
        runResourceT $ do
            (_, fs) <- withInternalState (\s -> parseRequestBody (tempFileBackEnd s) req)
            let fs' = map (tryPolicy policy . CBS.unpack . BS.tail . ((rawPathInfo req <> fromString "/") <>) . fileName . snd) fs
            let prefix = CBS.unpack (BS.tail (rawPathInfo req))
            liftIO $ forM_ fs $ \(_, f) ->
                case tryPolicy policy (prefix </> CBS.unpack (fileName f)) of
                    Nothing -> return ()
                    Just tgt -> do
                        let src = fileContent f
                        when (optVerbose opts) $ putStrLn ("Saving " ++ src ++ " to " ++ tgt)
                        copyFile src tgt
    app req k -- We execute the next Application regardless so that we return a listing after the POST completes.

-- TODO: Make this less fugly.
directoryListing :: Options -> FilePath -> Middleware -- TODO: Handle exceptions.  Note, this isn't critical.  It will carry on.
directoryListing opts baseDir app req k = do
    let path = baseDir </> CBS.unpack (BS.tail $ rawPathInfo req) -- TODO: This unpack is ugly.
    b <- doesDirectoryExist path
    if not b then app req k else do
        when (optVerbose opts) $ putStrLn $ "Rendering listing for " ++ path
        html <- fmap container (mapM (\p -> fileDetails p (path </> p)) =<< fmap sort (getDirectoryContents path))
        k (responseLBS status200 [] html)
  where allowWrites = optAllowUploads opts
        fileDetails label f = liftA2 (renderFile label f) (doesDirectoryExist f) (getModificationTime f)
        renderFile label path isDirectory modTime = LBS.concat $ map fromString [
            "<tr><td>", if isDirectory then "d" else "f", "</td><td><a href=\"/", path, "\">", label, "</a></td><td>", show modTime, "</td></tr>"
          ]
        container xs
          = fromString ("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta charset=\"utf-8\"><title>sws</title><style type=\"text/css\">a, a:active {text-decoration: none; color: blue;}a:visited {color: #48468F;}a:hover, a:focus {text-decoration: underline; color: red;}body {background-color: #F5F5F5;}h2 {margin-bottom: 12px;}table {margin-left: 12px;}th, td { font: 90% monospace; text-align: left;}th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}td {padding-right: 14px;}td.s, th.s {text-align: right;}div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;}div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}form { display: " ++ (if allowWrites then "inherit" else "none") ++ ";}</style></head><body><div class=\"list\"><table><tr><td></td><td>Name</td><td>Last Modified</td></tr>")
             <> LBS.concat xs
             <> fromString ("</table></div><form enctype=\"multipart/form-data\" method=\"post\" action=\"\">File: <input type=\"file\" name=\"file\" required=\"required\" multiple=\"multiple\"><input type=\"submit\" value=\"Upload\"></form><div class=\"foot\">sws" ++ vERSION ++ "</div></body></html>")

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
                $ enableIf (optAllowUploads opts) (update opts policy)
                $ staticPolicy policy 
                $ enableIf (optDirectoryListings opts) (directoryListing opts dir)
                $ app404
  where runner | optHTTPS opts && certProvided 
                    = Warp.runTLS tlsFileSettings (Warp.setPort (optPort opts) Warp.defaultSettings)
               | optHTTPS opts = \app -> do
                    now <- HG.dateCurrent
                    ep <- createEntropyPool
                    let (tlsSettings, _) = generateCert opts now ep
                    Warp.runTLS tlsSettings (Warp.setPort (optPort opts) Warp.defaultSettings) app
               | otherwise = Warp.run (optPort opts)
            where tlsFileSettings = (Warp.tlsSettings (optCertificate opts) (optKeyFile opts)) { 
                    Warp.onInsecure = Warp.DenyInsecure (fromString "Use HTTPS") } 
                  certProvided = not (null (optCertificate opts)) && not (null (optKeyFile opts))

        policy = basePolicy <> addBase dir

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> putStrLn (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
