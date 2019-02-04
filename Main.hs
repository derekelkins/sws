module Main where
import Control.Applicative ( liftA2 ) -- base
import Control.Exception ( catch ) -- base
import Control.Monad ( when ) -- base
import Control.Monad.IO.Class ( liftIO ) -- transformers
import Control.Monad.Trans.Resource ( withInternalState, runResourceT, ResourceCleanupException ) -- resourcet
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import Data.Bits ( (.&.), unsafeShiftR, xor ) -- base
import Data.Foldable ( forM_ ) -- base
import Data.Int ( Int64 ) -- base
import Data.Word ( Word8 ) -- base
import Data.List ( sort ) -- base
import Data.Monoid ( (<>) ) -- base
import Data.String ( fromString ) -- base
import System.Console.GetOpt ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) ) -- base
import System.Directory ( getDirectoryContents, doesDirectoryExist, getModificationTime, renameFile, doesFileExist ) -- directory
import System.Environment ( getArgs ) -- base
import System.FilePath ( makeRelative, (</>), takeDirectory, takeFileName ) -- filepath
import System.IO ( putStrLn, hPutStrLn, hPutStr, stderr, hClose, openBinaryTempFileWithDefaultPermissions ) -- base
import System.IO.Error ( userError, ioError, catchIOError, isUserError, ioeGetErrorString ) -- base
import Network.HTTP.Types.Status ( status200, status403, status404, status409 ) -- http-types
import Network.HTTP.Types.Method ( methodPost ) -- http-types
import Network.URI ( unEscapeString ) -- network-uri
import Network.Wai ( Application, Middleware, requestMethod, rawPathInfo, responseLBS ) -- wai
import qualified Network.Wai.Handler.Warp as Warp -- warp
import qualified Network.Wai.Handler.WarpTLS as Warp -- warp-tls
import Network.Wai.Middleware.AddHeaders ( addHeaders ) -- wai-extra
import Network.Wai.Middleware.Gzip ( gzip, gzipFiles, def, GzipFiles(GzipIgnore, GzipCompress) ) -- wai-extra
import Network.Wai.Middleware.HttpAuth ( basicAuth, AuthSettings ) -- wai-extra
import Network.Wai.Middleware.Local ( local ) -- wai-extra
import Network.Wai.Middleware.RequestLogger ( logStdout ) -- wai-extra
import Network.Wai.Middleware.Static ( staticPolicy, addBase, isNotAbsolute, noDots, Policy, tryPolicy ) -- wai-middleware-static
import Network.Wai.Middleware.StripHeaders ( stripHeadersIf ) -- wai-extra
import Network.Wai.Parse ( tempFileBackEndOpts, parseRequestBody, fileName, fileContent ) -- wai-extra

import Crypto.Random ( getSystemDRG, randomBytesGenerate, SystemDRG ) -- cryptonite

-- For certificate generation.
-- import Crypto.PubKey.RSA ( generate  ) -- crypto-pubkey
-- import Crypto.PubKey.RSA.PKCS15 ( sign ) -- crypto-pubkey
-- import Crypto.PubKey.HashDescr ( hashDescrSHA256 ) -- crypto-pubkey
-- import Data.ASN1.OID ( getObjectID ) -- asn1-types
-- import Data.ASN1.Types ( toASN1, {- for work-around -} ASN1Object ) -- asn1-types
-- import Data.ASN1.BinaryEncoding ( DER(DER) ) -- asn1-encoding
-- import Data.ASN1.Encoding ( encodeASN1' ) -- asn1-encoding
-- import qualified Data.PEM as PEM -- pem
-- import qualified Data.X509 as X509 -- x509
import qualified Data.Hourglass as HG -- hourglass
import qualified System.Hourglass as HG -- hourglass

-- for work-around
-- import Data.ASN1.Types ( ASN1(..), ASN1ConstructionType(..), ASN1TimeType(..) )
-- import Data.ASN1.Types.Lowlevel ( ASN1Class(..) )

-- For STUN
import Control.Concurrent ( forkIO, threadDelay ) -- base
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, tryTakeMVar ) -- base
import qualified Network.Socket as Net hiding ( sendTo, recvFrom ) -- network
import qualified Network.Socket.ByteString as Net ( sendTo, recvFrom ) -- network
import Network.BSD ( hostAddresses, getHostName, getHostByName ) -- network

-- Future things: case insensitive matching, optionally add CORS headers
-- Maybe future things: virtual hosts, caching, DELETE support, dropping permissions, client certificates
-- Not future things: CGI etc of any sort, "extensibility"
--
vERSION :: String
vERSION = "0.4.4.0"

-- STUN code

sendStun :: Options -> [Word8] -> Net.Socket -> IO ()
sendStun opts tId s = do -- TODO: Perhaps add check that length tId == 12
    [stunAddr] <- fmap (take 1 . hostAddresses) (getHostByName (optStunHost opts)) -- TODO: maybe have an option to list all addresses
    Net.sendTo s bytes (Net.SockAddrInet (optStunPort opts) stunAddr) >> return ()
  where bytes = BS.pack ([0x00, 0x01, 0x00, 0x00, -- Type Binding, Size 0
                          0x21, 0x12, 0xA4, 0x42] -- Magic Cookie
                         ++ tId) -- Transaction ID (should be cryptographically random and unique)

recvStun :: [Word8] -> Net.Socket -> IO [Word8]
recvStun tId s = do -- Assuming successful XOR-MAPPED-ADDRESS response.  See RFC5389. TODO: Don't assume so much.
    (bytes, addr) <- Net.recvFrom s 576
    -- TODO: Check for error, then, if successful, check for XOR-MAPPED-ADDRESS response type and appropriate length.
    let tId' = BS.unpack $ BS.take 12 $ BS.drop 8 bytes
    when (tId /= tId') $ ioError (userError "Mismatched Transaction ID in STUN response.")
    let [b0, b1, b2, b3] = BS.unpack $ BS.drop 28 bytes
    return [b0 `xor` 0x21, b1 `xor` 0x12, b2 `xor` 0xA4, b3 `xor` 0x42]

doStun :: Options -> [Word8] -> IO (Maybe [Word8]) -- TODO: add bracket
doStun opts tId = do
    s <- Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
    v <- newEmptyMVar
    forkIO (recvStun tId s >>= putMVar v)
    sendStun opts tId s
    threadDelay 1000000 -- wait a second
    Net.close s
    tryTakeMVar v

-- Certificate generation code

rsaPublicExponent :: Integer
rsaPublicExponent = 65537

rsaSizeInBytes :: Int
rsaSizeInBytes = 256 -- Corresponds to 2048 bit encryption

certExpiryInDays :: Int64
certExpiryInDays = 30

{-
-- Temporary work-around for bug in x509.
newtype CertificateWorkaround = CW X509.Certificate
    deriving ( Eq, Show )

encodeCertificateHeader :: X509.Certificate -> [ASN1]
encodeCertificateHeader cert =
    eVer ++ eSerial ++ eAlgId ++ eIssuer ++ eValidity ++ eSubject ++ epkinfo ++ eexts
  where eVer      = asn1Container (Container Context 0) [IntVal (fromIntegral $ X509.certVersion cert)]
        eSerial   = [IntVal $ X509.certSerial cert]
        eAlgId    = toASN1 (X509.certSignatureAlg cert) []
        eIssuer   = toASN1 (X509.certIssuerDN cert) []
        (t1, t2)  = X509.certValidity cert
        eValidity = asn1Container Sequence [ASN1Time TimeGeneralized t1 (Just (HG.TimezoneOffset 0))
                                           ,ASN1Time TimeGeneralized t2 (Just (HG.TimezoneOffset 0))]
        eSubject  = toASN1 (X509.certSubjectDN cert) []
        epkinfo   = toASN1 (X509.certPubKey cert) []
        eexts     = toASN1 (X509.certExtensions cert) []
        asn1Container ty l = [Start ty] ++ l ++ [End ty]

instance ASN1Object CertificateWorkaround where
    toASN1 (CW cert) = (encodeCertificateHeader cert ++)
-- End work-around code
-}

{-
generateCert :: Options -> HG.DateTime -> SystemDRG -> (Warp.TLSSettings, SystemDRG)
generateCert opts now g = ((Warp.tlsSettingsMemory (PEM.pemWriteBS pemCert) (PEM.pemWriteBS pemKey)) {
                                Warp.onInsecure = Warp.DenyInsecure (fromString "Use HTTPS") }, g'')
    where later = HG.timeAdd now (HG.Hours (24*certExpiryInDays))
          (bs, g') = randomBytesGenerate 8 g -- generate 8 random bytes for the serial number
          ((pk, sk), g'') = generate g' rsaSizeInBytes rsaPublicExponent
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
          certBytes = X509.encodeSignedObject $ fst $ X509.objectToSignedExact signFunc (CW cert)
          keyBytes = encodeASN1' DER (toASN1 sk [])
          pemCert = PEM.PEM (fromString "CERTIFICATE") [] certBytes  -- This is a mite silly.  Wrap in PEM just to immediately unwrap...
          pemKey = PEM.PEM (fromString "RSA PRIVATE KEY") [] keyBytes
-}

-- File upload

update :: Options -> Policy -> (String -> String -> IO ()) -> Middleware
update opts policy copyFileFn app req k = do
    if requestMethod req == methodPost then (do
        runResourceT $ do
            let prefix = if optUploadOnly opts then "" else CBS.unpack (BS.tail (rawPathInfo req))
            case tryPolicy policy prefix of
                Nothing -> liftIO $ ioError $ userError "Forbidden" -- TODO: k (responseLBS status403 [] (LBS.fromChunks [CBS.pack "Forbidden"]))
                Just tgtDir -> do
                    liftIO $ when (optVerbose opts) $ putStrLn (CBS.unpack (BS.tail (rawPathInfo req)))
                    (_, fs) <- withInternalState (\s -> parseRequestBody (tempFileBackEndOpts (return tgtDir) ".sws.tmp" s) req)
                    -- If UploadOnly then ignore the path part of the URL, i.e. only write the file to the base directory.
                    liftIO $ forM_ fs $ \(_, f) ->
                        case tryPolicy policy (prefix </> CBS.unpack (fileName f)) of
                            Nothing -> return ()
                            Just tgt -> do
                                let src = fileContent f
                                when (optVerbose opts) $ putStrLn ("Saving " ++ src ++ " to " ++ tgt)
                                copyFileFn src tgt
        app req k) -- We execute the next Application regardless so that we return a listing after the POST completes.
          `catch` (\e -> const (app req k) (e :: ResourceCleanupException)) -- HACK: tempFileBackEndOpts attempts to remove the temp file but we've already removed it.
          `catchIOError` \e -> if isUserError e then k (responseLBS status409 [] (LBS.fromChunks [CBS.pack $ ioeGetErrorString e])) else ioError e
      else
        app req k

overwriteFile :: String -> String -> IO ()
overwriteFile = renameFile -- copyFile

errorOnOverwriteFile :: String -> String -> IO ()
errorOnOverwriteFile src tgt = do -- TODO: This has a race condition.
    exists <- doesFileExist tgt
    if exists then do
        ioError $ userError "Attempting to overwrite an existing file."
      else do
        renameFile src tgt

renameOnOverwriteFile :: String -> String -> IO ()
renameOnOverwriteFile src tgt = do
    (tgt, h) <- openBinaryTempFileWithDefaultPermissions (takeDirectory tgt) (takeFileName tgt)
    hClose h
    renameFile src tgt

-- Directory listing

-- TODO: Make this less fugly.
directoryListing :: Options -> FilePath -> Middleware -- TODO: Handle exceptions.  Note, this isn't critical.  It will carry on.
directoryListing opts baseDir app req k = do
    let path = baseDir </> unEscapeString (CBS.unpack (BS.tail $ rawPathInfo req)) -- TODO: This unpack is ugly.
    b <- doesDirectoryExist path
    if not b then app req k else do
        when (optVerbose opts) $ putStrLn $ "Rendering listing for " ++ path
        html <- fmap container (mapM (\p -> fileDetails p (path </> p)) =<< fmap sort (getDirectoryContents path))
        k (responseLBS status200 [] html)
  where allowWrites = optAllowUploads opts
        fileDetails label f = liftA2 (renderFile label f) (doesDirectoryExist f) (getModificationTime f)
                                `catchIOError` \_ -> return (fromString "")
        renderFile label path isDirectory modTime = LBS.concat $ map fromString [
            "<tr><td>", if isDirectory then "d" else "f", "</td><td><a href=\"/", makeRelative baseDir path, "\">", label, "</a></td><td>", show modTime, "</td></tr>"
          ]
        container xs
          = fromString ("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta charset=\"utf-8\"><title>sws</title><style type=\"text/css\">a, a:active {text-decoration: none; color: blue;}a:visited {color: #48468F;}a:hover, a:focus {text-decoration: underline; color: red;}body {background-color: #F5F5F5;}h2 {margin-bottom: 12px;}table {margin-left: 12px;}th, td { font: 90% monospace; text-align: left;}th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}td {padding-right: 14px;}td.s, th.s {text-align: right;}div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;}div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}form { display: " ++ (if allowWrites then "inherit" else "none") ++ ";}</style></head><body><div class=\"list\"><table><tr><td></td><td>Name</td><td>Last Modified</td></tr>")
             <> LBS.concat xs
             <> fromString ("</table></div><form enctype=\"multipart/form-data\" method=\"post\" action=\"\">File: <input type=\"file\" name=\"file\" required=\"required\" multiple=\"multiple\"><input type=\"submit\" value=\"Upload\"></form><div class=\"foot\">sws" ++ vERSION ++ "</div></body></html>")

uploadForm :: Options -> Policy -> Middleware
uploadForm opts policy app req k = do
    when (optVerbose opts) $ putStrLn $ "Rendering upload form"
    k (responseLBS status200 [] html)
  where html = fromString ("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\"><html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><meta charset=\"utf-8\"><title>sws</title><style type=\"text/css\">a, a:active {text-decoration: none; color: blue;}a:visited {color: #48468F;}a:hover, a:focus {text-decoration: underline; color: red;}body {background-color: #F5F5F5;}h2 {margin-bottom: 12px;}table {margin-left: 12px;}th, td { font: 90% monospace; text-align: left;}th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}td {padding-right: 14px;}td.s, th.s {text-align: right;}div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;}div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}</style></head><body><form enctype=\"multipart/form-data\" method=\"post\" action=\"\">File: <input type=\"file\" name=\"file\" required=\"required\" multiple=\"multiple\"><input type=\"submit\" value=\"Upload\"></form><div class=\"foot\">sws" ++ vERSION ++ "</div></body></html>")

-- Option handling

data OverwriteOption = Overwrite | ErrorOnOverwrite | RenameOnOverwrite

instance Show OverwriteOption where
    show Overwrite = "allow"
    show ErrorOnOverwrite = "error"
    show RenameOnOverwrite = "rename"

instance Read OverwriteOption where
    readsPrec _ ('a':'l':'l':'o':'w':s) = [(Overwrite, s)]
    readsPrec _ ('e':'r':'r':'o':'r':s) = [(ErrorOnOverwrite, s)]
    readsPrec _ ('r':'e':'n':'a':'m':'e':s) = [(RenameOnOverwrite, s)]
    readsPrec _ _ = []

overwritePolicy :: OverwriteOption -> String -> String -> IO ()
overwritePolicy Overwrite = overwriteFile
overwritePolicy ErrorOnOverwrite = errorOnOverwriteFile
overwritePolicy RenameOnOverwrite = renameOnOverwriteFile

data Options = Options {
    optPort :: !Int,

    optHelp :: !Bool,
    optVerbose :: !Bool,
    optQuiet :: !Bool,

    optCompress :: !Bool,

    optDirectoryListings :: !Bool,

    optLocalOnly :: !Bool,

    optGetIP :: !Bool,
    optStunHost :: !String,
    optStunPort :: !Net.PortNumber,

    optHeaders :: ![String],

    -- Basic authentication options
    optAuthentication :: !Bool,
    optRealm :: !String,
    optUserName :: !BS.ByteString, -- some default could be chosen
    optPassword :: !BS.ByteString, -- maybe we could generate and display one rather than requiring this

    -- HTTPS options
    optHTTPS :: !Bool,
    optHost :: !String,
    optCertificate :: !FilePath,
    optKeyFile :: !FilePath,

    optAllowUploads :: !Bool,
    optUploadOnly :: !Bool,
    optOverwriteOption :: !OverwriteOption}

defOptions :: Options
defOptions = Options {
    optPort = 3000,
    optHelp = False,
    optVerbose = False, -- TODO: Set the logging settings for the various middlewares.
    optQuiet = False,
    optCompress = True,
    optDirectoryListings = True,
    optLocalOnly = False,
    optGetIP = True,
    optStunHost = "stun.l.google.com",
    optStunPort = 19302 ,
    optHeaders = [],
    optAuthentication = True,
    optRealm = "",
    optUserName = fromString "guest",
    optPassword = BS.empty,
    optHTTPS = False, --True,
    optHost = "localhost",
    optCertificate = "",
    optKeyFile = "",
    optAllowUploads = False,
    optUploadOnly = False,
    optOverwriteOption = ErrorOnOverwrite}

options :: [OptDescr (Options -> Options)]
options = [
    Option "p" ["port"] (ReqArg (\p opt -> opt { optPort = read p }) "NUM")
        ("Port to listen on. (Default: " ++ (show $ optPort defOptions) ++ ")"),
    Option "h?" ["help", "version"] (NoArg (\opt -> opt { optHelp = True }))
        "Print usage.",
    Option "V" ["verbose"] (NoArg (\opt -> opt { optVerbose = True }))
        "Print diagnostic output.",
    Option "q" ["quiet"] (NoArg (\opt -> opt { optQuiet = True }))
        "Only output access log information.",
    Option "l" ["local"] (NoArg (\opt -> opt { optLocalOnly = True }))
        "Only accept connections from localhost.",
    Option "" ["no-stun"] (NoArg (\opt -> opt { optGetIP = False }))
        "Don't attempt to get the public IP via STUN.",
    Option "" ["stun-host"] (ReqArg (\h opt -> opt { optStunHost = h }) "URL")
        ("Stun host. (Default: \"" ++ optStunHost defOptions ++ "\")"),
    Option "" ["stun-port"] (ReqArg (\p opt -> opt { optStunPort = read p }) "PORT")
        ("Stun port. Usually 3478. (Default: " ++ show (optStunPort defOptions) ++ ")"),
    Option "d" ["dev-mode"] (NoArg (\opt -> opt { optGetIP = False, optLocalOnly = True, optAuthentication = False, optHTTPS = False }))
        "Equivalent to --local --no-auth --no-https --no-stun.",
    Option "P" ["public"] (NoArg (\opt -> opt { optAuthentication = False, optHTTPS = False }))
        "Equivalent to --no-auth --no-https.",
    Option "X" [] (ReqArg (\h opt -> opt { optHeaders = h : optHeaders opt }) "HEADER")
        "Add HEADER to all server responses.",
    Option "z" ["gzip", "compress"] (NoArg (\opt -> opt { optCompress = True }))
        "Enable compression. (Default)",
    Option "" ["no-compress"] (NoArg (\opt -> opt { optCompress = False }))
        "Disable compression.",
    Option "" ["no-listings"] (NoArg (\opt -> opt { optDirectoryListings = False }))
        "Don't list directory contents.",
    Option "" ["no-auth"] (NoArg (\opt -> opt { optAuthentication = False }))
        "Don't require a password.",
    Option "r" ["realm"] (ReqArg (\r opt -> opt { optRealm = r }) "REALM")
        "Set the authentication realm. (Default: \"\")",
    Option "" ["password"] (ReqArg (\pw opt -> opt { optPassword = fromString pw }) "PASSWORD")
        "Require the given password. (Default: generated)",
    Option "u" ["username"] (ReqArg (\u opt -> opt { optUserName = fromString u }) "USERNAME")
        ("Require the given username. (Default: " ++ show (optUserName defOptions)  ++ ")"),
    {-
    Option "s" ["secure"] (NoArg (\opt -> opt { optHTTPS = True }))
        "Enable HTTPS. (Default)",
    Option "" ["no-https"] (NoArg (\opt -> opt { optHTTPS = False }))
        "Disable HTTPS.",
    -}
    Option "H" ["host"] (ReqArg (\host opt -> opt { optHost = host }) "HOST")
        ("Host name to use for generated certificate. (Default: " ++ show (optHost defOptions) ++ ")"),
    Option "" ["certificate"] (ReqArg (\f opt -> opt { optCertificate = f, optHTTPS = True }) "FILE")
        "The path to the server certificate.",
    Option "" ["key-file"] (ReqArg (\f opt -> opt { optKeyFile = f, optHTTPS = True }) "FILE")
        "The path to the private key for the certificate.",
    Option "w" ["allow-uploads", "writable"] (NoArg (\opt -> opt { optAllowUploads = True }))
        "Allow files to be uploaded.",
    Option "U" ["upload-only"] (NoArg (\opt -> opt { optUploadOnly = True }))
        "Only serve an upload form and do not serve any files.",
    Option "" ["overwrite"] (ReqArg (\overwriteOption opt -> opt { optOverwriteOption = read overwriteOption })
                                    (show Overwrite++","++show ErrorOnOverwrite++","++show RenameOnOverwrite))
        ("Policy when uploaded file name conflicts with existing file name. (Default: \"" ++ show (optOverwriteOption defOptions) ++ "\")")
 ]

-- TODO: KeyFile w/o Certificate and vice-versa, KeyFile/Certificate without HTTPS,
--       --no-auth and any of user/password/realm, header lacks ": ",
--       and whatever others make sense.
validateOptions :: Options -> Maybe String
validateOptions opts = Nothing

usageHeader :: String
usageHeader = "Usage: sws [OPTIONS...] [DIRECTORY]\nVersion: " ++ vERSION

basePolicy :: Policy
basePolicy = noDots <> isNotAbsolute

enableIf :: Bool -> (a -> a) -> a -> a
enableIf True f = f
enableIf    _ _ = id

app404 :: Application
app404 req k = k (responseLBS status404 [] (fromString "File Not Found"))

explodeHostAddress :: Net.HostAddress -> [Word8]
explodeHostAddress h = [fromIntegral h,
                        fromIntegral (h `unsafeShiftR` 8),
                        fromIntegral (h `unsafeShiftR` 16),
                        fromIntegral (h `unsafeShiftR` 24)]

prettyAddress :: Bool -> [Word8] -> Int -> String
prettyAddress isHTTPS [b0, b1, b2, b3] port
    = concat [if isHTTPS then "https://" else "http://", show b0, ".", show b1, ".", show b2, ".", show b3, ":", show port]

-- A "base 32" encode so we don't have differing case in the password.
base32Encode :: BS.ByteString -> BS.ByteString
base32Encode = let table = fromString "0123456789abcdefghijklmnopqrstuv"
                   go i | i == 0 = Nothing
                        | otherwise = Just (BS.index table (fromIntegral (i .&. 0x1F)), i `unsafeShiftR` 5)
    in \bs -> fst $ BS.unfoldrN 13 go $ BS.foldl' (\a w -> a*256 + fromIntegral w) (0 :: Integer) bs

serve :: Options -> String -> IO ()
serve (Options { optHelp = True }) _ = putStrLn $ usageInfo usageHeader options
serve opts dir = do
    now <- HG.dateCurrent
    g <- getSystemDRG
    let (prePW, g') = randomBytesGenerate 8 g -- generate 8 random bytes for the password if needed
        (stunTID, g'') = randomBytesGenerate 12 g' -- generate 12 random bytes for STUN Transaction ID
        pw = if not (BS.null (optPassword opts)) then optPassword opts else base32Encode prePW
        headers = map ((\(x,y) -> (x, BS.drop 2 y)) . BS.breakSubstring (fromString ": ") . fromString) (optHeaders opts)
    case validateOptions opts of
        Just err -> hPutStrLn stderr err
        Nothing -> do
            when (not $ optQuiet opts) $ do
                putStr "Private Address: "
                if optLocalOnly opts then do
                    putStrLn (prettyAddress (optHTTPS opts) [127,0,0,1] (optPort opts))
                  else do
                    hn <- getHostName
                    addrs <- fmap (take 1 . hostAddresses) (getHostByName hn) -- TODO: maybe have an option to list all addresses
                    forM_ addrs $ \ip -> putStrLn (prettyAddress (optHTTPS opts) (explodeHostAddress ip) (optPort opts))

                when (optGetIP opts && not (optLocalOnly opts)) $ do
                    mip <- doStun opts (BS.unpack stunTID)
                    case mip of
                        Nothing -> hPutStrLn stderr "Finding public IP failed."
                        Just ip -> do
                            putStr "Public Address: "
                            putStrLn (prettyAddress (optHTTPS opts) ip (optPort opts))

                when (optAuthentication opts && not (BS.null (optUserName opts))) $
                    putStrLn $ "Username is: " ++ CBS.unpack (optUserName opts)
                when (optAuthentication opts && BS.null (optPassword opts)) $ do
                    putStrLn $ "Generated password is: " ++ CBS.unpack pw
                    putStrLn "Use --no-auth if password protection is not desired."

            runner now g''
                $ enableIf (optVerbose opts) logStdout
                $ enableIf (optLocalOnly opts) (local (responseLBS status403 [] LBS.empty))
                $ enableIf (optAuthentication opts)
                    (basicAuth (\u p -> return $ optUserName opts == u && pw == p)
                               (fromString $ optRealm opts))
                $ enableIf (optCompress opts) (gzip def { gzipFiles = GzipCompress })
                $ enableIf (not (null headers)) (addHeaders headers . stripHeadersIf (map fst headers) (const True))
                $ enableIf (optAllowUploads opts || optUploadOnly opts) (update opts policy (overwritePolicy (optOverwriteOption opts)))
                $ (if optUploadOnly opts then uploadForm opts policy else staticPolicy policy)
                $ enableIf (optDirectoryListings opts) (directoryListing opts dir)
                $ app404
  where runner now g | optHTTPS opts && certProvided
                        = Warp.runTLS tlsFileSettings (Warp.setPort (optPort opts) Warp.defaultSettings)
                     -- | optHTTPS opts = \app -> do
                     --    when (not $ optQuiet opts) $ do
                     --        putStrLn "Generating a self-signed certificate.  Use --no-https to disable HTTPS."
                     --        putStrLn "Users will get warnings and will be vulnerable to man-in-the-middle attacks."
                     --    Warp.runTLS tlsMemSettings (Warp.setPort (optPort opts) Warp.defaultSettings) app
                     | otherwise = Warp.run (optPort opts)
            where tlsFileSettings = (Warp.tlsSettings (optCertificate opts) (optKeyFile opts)) {
                        Warp.onInsecure = Warp.DenyInsecure (fromString "Use HTTPS") }
                  -- (tlsMemSettings, _) = generateCert opts now g
                  certProvided = not (null (optCertificate opts)) && not (null (optKeyFile opts))

        policy = basePolicy <> addBase dir

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute options args of
        (os, [], []) -> serve (combine os) "."
        (os, [dir], []) -> serve (combine os) dir
        (_,_,errs) -> hPutStrLn stderr (concat errs ++ usageInfo usageHeader options)
  where combine = foldr ($) defOptions
