-- TODO Add support for registrations.
-- TODO Implement authorization.
-- TODO Add an authentication endpoint, and support token-based authentication.
-- TODO Get encryption keys from environment variables.
-- TODO Properly hash the passwords, and don't store them as plaintext.
-- TODO What's the correct HTTP error code for a rejected authentication?

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

import ClassyPrelude
import Prelude.Unicode
import qualified Data.Set as Set
import qualified Network.URI as URI
import qualified Data.Map as M
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Aeson as J
import qualified Web.ClientSession as Session
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Acid
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Data.SafeCopy
import qualified Data.ByteString.Base64 as Base64


-- [[Authentication Stuff]]
data AuthParse = Anon | Malformed | AuthAttempt Text User Password
type Registrations = Map User Password

userPassFromBasicAuthString ∷ Text → Maybe (Text,Text)
userPassFromBasicAuthString s = case Base64.decode $ encodeUtf8 s of
    Left _ → Nothing
    Right pair → case T.breakOn ":" $ decodeUtf8 pair of
        (_,"") → Nothing
        (user,colonPass) → Just (user, T.tail colonPass)

unPrefix ∷ Text → Text → Maybe Text
unPrefix prefix str = case T.splitAt (length prefix) str of
    (strHead, strTail) → if strHead≠prefix then Nothing else Just strTail

parseAuthorization ∷ ByteString → Maybe (Text,Text)
parseAuthorization =
    join∘fmap userPassFromBasicAuthString∘unPrefix "Basic "∘decodeUtf8

parseWWWAuthenticate ∷ ByteString → Maybe Text
parseWWWAuthenticate s = case unPrefix "Basic realm=\"" $ decodeUtf8 s of
    Nothing → Nothing
    Just "" → Nothing
    Just(shit∷Text) → case T.splitAt (length shit-1) shit of
        (r,"\"") → Just r
        _ → Nothing

authAttempt ∷ W.Request → AuthParse
authAttempt req = f where
    hdrs = W.requestHeaders req
    parse a b = case (parseWWWAuthenticate a, parseAuthorization b) of
        (Just aRealm, Just(uname,p)) → case userFromName uname of
            Nothing → Malformed
            Just u → AuthAttempt aRealm u p
        _ → Malformed
    f = case (lookup "WWW-Authenticate" hdrs, lookup "Authorization" hdrs) of
        (Nothing, Nothing) → Anon
        (Just a, Just b) → parse a b
        _ → Malformed

authFailed ∷ IO W.Response
authFailed = return $ W.responseLBS W.status403 [] ""

-- This rejects requests with invalid credentials. Any requests that pass
-- through this filter will either have no credentials or correct credentials.
basicAuth ∷ Registrations → AcidState Data → W.Application → W.Application
basicAuth users _ waiapp req = case authAttempt req of
    Anon → waiapp req
    Malformed → authFailed
    AuthAttempt _ u p → case M.lookup u users of
        Nothing → authFailed
        Just(password) → if password≡p then waiapp req else
            return $ W.responseLBS W.status400 [] ""


-- [[Misc]]
mkToken ∷ Session.Key → Session.IV → (User, Password, Date) → Text
mkToken key iv (u,p,d) = decodeUtf8 $ Session.encrypt key iv $ rawtok where
    rawtok = BS.concat $ LBS.toChunks $ J.encode (username u,p,d)

decToken ∷ Session.Key → BS.ByteString → Maybe (User, Password, Date)
decToken key tok = join $ fmap fromRawTok $ Session.decrypt key tok where
    fromRawTok rawtok = case J.decode $ LBS.fromChunks [rawtok] of
        Nothing → Nothing
        Just (uname,p,d) → case userFromName uname of
            Nothing → Nothing
            Just u → Just (u,p,d)


-- [[Data]]
type Date = Word64
type URL = Text
type Password = Text
newtype User = User Text deriving (Eq, Ord, Show, Typeable)
data Data = Data(Map User (Set URL)) deriving (Show, Typeable)

delURL ∷ User → URL → Data → Data
delURL user url (Data d) = Data(M.alter f user d) where
    f row = Just $ Set.delete url $ fromMaybe Set.empty row

addURL ∷ User → URL → Data → Data
addURL user url (Data d) = Data(M.alter (Just∘f) user d) where
    f row = Set.insert url $ fromMaybe Set.empty row

setURLs ∷ User → Set URL → Data → Data
setURLs user urls (Data d) = Data $ M.insert user urls d

getURLs ∷ User → Data → Set URL
getURLs user (Data d) = fromMaybe Set.empty $ M.lookup user d

validUsername ∷ Text → Bool
validUsername t = lenOK && okCharset where
    lenOK = length t≤50 && length t>0
    (az,az09) = (['a'..'z']++['A'..'Z'], az++['0'..'9'])
    okCharset = case T.uncons t of
        Nothing → False
        Just(h,tail) → h `elem` az && T.all (`elem` az09) tail

username ∷ User → Text
username (User name) = name

userFromName ∷ Text → Maybe User
userFromName t = if validUsername t then Just $ User t else Nothing


-- [[Database]]
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 2 'base ''Data)

delURL_ ∷ User → URL → Update Data ()
delURL_ user url = liftQuery ask >>= put ∘ delURL user url

addURL_ ∷ User → URL → Update Data ()
addURL_ user url = liftQuery ask >>= put ∘ addURL user url

setURLs_ ∷ User → Set URL → Update Data ()
setURLs_ user urls = liftQuery ask >>= put ∘ setURLs user urls

queryData ∷ Query Data Data
queryData = ask

$(makeAcidic ''Data ['queryData, 'delURL_, 'addURL_, 'setURLs_])

getData ∷ AcidState Data → IO Data
getData db = query db QueryData


-- [[Application Logic]]
data Resp = NotOk | Ok | URLs (Set URL) | ServeClient deriving Show
data Req
    = BadReq | WebClient
    | AddURL User URL | DelURL User URL | GetURLs User | SetURLs User (Set URL)
    deriving Show

respond ∷ AcidState Data → Req → IO Resp
respond db req = case req of
    BadReq → return NotOk
    AddURL user url → update db (AddURL_ user url) >> return Ok
    GetURLs user → getData db >>= (return ∘ URLs ∘ getURLs user)
    DelURL user url → update db (DelURL_ user url) >> return Ok
    SetURLs user urls → update db (SetURLs_ user urls) >> return Ok
    WebClient → return ServeClient


-- [[HTTP Requests and Responses]]
decodeURIComponent ∷ Text → Text
decodeURIComponent = pack ∘ URI.unEscapeString ∘ unpack

decRequest ∷ W.Request → IO Req
decRequest r = do
    let notOK="This code assumes that usernames/passwords are valid and correct"
    let usr uname = case userFromName uname of{Just u→u; Nothing→error notOK}
    body ← W.lazyRequestBody r
    let putURL user encUrl = AddURL user $ decodeURIComponent encUrl
    let putURLs user = fromMaybe BadReq $ fmap (SetURLs user) $ J.decode body
    return $ case (W.pathInfo r, W.requestMethod r, W.queryString r) of
        ([],"GET",[]) → WebClient
        (["index.html"],"GET",[]) → WebClient
        (["api","users",user,"urls",url],"PUT",[]) → putURL (usr user) url
        (["api","users",user,"urls"],"GET",[]) → GetURLs (usr user)
        (["api","users",user,"urls"],"PUT",[]) → putURLs (usr user)
        (["api","users",user,"urls"],"DELETE",[]) → SetURLs (usr user) Set.empty
        (["api","users",user,"urls",url],"DELETE",[]) → DelURL (usr user) url
        _ → BadReq

encResponse ∷ Resp → W.Response
encResponse resp = r resp where
    r NotOk = W.responseLBS W.status404 [] ""
    r Ok = W.responseLBS W.status204 [] ""
    r ServeClient = W.responseFile W.status200 html "./index.html" Nothing where
    r (URLs urls) = (W.responseLBS W.status200 json $ J.encode urls) where
    html = [("Content-Type", "text/html")]
    json = [("Content-Type", "application/javascript")]

app ∷ AcidState Data → W.Request → IO W.Response
app db webreq = do
    req ← decRequest webreq
    putStrLn "=========="
    putStrLn $ pack $ show req
    resp ← respond db req
    putStrLn $ pack $ show resp
    return $ encResponse resp

main ∷ IO()
main = do
    db ← openLocalState (Data M.empty)
    finally (W.run 8080 $ basicAuth M.empty db $ app db) $ do
        createCheckpoint db
        closeAcidState db
