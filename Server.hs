-- TODO Add an authentication endpoint, and support token-based authentication.
-- TODO Add support for HTTP Basic Authentication.
--     TODO Are there any libraries to handle this more easily?
--     TODO Read about padding schemes.
--     TODO Define sensible limits on username and password length.
--         TODO Enforce them.
--     TODO Come up with a sane way to handle passing in encryption keys at
--         run time or compile time.
-- TODO Better Response Codes
--     200 OK, 201 Created, 204 No Content
--     400 Bad Request, 404 Not Found, 403 Forbidden, 401 Unauthorized

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

import Prelude.Unicode
import Data.Acid
import Data.Set (Set)
import Data.Word
import qualified Data.Set as Set
import Control.Exception (finally)
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import qualified Network.URI as URI
import Data.SafeCopy
import Data.Typeable
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as W
import qualified Network.HTTP.Types.Status as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Aeson as J
import qualified Web.ClientSession as Session
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


-- [[Misc]]
toLazy ∷ BS.ByteString → LBS.ByteString
toLazy s = LBS.fromChunks [s]

unLazy ∷ LBS.ByteString → BS.ByteString
unLazy = BS.concat ∘ LBS.toChunks

unsafeToken ∷ (User, Password, Date) → BS.ByteString
unsafeToken (u,p,d) = unLazy $ J.encode (u,p,d)

fromUnsafeToken ∷ BS.ByteString → Maybe (User, Password, Date)
fromUnsafeToken tok = J.decode $ toLazy tok

mkToken ∷ Session.Key → Session.IV → (User, Password, Date) → Text
mkToken key iv upd = T.decodeUtf8 $ Session.encrypt key iv $ unsafeToken upd

decToken ∷ Session.Key → BS.ByteString → Maybe (User, Password, Date)
decToken key tok = case Session.decrypt key tok of
    Nothing → Nothing
    Just x → fromUnsafeToken x

decodeURIComponent ∷ Text → Text
decodeURIComponent = T.pack ∘ URI.unEscapeString ∘ T.unpack


-- [[Data]]
type Date = Word64
type URL = Text
type User = Text
type Password = Text
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


-- [[Database]]
delURL_ ∷ User → URL → Update Data ()
delURL_ user url = liftQuery ask >>= put ∘ delURL user url

addURL_ ∷ User → URL → Update Data ()
addURL_ user url = liftQuery ask >>= put ∘ addURL user url

setURLs_ ∷ User → Set URL → Update Data ()
setURLs_ user urls = liftQuery ask >>= put ∘ setURLs user urls

queryData ∷ Query Data Data
queryData = ask

$(deriveSafeCopy 1 'base ''Data)
$(makeAcidic ''Data ['queryData, 'delURL_, 'addURL_, 'setURLs_])

getData ∷ AcidState Data -> IO Data
getData db = query db QueryData


-- [[Application Logic]]
data Resp = NotOk | Ok | URLs (Set URL) | ServeClient
    deriving Show

data Req
    = BadReq
    | WebClient
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
decRequest ∷ W.Request → IO Req
decRequest r = do
    body ← W.lazyRequestBody r
    let putURL user encUrl = AddURL user $ decodeURIComponent encUrl
    let putURLs user = fromMaybe BadReq $ fmap (SetURLs user) $ J.decode body
    return $ case (W.pathInfo r, W.requestMethod r, W.queryString r) of
        ([], "GET", []) → WebClient
        (["index.html"], "GET", []) → WebClient
        (["api", "users",user, "urls",url], "PUT", []) → putURL user url
        (["api", "users",user, "urls"], "GET", []) → GetURLs user
        (["api", "users",user, "urls"], "PUT", []) → putURLs user
        (["api", "users",user, "urls"], "DELETE", []) → SetURLs user Set.empty
        (["api", "users",user, "urls",url], "DELETE", []) → DelURL user url
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
    putStrLn(show req)
    resp ← respond db req
    putStrLn(show resp)
    return $ encResponse resp

main ∷ IO()
main = do
    db ← openLocalState (Data M.empty)
    finally (W.run 8080 $ app db) $ do
        createCheckpoint db
        closeAcidState db
