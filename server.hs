-- TODO Write a simple client.
-- TODO Add support for registration, and auth using HTTP Basic Authentication.
-- TODO Better Response Codes
--   200 OK, 201 Created, 204 No Content
--   400 Bad Request, 404 Not Found, 403 Forbidden, 401 Unauthorized

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

import Prelude.Unicode
import Data.Acid
import Data.Set (Set)
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
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as W
import qualified Network.HTTP.Types.Status as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Aeson as J


-- [[URL Encoding]]
decodeURIComponent = T.pack ∘ URI.unEscapeString ∘ T.unpack


-- [[Data]]
type URL = Text
type User = Text
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
type DB = AcidState Data

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
-- ‘makeAcidicُ’ creates the event constructors:
    -- QueryData DelURL_ AddURL_ SetURLs_

getData ∷ DB -> IO Data
getData db = query db QueryData


-- [[Application Logic]]
data Resp = NotOk | Ok | URLs (Set URL)
    deriving Show

data Req
    = InvalidReq
    | AddURL User URL | DelURL User URL | GetURLs User | SetURLs User (Set URL)
        deriving Show

respond ∷ DB → Req → IO Resp
respond db req = case req of
    InvalidReq → return NotOk
    AddURL user url → update db (AddURL_ user url) >> return Ok
    GetURLs user → getData db >>= (return ∘ URLs ∘ getURLs user)
    DelURL user url → update db (DelURL_ user url) >> return Ok
    SetURLs user urls → update db (SetURLs_ user urls) >> return Ok


-- [[HTTP Requests and Responses]]
decRequest ∷ W.Request → IO Req
decRequest r = do
    body ← W.lazyRequestBody r
    return $ case (W.pathInfo r, W.requestMethod r, W.queryString r) of
        (["api", "users",user, "urls",url], "PUT", []) →
            AddURL user $ decodeURIComponent $ url
        (["api", "users",user, "urls"], "GET", []) →
            GetURLs user
        (["api", "users",user, "urls"], "PUT", []) →
            fromMaybe InvalidReq $ fmap (SetURLs user) $ J.decode body
        (["api", "users",user, "urls"], "DELETE", []) →
            SetURLs user Set.empty
        (["api", "users",user, "urls",url], "DELETE", []) →
            DelURL user url
        _ → InvalidReq

encResponse ∷ Resp → W.Response
encResponse repo = case resp of
    NotOk → W.responseLBS W.status404 [] ""
    Ok → W.responseLBS W.status204 [] ""
    (URLs urls) → (W.responseLBS W.status200 json $ J.encode urls) where
        json = [("Content-Type", "application/javascript")]

app ∷ DB → W.Request → IO W.Response
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
