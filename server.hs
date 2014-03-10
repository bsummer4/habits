-- TODO Complain about attempts to delete non-existant urls.
-- TODO Better Response Codes
--   200 OK, 201 Created, 204 No Content
--   400 Bad Request, 404 Not Found, 403 Forbidden, 401 Unauthorized
-- TODO Support PUT requests on a user to set the URL list with a JSON array.
-- TODO Write a simple client.
-- TODO Add support for registration, and auth using HTTP Basic Authentication.

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

import Prelude.Unicode
import Data.Acid
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Data.SafeCopy
import Data.Typeable
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


-- [[Data]]
type URL = Text
type User = Text
data Data = Data(Map User [URL]) deriving (Show, Typeable)

delURL ∷ User → URL → Data → Data
delURL user url (Data d) = Data(M.alter f user d) where
    f row = Just $ filter (/= url) $ fromMaybe [] row

addURL ∷ User → URL → Data → Data
addURL user url (Data d) = Data(M.alter (Just∘f) user d) where
    f row = case row of {Nothing→[url]; (Just urls)→url:urls}

getURLs ∷ User → Data → [URL]
getURLs user (Data d) = fromMaybe [] $ M.lookup user d


-- [[Database]]
type DB = AcidState Data

writeData ∷ Data -> Update Data ()
writeData (Data db) = put (Data db)

queryData ∷ Query Data Data
queryData = ask

$(deriveSafeCopy 0 'base ''Data)
$(makeAcidic ''Data ['writeData, 'queryData])
-- ‘makeAcidicُ’ creates the event constructors ‘QueryData’ and ‘WriteData.’

getData ∷ DB -> IO Data
getData db = query db QueryData

modifyData ∷ DB -> (Data -> Data) -> IO()
modifyData db f = getData db >>= (update db∘WriteData∘f) >> return()


-- [[Application Logic]]
data Resp = NotOk | Ok | URLs [URL]
    deriving Show

data Req = InvalidReq | AddURL User URL | DelURL User URL | GetURLs User
    deriving Show

respond ∷ DB → Req → IO Resp
respond db req = case req of
    InvalidReq → return NotOk
    AddURL user url → modifyData db (addURL user url) >> return Ok
    GetURLs user → getData db >>= (return ∘ URLs ∘ getURLs user)
    DelURL user url → modifyData db (delURL user url) >> return Ok


-- [[HTTP Requests and Responses]]
decRequest ∷ W.Request → Req
decRequest r =
    case (W.pathInfo r, W.requestMethod r, W.queryString r) of
        (["api", user], "GET", []) → GetURLs user
        (["api",user], "POST", [("url",Just u)]) → AddURL user $ T.decodeUtf8 u
        (["api",user,url], "DELETE", []) → DelURL user url
        _ → InvalidReq

encResponse ∷ Resp → W.Response
encResponse NotOk = W.responseLBS W.status404 [] ""
encResponse Ok = W.responseLBS W.status204 [] ""
encResponse (URLs urls) = (W.responseLBS W.status200 json $ J.encode urls) where
    json = [("Content-Type", "application/javascript")]

app ∷ DB → W.Request → IO W.Response
app db webreq = do
    let req = decRequest webreq
    putStrLn(show req)
    resp ← respond db req
    putStrLn(show resp)
    getData db >>= putStrLn ∘ show
    return $ encResponse resp

main ∷ IO()
main = openLocalState (Data M.empty) >>= W.run 5005 ∘ app
