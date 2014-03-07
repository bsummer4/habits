-- TODO Complain about attempts to delete non-existant urls.
-- TODO Better Response Codes
--   200 OK, 201 Created, 204 No Content
--   400 Bad Request, 404 Not Found, 403 Forbidden, 401 Unauthorized
-- TODO Support PUT requests on a user to set the URL list with a JSON array.
-- TODO Actually parse out the '?url=URL' query on POST requests.
-- TODO Write a simple client.

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}

import Prelude.Unicode
import Control.Concurrent.MVar
import Data.Maybe
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

-- [[Application Logic]]
type URL = Text
type User = Text
data Req = AddURL User URL | DelURL User URL | GetURLs User
data Resp = Ok | Urls [URL]
type DB = Map User [URL]

addUrl ∷ URL → Maybe [URL] → [URL]
addUrl url row = case row of {Nothing→[url]; (Just urls)→url:urls}

delUrl ∷ URL → Maybe [URL] → [URL]
delUrl url Nothing = []
delUrl url (Just row) = filter (/= url) row

respond ∷ MVar DB → Req → IO Resp
respond db req = case req of
	AddURL user url → modifyMVar db $ \t →
		return (M.alter (Just∘addUrl url) user t, Ok)
	GetURLs user → do
		userT ← readMVar db
		return $ Urls $ fromMaybe [] $ M.lookup user userT
	DelURL user url → modifyMVar db $ \t →
		return (M.alter (Just∘delUrl url) user t, Ok)


-- [[HTTP Requests and Responses]]
json = [("Content-Type", "application/javascript")]∷W.RequestHeaders

reqContentType ∷ W.Request → Maybe Text
reqContentType = fmap T.decodeUtf8 ∘ lookup "content-type" ∘ W.requestHeaders

urlFromQuery ∷ ByteString → URL
urlFromQuery q = T.decodeUtf8 q -- TODO Actually extract the url from the query.

decRequest ∷ W.Request → Maybe Req
decRequest r =
	case (W.pathInfo r, W.requestMethod r, W.rawQueryString r) of
		([user],"GET","") → Just $ GetURLs user
		([user],"POST",q) → Just $ AddURL user $ urlFromQuery q
		([user,url],"DELETE","") → Just $ DelURL user url
		_ → Nothing

encResponse ∷ Maybe Resp → W.Response
encResponse Nothing = W.responseLBS W.status404 [] ""
encResponse (Just Ok) = W.responseLBS W.status204 [] ""
encResponse (Just (Urls us)) = W.responseLBS W.status200 json $ J.encode us

app ∷ MVar DB → W.Request → IO W.Response
app db webreq = do
	let req = decRequest webreq
	resp ← case req of
		Nothing → return Nothing
		Just r → respond db r >>= return∘Just
	return $ encResponse resp

main ∷ IO()
main = do
	db ← newMVar(M.empty∷DB)
	W.run 5003 $ app db
