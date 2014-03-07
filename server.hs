-- TODO Complain about attempts to delete non-existant urls.
-- TODO Better Response Codes
--   200 OK, 201 Created, 204 No Content
--   400 Bad Request, 404 Not Found, 403 Forbidden, 401 Unauthorized
-- TODO Support PUT requests on a user to set the URL list with a JSON array.
-- TODO Actually parse out the '?url=URL' query on POST requests.
-- TODO Write a simple client.

{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude.Unicode
import Control.Concurrent.MVar
import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import Data.SafeCopy
import Data.Typeable
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
-- ‘makeAcidicُ’ creates the constructors ‘QueryData’ and ‘WriteData.’

getData ∷ DB -> IO Data
getData db = query db QueryData

modifyData ∷ DB -> (Data -> Data) -> IO()
modifyData db f = getData db >>= (update db∘WriteData∘f) >> return()


-- [[Application Logic]]
data Req = AddURL User URL | DelURL User URL | GetURLs User
data Resp = Ok | URLs [URL]

respond ∷ DB → Req → IO Resp
respond db req = case req of
	AddURL user url → modifyData db (addURL user url) >> return Ok
	GetURLs user → getData db >>= (return ∘ URLs ∘ getURLs user)
	DelURL user url → modifyData db (delURL user url) >> return Ok


-- [[HTTP Requests and Responses]]
json = [("Content-Type", "application/javascript")]∷W.RequestHeaders

reqContentType ∷ W.Request → Maybe Text
reqContentType = fmap T.decodeUtf8 ∘ lookup "content-type" ∘ W.requestHeaders

urlFromQuery ∷ ByteString → URL
urlFromQuery q = T.decodeUtf8 q

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
encResponse (Just (URLs us)) = W.responseLBS W.status200 json $ J.encode us

app ∷ DB → W.Request → IO W.Response
app db webreq = do
	getData db >>= putStrLn∘("STATE: "++)∘show
	let req = decRequest webreq
	resp ← case req of
		Nothing → return Nothing
		Just r → respond db r >>= return∘Just
	return $ encResponse resp

main ∷ IO()
main = do
	db ← openLocalState (Data M.empty)
	W.run 5005 $ app (db∷DB)
