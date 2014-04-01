{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO Making DB.User Opaque is sort of silly since we can J.encode it.

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Prelude (read)
import System.Environment (getEnv)
import Control.Monad
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J
import qualified Data.Map as Map
import qualified Data.Text.Read as Text
import qualified State as DB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as ASCII
import qualified Web.ClientSession as ClientSession

data Tok = Tok Text
data Resp
  = OK
  | MALFORMED_REQUEST | NOT_FOUND | USER_ALREADY_EXISTS | NOT_ALLOWED
  | AUTH Tok
  | STATUSES (Map DB.Habit DB.HabitStatus)
  | HABITS (Set DB.Habit)
  | CHAINS (Map DB.Habit Int)

data Req
  = Register DB.User DB.Password
  | GetHabitsStatus Tok DB.User Day
  | SetHabitsStatus Tok DB.User Day DB.Habit DB.HabitStatus
  | GetChains Tok DB.User Day
  | AddHabit Tok DB.User DB.Habit
  | DelHabit Tok DB.User DB.Habit
  | ListHabits Tok DB.User

deriving instance Show Tok
deriving instance Show Resp
deriving instance Show Req

$(J.deriveJSON J.defaultOptions ''Tok)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''Req)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''Resp)

instance J.ToJSON Day where
  toJSON (ModifiedJulianDay x) = J.toJSON x

instance J.FromJSON Day where
  parseJSON x = J.parseJSON x >>= return∘ModifiedJulianDay

instance J.ToJSON α ⇒ J.ToJSON (Map DB.Habit α) where
  toJSON x = J.toJSON $ Map.mapKeys DB.habitText x

instance J.FromJSON α ⇒ J.FromJSON (Map DB.Habit α) where
  parseJSON o = do
    x ∷ Map Text α ← J.parseJSON o
    let mb (mk,v) = case mk of {Nothing→Nothing; Just k→Just(k,v)}
    case sequence $ map mb $ Map.toList $ Map.mapKeys DB.textHabit x of
      Nothing → mzero
      Just kvs → return $ Map.fromList kvs


-- [[Authentication and Authorization]]
genToken ∷ DB.User → DB.Password → IO Tok
genToken u p = do
	k ← ClientSession.getDefaultKey
	t ← ClientSession.encryptIO k $ unlazy $ J.encode(u,p)
	return $ Tok $ decodeUtf8 t

tokenUserMatch ∷ AcidState DB.State → Tok → DB.User → IO Bool
tokenUserMatch db (Tok t) user = do
	k ← ClientSession.getDefaultKey
	let tokjson = ClientSession.decrypt k $ encodeUtf8 t
	let up = join $ fmap (J.decode ∘ tolazy) tokjson
	putStrLn $ pack $ show k
	putStrLn $ pack $ show tokjson
	putStrLn $ pack $ show up
	case (up ∷ Maybe (DB.User,DB.Password)) of
		Nothing → return False
		Just (u,p) → do
			okpass ← query db $ DB.Authenticate u p
			return $ okpass && u≡user

authorized ∷ AcidState DB.State → Req → IO Bool
authorized db r = let check t u = tokenUserMatch db t u in
  case r of
    Register _ _ → return True
    GetHabitsStatus t u _ → check t u
    SetHabitsStatus t u _ _ _ → check t u
    GetChains t u _ → check t u
    AddHabit t u _ → check t u
    DelHabit t u _ → check t u
    ListHabits t u → check t u


-- [[HTTP Requests and Responses]]
tolazy ∷ BS.ByteString → LBS.ByteString
tolazy b = LBS.fromChunks [b]

unlazy ∷ LBS.ByteString → BS.ByteString
unlazy = BS.concat ∘ LBS.toChunks

mkDay ∷ Text → Maybe Day
mkDay t = case Text.decimal t of
  Left _ → Nothing
  Right (v,_) → Just(ModifiedJulianDay v)

respond ∷ AcidState DB.State → Req → IO Resp
respond db req = do
  ok ← authorized db req
  if not ok then return NOT_ALLOWED else case req of
    Register user pass → do
      _ ← DB.register db user pass
      tok ← genToken user pass
      return $ AUTH tok
    ListHabits _ user → do
      mhabits ← query db $ DB.UserHabits user
      return $ fromMaybe NOT_FOUND $ fmap HABITS $ mhabits
    GetHabitsStatus _ user day → do
      mstatuses ← query db $ DB.HabitsStatus user day
      return $ fromMaybe NOT_FOUND $ fmap STATUSES $ mstatuses
    GetChains _ user day → do
      mchains ← query db (DB.Chains user day)
      return $ fromMaybe NOT_FOUND $ fmap CHAINS $ mchains
    AddHabit _ user habit → do
      _ ← update db (DB.AddHabit user habit)
      return OK
    DelHabit _ user habit → do
      _ ← update db (DB.DelHabit user habit)
      return OK
    SetHabitsStatus _ user day habit status → do
      _ ← update db $ DB.SetHabitStatus user day habit status
      return OK

app ∷ AcidState DB.State → W.Request → IO W.Response
app db webreq = case W.requestMethod webreq of
  "GET" → return $ W.responseFile W.status200 html "./index.html" Nothing
    where html = [("Content-Type", "text/html")]
  _ → do
    -- ClassyPrelude.mapM_ (ASCII.putStrLn ∘ J.encode) [lh,ghs,shs]
    body ← W.lazyRequestBody webreq
    ASCII.putStrLn body
    resp ← case J.decode body of
      Nothing → return MALFORMED_REQUEST
      Just req →
        putStrLn "==========" >> (putStrLn $ pack $ show req) >> respond db req
    ASCII.putStrLn $ J.encode resp
    let json = [("Content-Type", "application/javascript")]
    return $ W.responseLBS W.status200 json $ J.encode resp where

-- Just isan = DB.textUser "isan"
-- pw = "pw"
-- Just today = mkDay "34987234"
-- Just hab = DB.textHabit "pizza"
-- shs = SetHabitsStatus (Tok "isan pw") isan today hab (DB.Success Nothing)
-- ghs = GetHabitsStatus (Tok "isan pw") isan today
-- lh = ListHabits (Tok "isan pw") isan

main ∷ IO()
main = do
  let tlsOpts = W.defaultTlsSettings
  port ← getEnv "PORT" >>= return∘read
  let warpOpts = W.setPort port W.defaultSettings
  db ← openLocalState DB.emptyState
  finally (W.runTLS tlsOpts warpOpts $ app db) $ do
    createCheckpoint db
    closeAcidState db
