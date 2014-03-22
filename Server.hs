{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Prelude (read)
import System.Environment (getEnv)
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Data.Aeson as J
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import qualified Data.ByteString.Base64 as Base64
import qualified State as DB

data AuthParse = Anon | Malformed | AuthAttempt Text DB.User DB.Password
data Resp
  = NotOk | Ok | ServeClient | Habits (Set DB.Habit) | Chains (Map DB.Habit Int)

data Req
  = BadReq | WebClient
  | GetSuccesses Day | GetFailures Day
  | GetChains Day | SetDone Day DB.Habit Bool
  | AddHabit DB.Habit | DelHabit DB.Habit | ListHabits

deriving instance Show Resp
deriving instance Show Req


-- [[Authentication]]
userPassFromBasicAuthString ∷ Text → Maybe (Text,Text)
userPassFromBasicAuthString s = case Base64.decode $ encodeUtf8 s of
  Left _ → Nothing
  Right pair → case Text.breakOn ":" $ decodeUtf8 pair of
    (_,"") → Nothing
    (user,colonPass) → Just (user, Text.tail colonPass)

unPrefix ∷ Text → Text → Maybe Text
unPrefix prefix str = case Text.splitAt (length prefix) str of
  (strHead, strTail) → if strHead≠prefix then Nothing else Just strTail

parseAuthorization ∷ ByteString → Maybe (Text,Text)
parseAuthorization =
  join∘fmap userPassFromBasicAuthString∘unPrefix "Basic "∘decodeUtf8

parseWWWAuthenticate ∷ ByteString → Maybe Text
parseWWWAuthenticate s = case unPrefix "Basic realm=\"" $ decodeUtf8 s of
  Nothing → Nothing
  Just "" → Nothing
  Just(shit∷Text) → case Text.splitAt (length shit-1) shit of
    (r,"\"") → Just r
    _ → Nothing

authAttempt ∷ W.Request → AuthParse
authAttempt req = f where
  hdrs = W.requestHeaders req
  parse a b = case (parseWWWAuthenticate a, parseAuthorization b) of
    (Just aRealm, Just(uname,p)) → case DB.textUser uname of
      Nothing → Malformed
      Just u → AuthAttempt aRealm u (DB.textPassword p)
    _ → Malformed
  f = case (lookup "WWW-Authenticate" hdrs, lookup "Authorization" hdrs) of
    (Nothing, Just b) → parse "Basic realm=\"api\"" b
    (Nothing, Nothing) → Anon
    (Just a, Just b) → parse a b
    _ → Malformed

-- This rejects requests with invalid credentials. Any requests that pass
-- through this filter will either have no credentials or correct credentials.
basicAuth ∷ AcidState DB.State → W.Application → W.Application
basicAuth db waiapp req = do
  case authAttempt req of
    Anon → return $ W.responseLBS W.status401 [("www-authenticate","Basic")] ""
    Malformed → return $ W.responseLBS W.status403 [] ""
    AuthAttempt _ u p → do
      currentPass ← query db (DB.UserPassword u)
      case currentPass of
        Nothing → update db (DB.Register u p) >> waiapp req
        Just cp → if cp≡p then waiapp req else
          return $ W.responseLBS W.status400 [] ""


-- [[HTTP Requests and Responses]]
mkDay ∷ Text → Maybe Day
mkDay t = case Text.decimal t of
  Left _ → Nothing
  Right (v,_) → Just(ModifiedJulianDay v)

decRequest ∷ W.Request → IO Req
decRequest r = do
  let
    orReject = fromMaybe BadReq
    day = mkDay ∘ decodeUtf8
    dayQuery daystr status = case (join(fmap day daystr), status) of
      (Just d, Just "True") → GetSuccesses d
      (Just d, Just "False") → GetFailures d
      _ → BadReq
    done habstr daystr status =
      case (join(fmap day daystr), DB.textHabit habstr, status) of
        (Just d, Just h, Just "True") → SetDone d h True
        (Just d, Just h, Just "False") → SetDone d h False
        _ → BadReq
    hAddHabit habit = orReject $ fmap AddHabit $ DB.textHabit habit
    hDelHabit habit = orReject $ fmap DelHabit $ DB.textHabit habit
    hChains d = orReject $ fmap GetChains $ join $ fmap day d
  return $ case(W.pathInfo r, W.requestMethod r, List.sort$W.queryString r) of
    ([],"GET",[]) → WebClient
    (["api","chains"],"GET",[("upto",d)]) → hChains d
    (["api","habits"],"LIST",[]) → ListHabits
    (["api","habits",h],"PUT",[]) → hAddHabit h
    (["api","habits",h],"DELETE",[]) → hDelHabit h
    (["api","habits","done"],"GET",[("day",d),("status",s)]) → dayQuery d s
    (["api","habits",h,"done"],"POST",[("day",d),("status",s)]) → done h d s
    _ → BadReq

encResponse ∷ Resp → W.Response
encResponse resp = r resp where
  r NotOk = W.responseLBS W.status404 [] ""
  r Ok = W.responseLBS W.status204 [] ""
  r ServeClient = W.responseFile W.status200 html "./index.html" Nothing
  r (Habits habits) = (W.responseLBS W.status200 json $ J.encode $ Set.map DB.habitText habits)
  r (Chains cs) = W.responseLBS W.status200 json $ J.encode $ Map.mapKeys DB.habitText cs
  html = [("Content-Type", "text/html")]
  json = [("Content-Type", "application/javascript")]

failures ∷ Map DB.Habit DB.HabitStatus → Resp
failures = Habits ∘ Set.fromList ∘ Map.keys ∘ Map.filter (not ∘ DB.isSuccess)

successes ∷ Map DB.Habit DB.HabitStatus → Resp
successes = Habits ∘ Set.fromList ∘ Map.keys ∘ Map.filter DB.isSuccess


-- [[Application Logic]]
orFail ∷ (a → Resp) → Maybe a → Resp
orFail c (Just a) = c a
orFail _ Nothing = NotOk

respond ∷ AcidState DB.State → Req → IO Resp
respond db req = case req of
  BadReq → return NotOk
  WebClient → return ServeClient
  ListHabits → query db (DB.UserHabits DB.isan) >>= return ∘ orFail Habits
  GetFailures day → query db (DB.HabitsStatus DB.isan day) >>= return ∘ orFail failures
  GetSuccesses day → query db (DB.HabitsStatus DB.isan day) >>= return ∘ orFail successes
  AddHabit habit → update db (DB.AddHabit DB.isan habit) >> return Ok
  DelHabit habit → update db (DB.DelHabit DB.isan habit) >> return Ok
  SetDone day habit True → (update db $ DB.SetHabitStatus DB.isan day habit $ DB.Success Nothing) >> return Ok
  SetDone day habit False → (update db $ DB.SetHabitStatus DB.isan day habit $ DB.Failure Nothing) >> return Ok
  GetChains day → query db (DB.Chains DB.isan day) >>= return ∘ orFail Chains

app ∷ AcidState DB.State → W.Request → IO W.Response
app db webreq = do
  req ← decRequest webreq
  putStrLn "=========="
  putStrLn $ pack $ show req
  resp ← respond db req
  putStrLn $ pack $ show resp
  return $ encResponse resp

main ∷ IO()
main = do
  let tlsOpts = W.defaultTlsSettings
  port ← getEnv "PORT" >>= return∘read
  let warpOpts = W.setPort port W.defaultSettings
  db ← openLocalState DB.emptyState
  finally (W.runTLS tlsOpts warpOpts $ basicAuth db $ app db) $ do
    createCheckpoint db
    closeAcidState db
