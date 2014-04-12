{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

-- TODO Make a separate request for loging in. Right now I just register every
--   time which is a hack.

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Prelude (read)
import System.Environment (getEnv)
import Control.Monad
import Data.Maybe (fromJust)
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified State as DB
import qualified Data.ByteString.Lazy.Char8 as ASCII
import qualified Auth as Auth
import qualified Web.ClientSession as ClientSession

data Resp
  = OK
  | MALFORMED_REQUEST | NOT_FOUND | USER_ALREADY_EXISTS | NOT_ALLOWED
  | AUTH Auth.Token
  | STATUSES (Map DB.Habit DB.HabitStatus)
  | HABITS (Set DB.Habit)
  | NOTES (Set Text)
  | CHAINS (Map DB.Habit Int)
  | HISTORY (Map Day (Map DB.Habit DB.HabitStatus))

data Req
  = Login Auth.User Text
  | Register Auth.User Text
  | Query Auth.Token RQuery
  | Update Auth.Token RUpdate

data RUpdate
  = SetHabitsStatus Auth.User Day DB.Habit DB.HabitStatus
  | AddNote Auth.User Day Text
  | DelNote Auth.User Day Text
  | AddHabit Auth.User DB.Habit
  | DelHabit Auth.User DB.Habit

data RQuery
  = GetHabitsStatus Auth.User Day
  | GetChains Auth.User Day
  | GetNotes Auth.User Day
  | ListHabits Auth.User
  | History30 Auth.User Day

$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''Req)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''RUpdate)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''RQuery)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''Resp)

instance J.ToJSON Day where
  toJSON (ModifiedJulianDay x) = J.toJSON x

instance J.FromJSON Day where
  parseJSON x = ModifiedJulianDay <$> J.parseJSON x

instance J.ToJSON α ⇒ J.ToJSON (Map Day α) where
  toJSON x = J.toJSON $ Map.mapKeys unday x where
    unday (ModifiedJulianDay x) = show x

-- TODO Using ‘read’ here allows bad data to raise an error.
instance J.FromJSON α ⇒ J.FromJSON (Map Day α) where
  parseJSON x = Map.mapKeys toDay <$> J.parseJSON x where
    toDay x = ModifiedJulianDay $ read x

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
tokenUserMatch ∷ AcidState Auth.Registrations → Auth.Token → Auth.User → IO Bool
tokenUserMatch db t user = do
  k ← ClientSession.getDefaultKey
  muser ← Auth.authenticate db k t
  return $ case muser of
    Nothing → False
    Just u → user≡u

authorized ∷ AcidState Auth.Registrations → Req → IO Bool
authorized db req = let check t u = tokenUserMatch db t u in
  case req of
    Register _ _ → return True
    Login _ _ → return True
    Query t r → case r of
      GetHabitsStatus u _ → check t u
      GetChains u _ → check t u
      ListHabits u → check t u
      GetNotes u _ → check t u
      History30 u _ → check t u
    Update t r → case r of
      SetHabitsStatus u _ _ _ → check t u
      AddHabit u _ → check t u
      DelHabit u _ → check t u
      AddNote u _ _ → check t u
      DelNote u _ _ → check t u

-- [[HTTP Requests and Responses]]
respond ∷ AcidState Auth.Registrations → AcidState DB.State → Req → IO Resp
respond authdb db req = do
  k ← ClientSession.getDefaultKey
  ok ← authorized authdb req
  if not ok then return NOT_ALLOWED else case req of
    Login user pass → do
      AUTH <$> Auth.token k user (Auth.mkPass pass)
    Register user pass → do
      _ ← Auth.register authdb user (Auth.mkPass pass)
      AUTH <$> Auth.token k user (Auth.mkPass pass)
    Query _ r → case r of
      ListHabits user →
        query db (DB.UserHabits user) >>= return ∘ HABITS
      GetHabitsStatus user day →
        query db (DB.HabitsStatus user day) >>= return ∘ STATUSES
      GetChains user day → do
        query db (DB.Chains user day) >>= return ∘ CHAINS
      GetNotes user day →
        NOTES <$> query db (DB.GetNotes user day)
      History30 user day →
        HISTORY <$> query db (DB.GetHistory30 user day)
    Update _ r → case r of
      AddHabit user habit → do
        update db (DB.AddHabit user habit) >> return OK
      DelHabit user habit → do
        update db (DB.DelHabit user habit) >> return OK
      SetHabitsStatus user day habit status → do
        update db (DB.SetHabitStatus user day habit status) >> return OK
      AddNote user day note →
        update db (DB.AddNote user day note) >> return OK
      DelNote user day note →
        update db (DB.DelNote user day note) >> return OK

html = [("Content-Type", "text/html")]
json = [("Content-Type", "application/javascript")]

jprint ∷ J.ToJSON j ⇒ j → IO ()
jprint = ASCII.putStrLn ∘ J.encode

app ∷ AcidState Auth.Registrations → AcidState DB.State → W.Request → IO W.Response
app authdb db webreq =
  if "GET" ≡ W.requestMethod webreq
  then return $ W.responseFile W.status200 html "./index.html" Nothing
  else do
    body ← W.lazyRequestBody webreq
    putStrLn "=========="
    ASCII.putStrLn body
    resp ← case J.decode body of
      Nothing → return MALFORMED_REQUEST
      Just req → respond authdb db req
    ASCII.putStrLn $ J.encode resp
    return $ W.responseLBS W.status200 json $ J.encode resp

main ∷ IO()
main = do
  let tlsOpts = W.defaultTlsSettings
  port ← getEnv "PORT" >>= return∘read
  let warpOpts = W.setPort port W.defaultSettings
  db ← openLocalState DB.emptyState
  authdb ← openLocalState Auth.emptyRegistrations
  finally (W.runTLS tlsOpts warpOpts $ app authdb db) $ do
    createCheckpoint db
    createCheckpoint authdb
    closeAcidState db
    closeAcidState authdb

forExample ∷ IO ()
forExample =
  let
    day = ModifiedJulianDay 1234
    habit = fromJust $ DB.textHabit "hihi"
    hstatus = DB.Success Nothing
    hstatus2 = DB.Unspecified
    reqEx =
      [ Login "user" "pass"
      , Register "user" "pass"
      , Update "tok" $ SetHabitsStatus "isan" day habit hstatus
      , Update "tok" $ SetHabitsStatus "isan" day habit hstatus2
      , Query "tok" $ GetHabitsStatus "isan" day
      , Query "tok" $ GetChains "isan" day
      , Update "tok" $ AddHabit "isan" habit
      , Update "tok" $ DelHabit "isan" habit
      , Query "tok" $ ListHabits "isan"
      , Update "tok" $ AddNote "isan" day "test note"
      , Update "tok" $ DelNote "isan" day "test note"
      , Query "tok" $ GetNotes "isan" day
      , Query "tok" $ History30 "isan" day
      ]

    respEx =
      [ OK
      , AUTH "tok"
      , STATUSES $ Map.singleton habit hstatus
      , HABITS $ Set.singleton habit
      , NOTES $ Set.singleton "TODO it"
      , CHAINS $ Map.singleton habit 99
      ]

  in do
    Control.Monad.mapM_ jprint reqEx
    Control.Monad.mapM_ jprint respEx
