{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main(main,css,cssfile,forExample) where

import qualified Auth                        as Auth
import           ClassyPrelude
import           Control.Monad
import           Control.Monad.State         (put)
import           Data.Acid
import qualified Data.Aeson                  as J
import qualified Data.Aeson.TH               as J
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as ASCII
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust)
import           Data.SafeCopy
import qualified Data.Set                    as Set
import           Language.Haskell.TH
import qualified Network.HTTP.Types          as W
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import           Prelude                     (read)
import           Prelude.Unicode
import qualified State                       as DB
import           System.Environment          (getEnv)
import qualified Web.ClientSession           as ClientSession

data Resp
  = OK
  | MALFORMED_REQUEST | NOT_FOUND | USER_ALREADY_EXISTS | NOT_ALLOWED
  | AUTH Auth.Token
  | STATUSES (Map DB.Habit DB.HabitStatus)
  | HABITS (Set DB.Habit)
  | NOTES (Set Text)
  | CHAINS (Map DB.Habit Int)
  | HISTORY (Map Day (Map DB.Habit DB.HabitStatus))
  deriving Typeable

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
  | RenameHabit Auth.User DB.Habit DB.Habit

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
    unday (ModifiedJulianDay d) = show d

-- TODO Using ‘read’ here allows bad data to raise an error.
instance J.FromJSON α ⇒ J.FromJSON (Map Day α) where
  parseJSON x = Map.mapKeys toDay <$> J.parseJSON x where
    toDay d = ModifiedJulianDay $ read d

instance J.ToJSON α ⇒ J.ToJSON (Map DB.Habit α) where
  toJSON x = J.toJSON $ Map.mapKeys DB.habitText x

instance J.FromJSON α ⇒ J.FromJSON (Map DB.Habit α) where
  parseJSON o = do
    x ∷ Map Text α ← J.parseJSON o
    let mb (mk,v) = case mk of {Nothing→Nothing; Just k→Just(k,v)}
    case sequence $ map mb $ Map.toList $ Map.mapKeys DB.textHabit x of
      Nothing → mzero
      Just kvs → return $ Map.fromList kvs

fix ∷ (s → s) → Update s ()
fix f = (f <$> liftQuery ask) >>= put

updateState ∷ RUpdate → Update DB.State ()
updateState = \case
    SetHabitsStatus u d h status → fix (DB.setHabitStatus u d h status)
    AddNote u d n                → fix (DB.addNote u d n)
    DelNote u d n                → fix (DB.delNote u d n)
    AddHabit u h                 → fix (DB.addHabit u h)
    DelHabit u h                 → fix (DB.delHabit u h)
    RenameHabit u old new        → fix (DB.renameHabit u old new)

queryState ∷ RQuery → Query DB.State Resp
queryState = \case
    GetHabitsStatus u d → STATUSES <$> DB.habitsStatus u d <$> ask
    GetChains u d       → CHAINS   <$> DB.chains u d       <$> ask
    GetNotes u d        → NOTES    <$> DB.getNotes u d     <$> ask
    ListHabits u        → HABITS   <$> DB.userHabits u     <$> ask
    History30 u d       → HISTORY  <$> DB.getHistory30 u d <$> ask

$(deriveSafeCopy 0 'base ''Resp)
$(deriveSafeCopy 0 'base ''RUpdate)
$(deriveSafeCopy 0 'base ''RQuery)
$(makeAcidic ''DB.State ['queryState, 'updateState])

-- [[Authentication and Authorization]]
tokenUserMatch ∷ AcidState Auth.Registrations → Auth.Token → Auth.User → IO Bool
tokenUserMatch db t user = do
  k ← ClientSession.getDefaultKey
  muser ← Auth.authenticate db k t
  return $ case muser of
    Nothing → False
    Just u → user≡u

authorized ∷ AcidState Auth.Registrations → Req → IO Bool
authorized db req =
  let check t u = tokenUserMatch db t u in
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
        RenameHabit u _ _ → check t u
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
    Query _ r → query db (QueryState r)
    Update _ r → update db (UpdateState r) >> return OK

js,css,html ∷ [(W.HeaderName, ByteString)]
html = [("Content-Type", "text/html")]
js   = [("Content-Type", "application/javascript")]
css  = [("Content-Type", "text/css")]

htmlfile, cssfile, jsfile ∷ LBS.ByteString
htmlfile = $(runQ $ (LitE∘StringL) <$> (runIO $ readFile "./client.html"))
cssfile  = $(runQ $ (LitE∘StringL) <$> (runIO $ readFile "./client.css"))
jsfile   = $(runQ $ (LitE∘StringL) <$> (runIO $ readFile "./client.js"))

app ∷ AcidState Auth.Registrations → AcidState DB.State → W.Application
app authdb db webreq callback =
  if "GET" ≡ W.requestMethod webreq
  then case W.pathInfo webreq of
    []            → callback $ W.responseLBS W.ok200 html htmlfile
    ["client.js"] → callback $ W.responseLBS W.ok200 js jsfile
    _             → callback $ W.responseLBS W.notFound404 [] ""
  else do
    body ← W.lazyRequestBody webreq
    putStrLn "=========="
    ASCII.putStrLn body
    resp ← case J.decode body of
      Nothing → return MALFORMED_REQUEST
      Just req → respond authdb db req
    ASCII.putStrLn $ J.encode resp
    callback $ W.responseLBS W.ok200 js $ J.encode resp

main ∷ IO ()
main = do
  port   ← getEnv "PORT" >>= return∘read
  db     ← openLocalState DB.emptyState
  authdb ← openLocalState Auth.emptyRegistrations
  finally (W.run port $ app authdb db) $ do
    createCheckpoint db
    createCheckpoint authdb
    closeAcidState db
    closeAcidState authdb

forExample ∷ IO ()
forExample =
  let
    jprint ∷ (J.ToJSON j) ⇒ j → IO()
    jprint = ASCII.putStrLn ∘ J.encode
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
      , NOTES $ Set.singleton "do it"
      , CHAINS $ Map.singleton habit 99
      ]

  in do
    Control.Monad.mapM_ jprint reqEx
    Control.Monad.mapM_ jprint respEx
