{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Data.SafeCopy
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import qualified Network.URI as URI
import qualified Network.HTTP.Types as W
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Data.Aeson as J
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Time.Calendar


-- [[Habit Management]]
type Habit = Text
type History = Map Day (Set Habit)
data State = State (Set Habit) History deriving Typeable

successes ∷ Day → History → Set Habit
successes day hist = case Map.lookup day hist of {Nothing→Set.empty; Just s→s}

failures ∷ Day → State → Set Habit
failures day (State habits hist) = habits `Set.difference` successes day hist

setDone ∷ Day → Habit → Bool → History → History
setDone day habit status hist = Map.insert day daySuccesses hist where
    daySuccesses = if status
        then Set.insert habit $ successes day hist
        else Set.delete habit $ successes day hist

chains ∷ Day → History → Set Habit → [Set Habit]
chains today hist soFar = if Set.null habits then [] else habits:yesterday where
    yesterday = chains (addDays (-1) today) hist habits
    habits = soFar `Set.intersection` (successes today hist)

incCounts ∷ Ord k ⇒ Set k → Map k Int → Map k Int
incCounts keys init = Set.fold iter init keys where
    iter k m = Map.alter addOne k m
    addOne n = Just $ (1∷Int)+fromMaybe 0 n

chainLengths ∷ [Set Habit] → Map Habit Int
chainLengths = List.foldl (\acc hs → incCounts hs acc) Map.empty

addHabit ∷ Habit → State → State
addHabit habit (State habits hist) = (State (Set.insert habit habits) hist)

delHabit ∷ Habit → State → State
delHabit habit (State habits hist) = (State (Set.delete habit habits) hist)


-- [[Database]]
addHabit_ ∷ Habit → Update State ()
addHabit_ habit = liftQuery ask >>= put ∘ addHabit habit

delHabit_ ∷ Habit → Update State ()
delHabit_ habit = liftQuery ask >>= put ∘ delHabit habit

setDone_ ∷ Day → Habit → Bool → Update State ()
setDone_ day habit status = do
    State habits history ← liftQuery ask
    put $ State habits $ setDone day habit status history

queryState ∷ Query State State
queryState = ask

$(deriveSafeCopy 1 'base ''State)
$(makeAcidic ''State ['queryState, 'addHabit_, 'setDone_, 'delHabit_])

getState ∷ AcidState State → IO State
getState db = query db QueryState


-- [[Application Logic]]
data Resp
    = NotOk | Ok | ServeClient | Habits (Set Habit) | Chains (Map Habit Int)
    deriving Show

data Req
    = BadReq | WebClient
    | GetSuccesses Day | GetFailures Day
    | GetChains Day | SetDone Day Habit Bool
    | AddHabit Habit | DelHabit Habit | ListHabits
    deriving Show

respond ∷ AcidState State → Req → IO Resp
respond db req = case req of
    BadReq → return NotOk
    WebClient → return ServeClient
    ListHabits → getState db >>= (\(State habits _)→return $ Habits habits)
    GetFailures date → getState db >>= return ∘ Habits ∘ failures date
    GetSuccesses date → getState db >>= (\(State _ hist) →
        return $ Habits $ successes date hist)
    AddHabit habit → update db (AddHabit_ habit) >> return Ok
    DelHabit habit → update db (DelHabit_ habit) >> return Ok
    SetDone date habit stat → update db (SetDone_ date habit stat) >> return Ok
    GetChains day → getState db >>= \(State allHabits hist) →
        return $ Chains $ chainLengths $ chains day hist allHabits


-- [[HTTP Requests and Responses]]
decodeURIComponent ∷ Text → Text
decodeURIComponent = pack ∘ URI.unEscapeString ∘ unpack

mkHabit ∷ Text → Maybe Habit
mkHabit t = if invalidHabit then Nothing else Just t where
    invalidHabit = length t≡0 || length t>16 || not(Text.all (`elem` az) t)
    az = ['a'..'z']

mkDay ∷ Text → Maybe Day
mkDay t = case Text.decimal t of
    Left _ → Nothing
    Right (v,_) → Just(ModifiedJulianDay v)


decRequest ∷ W.Request → IO Req
decRequest r = do
    today ← getCurrentTime >>= return ∘ utctDay
    let
        orReject = fromMaybe BadReq
        day daystr = case daystr∷ByteString of
            "today" → Just today
            "yesterday" → Just $ addDays (-1) today
            d → mkDay $ decodeUtf8 d
        dayQuery daystr statusstr = case (join(fmap day daystr), statusstr) of
            (Just d, Just "True") → GetSuccesses d
            (Just d, Just "False") → GetFailures d
            _ → BadReq
        done habstr daystr statusstr =
            case (join(fmap day daystr), mkHabit habstr, statusstr) of
                (Just d, Just h, Just "True") → SetDone d h True
                (Just d, Just h, Just "False") → SetDone d h False
                _ → BadReq
        hAddHabit habit = orReject $ fmap AddHabit $ mkHabit habit
        hDelHabit habit = orReject $ fmap DelHabit $ mkHabit habit
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
    r (Habits habits) = (W.responseLBS W.status200 json $ J.encode habits)
    r (Chains cs) = (W.responseLBS W.status200 json $ J.encode cs)
    html = [("Content-Type", "text/html")]
    json = [("Content-Type", "application/javascript")]

app ∷ AcidState State → W.Request → IO W.Response
app db webreq = do
    req ← decRequest webreq
    putStrLn "=========="
    putStrLn $ pack $ show req
    resp ← respond db req
    putStrLn $ pack $ show resp
    return $ encResponse resp

main ∷ IO()
main = do
    let dumb = ["curfew", "breakfast", "lunch", "dinner", "nosnacks"]
    db ← openLocalState (State (Set.fromList dumb) Map.empty)
    finally (W.run 8080 $ app db) $ do
        createCheckpoint db
        closeAcidState db
