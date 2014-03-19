-- Here's what the interface might look like:
--     You still need to do $h, $i, $j, and $k today.
--     Yesterday, you fucked up on $h, $i, $j, and $k.
--     Gratz! You've been doing $h for $x days in a row!
--     Gratz! You've been doing $k for $y days in a row!

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
-- import Data.Time.Clock
import Data.Time.Calendar


-- [[Habit Management]]
type Habit = Text
type History = Map Day (Set Habit)
data State = State (Set Habit) History deriving Typeable

successes ∷ Day → History → Set Habit
successes day hist = case Map.lookup day hist of {Nothing→Set.empty; Just s→s}

failures ∷ Day → State → Set Habit
failures day (State habits hist) = habits `Set.difference` successes day hist

didIt ∷ (Day,Habit) → History → History
didIt (day,habit) hist = Map.insert day daySuccesses hist where
    daySuccesses = Set.insert habit $ successes day hist

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


-- [[Database]]
addHabit_ ∷ Habit → Update State ()
addHabit_ habit = liftQuery ask >>= put ∘ addHabit habit

didIt_ ∷ (Day,Habit) → Update State ()
didIt_ (date,habit) = do
    State habits history ← liftQuery ask
    put $ State habits $ didIt (date,habit) history

queryState ∷ Query State State
queryState = ask

$(deriveSafeCopy 0 'base ''State)
$(makeAcidic ''State ['queryState, 'addHabit_, 'didIt_])

getState ∷ AcidState State → IO State
getState db = query db QueryState


-- [[Application Logic]]
data Resp
    = NotOk | Ok | ServeClient | Habits (Set Habit) | Chains (Map Habit Int)
    deriving Show

data Req
    = BadReq | WebClient
    | GetFailures Day | GetChains Day |  DidIt Day Habit
    | AddHabit Habit | GetHabits
    deriving Show

respond ∷ AcidState State → Req → IO Resp
respond db req = case req of
    BadReq → return NotOk
    WebClient → return ServeClient
    GetHabits → getState db >>= (\(State habits _)→return $ Habits habits)
    GetFailures date → getState db >>= return ∘ Habits ∘ failures date
    AddHabit habit → update db (AddHabit_ habit) >> return Ok
    DidIt date habit → update db (DidIt_ (date,habit)) >> return Ok
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
    let day d = case d of
        { "today" → Just today
        ; "yesterday" → Just $ addDays (-1) today
        ; _ → mkDay d }
    let addhab hab = case mkHabit hab of
        {Nothing→BadReq; Just h→AddHabit h}
    let get conn date = case day date of
        {Nothing→BadReq; Just d→conn d}
    let did d h = case (day d, mkHabit h) of
        {(Just date,Just hab) → DidIt date hab;  _ → BadReq}
    return $ case (W.pathInfo r, W.requestMethod r, W.queryString r) of
        ([],"GET",[]) → WebClient
        (["api","failures"],"GET",[]) → GetFailures today
        (["api","failures",date],"GET",[]) → get GetFailures date
        (["api","successes",date,habit],"PUT",[]) → did date habit
        (["api","chains"],"GET",[]) → GetChains today
        (["api","chains",date],"GET",[]) → get GetChains date
        (["api","habits"],"GET",[]) → GetHabits
        (["api","habits",habit],"PUT",[]) → addhab habit
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
