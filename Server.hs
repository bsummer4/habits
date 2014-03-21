{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Data.SafeCopy
import Prelude (read)
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import System.Environment (getEnv)
import qualified Network.URI as URI
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
import Data.Time.Calendar
import qualified Data.ByteString.Base64 as Base64


-- [[Datatypes]]
data AuthParse = Anon | Malformed | AuthAttempt Text User Password
type Registrations = Map User Password
data User = User Text deriving (Eq, Ord, Show, Typeable)
type Password = Text
type Habit = Text
type History = Map Day (Set Habit)
data State = State Registrations (Set Habit) History deriving Typeable

data StateV1 = StateV1 (Set Habit) History deriving Typeable
instance Migrate State where
    type MigrateFrom State = StateV1
    migrate (StateV1 habits hist) = State Map.empty habits hist

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 1 'base ''StateV1)
$(deriveSafeCopy 2 'extension ''State)

data Resp
    = NotOk | Ok | ServeClient | Habits (Set Habit) | Chains (Map Habit Int)
    deriving Show

data Req
    = BadReq | WebClient
    | GetSuccesses Day | GetFailures Day
    | GetChains Day | SetDone Day Habit Bool
    | AddHabit Habit | DelHabit Habit | ListHabits
    deriving Show

validUsername ∷ Text → Bool
validUsername t = lenOK && okCharset where
    lenOK = length t≤50 && length t>0
    (az,az09) = (['a'..'z']++['A'..'Z'], az++['0'..'9'])
    okCharset = case Text.uncons t of
        Nothing → False
        Just(h,tail) → h `elem` az && Text.all (`elem` az09) tail

username ∷ User → Text
username (User name) = name

userFromName ∷ Text → Maybe User
userFromName t = if validUsername t then Just $ User t else Nothing


-- [[Habit Management]]
successes ∷ Day → State → Set Habit
successes day (State _ habits hist) =
    habits `Set.intersection` fromMaybe Set.empty(Map.lookup day hist)

failures ∷ Day → State → Set Habit
failures day st@(State _ habits _) = habits `Set.difference` successes day st

incCounts ∷ Ord k ⇒ Set k → Map k Int → Map k Int
incCounts keys init = Set.fold iter init keys where
    iter k m = Map.alter addOne k m
    addOne n = Just $ (1∷Int)+fromMaybe 0 n



-- [[Database]]
addHabitDB ∷ Habit → Update State ()
addHabitDB habit = liftQuery ask >>= put ∘ add habit where
    add habit (State rs habs hist) = (State rs (Set.insert habit habs) hist)

delHabitDB ∷ Habit → Update State ()
delHabitDB habit = liftQuery ask >>= put ∘ del habit where
    del habit (State rs habs hist) = (State rs (Set.delete habit habs) hist)

setDoneDB ∷ Day → Habit → Bool → Update State ()
setDoneDB day habit status = liftQuery ask >>= put∘set day habit status where
    set day habit status st@(State regs habits hist) =
        State regs habits hist' where
            hist' = Map.insert day daySuccesses hist
            daySuccesses = if status
                then Set.insert habit $ successes day st
                else Set.delete habit $ successes day st

queryStateDB ∷ Query State State
queryStateDB = ask

registerUserDB ∷ User → Password → Update State ()
registerUserDB u p = do
    (State registrations hab hist) ← liftQuery ask
    let newregs = Map.insert u p registrations
    put $ State newregs hab hist

$(makeAcidic ''State
    ['queryStateDB, 'addHabitDB, 'setDoneDB, 'delHabitDB, 'registerUserDB])

getState ∷ AcidState State → IO State
getState db = query db QueryStateDB


-- [[Authentication Stuff]]
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
        (Just aRealm, Just(uname,p)) → case userFromName uname of
            Nothing → Malformed
            Just u → AuthAttempt aRealm u p
        _ → Malformed
    f = case (lookup "WWW-Authenticate" hdrs, lookup "Authorization" hdrs) of
        (Nothing, Just b) → parse "Basic realm=\"api\"" b
        (Nothing, Nothing) → Anon
        (Just a, Just b) → parse a b
        _ → Malformed

authFailed ∷ IO W.Response
authFailed = return $ W.responseLBS W.status403 [] ""

-- This rejects requests with invalid credentials. Any requests that pass
-- through this filter will either have no credentials or correct credentials.
basicAuth ∷ AcidState State → W.Application → W.Application
basicAuth db waiapp req = do
    (State users _ _) ← getState db
    putStrLn $ Text.pack $ show users
    case authAttempt req of
        Anon → return $ W.responseLBS W.status401 [("www-authenticate","Basic")] ""
        Malformed → authFailed
        AuthAttempt _ u p → case Map.lookup u users of
            Nothing → update db (RegisterUserDB u p) >> waiapp req
            Just(password) → if password≡p then waiapp req else
                return $ W.responseLBS W.status400 [] ""

-- [[Application Logic]]
chains ∷ Day → State → [Set Habit]
chains day st@(State _ allHabits _) = r day allHabits where
    r d soFar = if Set.null habits then [] else habits:yesterday where
        yesterday = r (addDays (-1) d) habits
        habits = soFar `Set.intersection` (successes d st)

chainLengths ∷ [Set Habit] → Map Habit Int
chainLengths = List.foldl (\acc hs → incCounts hs acc) Map.empty

respond ∷ AcidState State → Req → IO Resp
respond db req = case req of
    BadReq → return NotOk
    WebClient → return ServeClient
    ListHabits → getState db >>= (\(State _ habits _)→return $ Habits habits)
    GetFailures date → getState db >>= return ∘ Habits ∘ failures date
    GetSuccesses date → getState db >>= return ∘ Habits ∘ successes date
    AddHabit habit → update db (AddHabitDB habit) >> return Ok
    DelHabit habit → update db (DelHabitDB habit) >> return Ok
    SetDone date habit stat → update db (SetDoneDB date habit stat) >> return Ok
    GetChains day → getState db >>= return ∘ Chains ∘ chainLengths ∘ chains day


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
    let
        orReject = fromMaybe BadReq
        day = mkDay ∘ decodeUtf8
        dayQuery daystr status = case (join(fmap day daystr), status) of
            (Just d, Just "True") → GetSuccesses d
            (Just d, Just "False") → GetFailures d
            _ → BadReq
        done habstr daystr status =
            case (join(fmap day daystr), mkHabit habstr, status) of
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

-- This code does no error handling. It doesn't need to be fixed right, away
-- since we'll exit the program in all the possible error situations anyways.
getPortFromEnvironment ∷ IO Int
getPortFromEnvironment = getEnv "PORT" >>= return ∘ read

main ∷ IO()
main = do
    let tlsOpts = W.defaultTlsSettings
    port ← getPortFromEnvironment
    let warpOpts = W.setPort port W.defaultSettings
    db ← openLocalState (State Map.empty Set.empty Map.empty)
    let getRegs = getState db >>= (\(State regs _ _) → return regs)
    finally (W.runTLS tlsOpts warpOpts $ basicAuth db $ app db) $ do
        createCheckpoint db
        closeAcidState db
