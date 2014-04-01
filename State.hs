{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}

module State
  ( User, Habit, State, Password
  , NoteStatus
  , HabitStatus(Success, Failure, Unspecified)
  , textHabit, habitText, userText, textUser
  , isSuccess, emptyState
  , register, Authenticate(Authenticate)
  , UserHabits(UserHabits), AddHabit(AddHabit), DelHabit(DelHabit)
  , Chains(Chains), HabitsStatus(HabitsStatus)
  , SetHabitStatus(SetHabitStatus)
  ) where

import ClassyPrelude
import Prelude.Unicode
import Data.Acid
import Data.SafeCopy
import Prelude (read)
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar (addDays)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J
import qualified Crypto.PasswordStore as PW


-- [[Datatypes]]
data User = User Text
data Password = Password ByteString
data Habit = Habit Text
data NoteStatus = NoteSuccess | NoteFailure | NoteUnspecified
data HabitStatus = Success(Maybe Double) | Failure(Maybe Double) | Unspecified
data DayState = DayState [(Text,NoteStatus)] (Map Habit HabitStatus)
data State = State(Map User UserState)
type History = Map Day DayState
data UserState = UserState
  { uPassHash ∷ ByteString
  , uHabits ∷ Set Habit
  , uHistory ∷ History }

deriving instance Eq Habit
deriving instance Eq User
deriving instance Ord Habit
deriving instance Ord User
deriving instance Show DayState
deriving instance Show Habit
deriving instance Show HabitStatus
deriving instance Show NoteStatus
deriving instance Show Password -- TODO No!
deriving instance Show State
deriving instance Show User
deriving instance Show UserState
deriving instance Typeable Habit
deriving instance Typeable HabitStatus
deriving instance Typeable NoteStatus
deriving instance Typeable State

$(deriveSafeCopy 0 'base ''Password)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Habit)
$(deriveSafeCopy 0 'base ''NoteStatus)
$(deriveSafeCopy 0 'base ''HabitStatus)
$(deriveSafeCopy 0 'base ''UserState)
$(deriveSafeCopy 0 'base ''DayState)
$(deriveSafeCopy 3 'base ''State)

$(J.deriveJSON J.defaultOptions ''Habit)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''HabitStatus)
$(J.deriveJSON J.defaultOptions ''User)
$(J.deriveJSON J.defaultOptions ''Password)

instance J.FromJSON ByteString where
	parseJSON o = J.parseJSON o >>= return∘encodeUtf8

instance J.ToJSON ByteString where
	toJSON = J.toJSON ∘ decodeUtf8


-- [[Smart Constructors]]
emptyState ∷ State
emptyState = State Map.empty

textHabit ∷ Text → Maybe Habit
textHabit t = if invalidHabit then Nothing else Just $ Habit t where
  invalidHabit = length t≡0 || length t>16 || not(Text.all (`elem` az) t)
  az = ['a'..'z']

habitText ∷ Habit → Text
habitText (Habit h) = h

validUsername ∷ Text → Bool
validUsername t = lenOK && okCharset where
  lenOK = length t≤50 && length t>0
  (az,az09) = (['a'..'z']++['A'..'Z'], az++['0'..'9'])
  okCharset = case Text.uncons t of
    Nothing → False
    Just(h,tail) → h `elem` az && Text.all (`elem` az09) tail

userText ∷ User → Text
userText (User name) = name

textUser ∷ Text → Maybe User
textUser t = if validUsername t then Just $ User t else Nothing


-- [[Utility Functions]]
-- ‘Map.union’ is left-biased, so things in ‘statuses’ are always used
-- if they exist.
fillStatusBlanks ∷ Set Habit → Map Habit HabitStatus → Map Habit HabitStatus
fillStatusBlanks allHabits statuses = statuses `Map.union` bs where
  bs = Map.fromList $ map (\k→(k,Unspecified)) $ Set.toList allHabits

isSuccess ∷ HabitStatus → Bool
isSuccess (Success _) = True
isSuccess _ = False

-- An infinite list of DayStates going back in time from a given day.
historyIterator ∷ Day → History → [DayState]
historyIterator startDay history = iter startDay where
  iter today = todayState : iter tomorrow where
    tomorrow = addDays (-1) today
    todayState = fromMaybe emptyDayState $ Map.lookup today history

successfulHabits ∷ DayState → Set Habit
successfulHabits (DayState _ habits) =
  Set.fromList $ Map.keys $ Map.filter isSuccess habits

chainLengths ∷ Set Habit → [Set Habit] → Map Habit Int
chainLengths allHabits days = loop (allHabits,Map.empty) days
  where
    loop (_,lengths) [] = lengths
    loop (unbrokenChains,lengths) (today:before) =
      case unbrokenChains `Set.intersection` today of
        stillUnbroken → if Set.null stillUnbroken then lengths else
          loop(stillUnbroken,incCounts stillUnbroken lengths) before
    incCounts keysToIncrement counts =
      Set.fold (Map.alter(Just∘(1+)∘fromMaybe 0)) counts keysToIncrement

unState ∷ State → Map User UserState
unState (State st) = st


-- [[Acid State Operations]]
registerDB ∷ User → ByteString → Update State Bool
registerDB user pwhash = do
  State users ← liftQuery ask
  case Map.lookup user users of
    Just _ → return False
    Nothing → do
      let users' = Map.insert user (UserState pwhash Set.empty Map.empty) users
      put $ State $ users'
      return True

authenticate ∷ User → Password → Query State Bool
authenticate user (Password pass) = ask >>= \(State users) →
  return $ case Map.lookup user users of
    Nothing → False
    Just usrSt → PW.verifyPassword pass $ uPassHash usrSt

userHabits ∷ User → Query State (Maybe (Set Habit))
userHabits user = ask >>= return ∘ fmap uHabits ∘ Map.lookup user ∘ unState

addHabit ∷ User → Habit → Update State Bool
addHabit user newhabit = do
  State users ← liftQuery ask
  case Map.lookup user users of
    Nothing → return False
    Just usrSt → do
      let habits' = (Set.insert newhabit $ uHabits usrSt) in
        put $ State $ Map.insert user (usrSt {uHabits=habits'}) users
      return True

delHabit ∷ User → Habit → Update State Bool
delHabit user habit = do
  State users ← liftQuery ask
  case Map.lookup user users of
    Nothing → return False
    Just(UserState pass habits history) → do
      let habits' = (Set.delete habit habits) in
        put $ State $ Map.insert user (UserState pass habits' history) users
      return True

emptyDayState ∷ DayState
emptyDayState = DayState [] Map.empty

updateHabitStatus ∷ Day → Habit → HabitStatus → History → History
updateHabitStatus day habit status history = result where
  DayState notes adherance = fromMaybe emptyDayState $ Map.lookup day history
  dayState' = DayState notes (Map.insert habit status adherance)
  result = Map.insert day dayState' history

setHabitStatus ∷ User → Day → Habit → HabitStatus → Update State Bool
setHabitStatus user day habit newStatus = do
  State users ← liftQuery ask
  case Map.lookup user users of
    Nothing → return False
    Just(UserState pass habits history) →
      if not(habit `Set.member` habits) then return False else
        let history' = updateHabitStatus day habit newStatus history in do
          put $ State $ Map.insert user (UserState pass habits history') users
          return True

chains ∷ User → Day → Query State (Maybe (Map Habit Int))
chains user day = do
  (State users) ← ask
  return $ fmap mkChains $ Map.lookup user users where
    mkChains usrSt = chainLengths (uHabits usrSt) $ map successfulHabits $
      historyIterator day (uHistory usrSt)

habitsStatus ∷ User → Day → Query State (Maybe (Map Habit HabitStatus))
habitsStatus u day = do
  State users ← ask
  return $ do
    usrSt ← Map.lookup u users
    let DayState _ habitStatuses = fromMaybe emptyDayState $ Map.lookup day $ uHistory usrSt
    return $ fillStatusBlanks (uHabits usrSt) habitStatuses

$(makeAcidic ''State
  [ 'addHabit, 'setHabitStatus, 'delHabit, 'registerDB, 'authenticate
  , 'userHabits, 'chains, 'habitsStatus
  ])

register ∷ AcidState State → User → Password → IO Bool
register db user (Password pass) = do
	pwhash ← PW.makePassword pass 14
	update db $ RegisterDB user pwhash

