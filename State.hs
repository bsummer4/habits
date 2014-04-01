{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}

module State
  ( Habit, State, Password
  , NoteStatus
  , HabitStatus(Success, Failure, Unspecified)
  , textHabit, habitText, userText, textUser
  , emptyState
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


-- [[Datatypes]]
type User = ByteString
data Password = Password ByteString
data Habit = Habit Text
data NoteStatus = NoteSuccess | NoteFailure | NoteUnspecified
data HabitStatus = Success(Maybe Double) | Failure(Maybe Double) | Unspecified
data DayState = DayState [(Text,NoteStatus)] (Map Habit HabitStatus)
data State = State(Map User UserState)
type History = Map Day DayState
data UserState = UserState {uHabits∷Set Habit, uHistory∷History}

deriving instance Eq Habit
deriving instance Ord Habit
deriving instance Show DayState
deriving instance Show Habit
deriving instance Show HabitStatus
deriving instance Show NoteStatus
deriving instance Show Password -- TODO No!
deriving instance Show State
deriving instance Show UserState
deriving instance Typeable Habit
deriving instance Typeable HabitStatus
deriving instance Typeable NoteStatus
deriving instance Typeable State

$(deriveSafeCopy 0 'base ''Password)
$(deriveSafeCopy 0 'base ''Habit)
$(deriveSafeCopy 0 'base ''NoteStatus)
$(deriveSafeCopy 0 'base ''HabitStatus)
$(deriveSafeCopy 0 'base ''UserState)
$(deriveSafeCopy 0 'base ''DayState)
$(deriveSafeCopy 3 'base ''State)

$(J.deriveJSON J.defaultOptions ''Habit)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''HabitStatus)
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

userText ∷ ByteString → ByteString
userText = id

textUser ∷ ByteString → Maybe ByteString
textUser t = if validUsername (decodeUtf8 t) then Just t else Nothing


-- [[Utility Functions]]
-- ‘Map.union’ is left-biased, so things in ‘statuses’ are always used
-- if they exist.
fillStatusBlanks ∷ Set Habit → Map Habit HabitStatus → Map Habit HabitStatus
fillStatusBlanks allHabits statuses = result
  where
    result = Map.filterWithKey validHabit $ statuses `Map.union` bs
    validHabit k _ = Set.member k allHabits
    bs = Map.fromList $ map (\k→(k,Unspecified)) $ Set.toList allHabits

-- An infinite list of DayStates going back in time from a given day.
historyIterator ∷ Day → History → [DayState]
historyIterator startDay history = iter startDay where
  iter today = todayState : iter tomorrow where
    tomorrow = addDays (-1) today
    todayState = getDay today history

isSuccess ∷ HabitStatus → Bool
isSuccess (Success _) = True
isSuccess _ = False

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

getUser ∷ User → State → UserState
getUser user (State users) =
  fromMaybe (UserState Set.empty Map.empty) $ Map.lookup user users

getDay ∷ Day → History → DayState
getDay d hist = fromMaybe (DayState [] Map.empty) $ Map.lookup d hist


-- [[Acid State Operations]]
userHabits ∷ User → Query State (Set Habit)
userHabits user = ask >>= return ∘ uHabits ∘ getUser user

addHabit ∷ User → Habit → Update State ()
addHabit user newhabit = do
  st@(State users) ← liftQuery ask
  let usrSt = getUser user st
  let usrSt' = usrSt {uHabits = Set.insert newhabit $ uHabits usrSt}
  put $ State $ Map.insert user usrSt' users

delHabit ∷ User → Habit → Update State ()
delHabit user habit = do
  st@(State users) ← liftQuery ask
  let UserState habits history = getUser user st
  let habits' = Set.delete habit habits
  put $ State $ Map.insert user (UserState habits' history) users

updateHabitStatus ∷ Day → Habit → HabitStatus → History → History
updateHabitStatus day habit status history = result where
  DayState notes adherance = getDay day history
  dayState' = DayState notes (Map.insert habit status adherance)
  result = Map.insert day dayState' history

setHabitStatus ∷ User → Day → Habit → HabitStatus → Update State ()
setHabitStatus user day habit newStatus = do
  st@(State users) ← liftQuery ask
  let (UserState habits history) = getUser user st
  let history' = updateHabitStatus day habit newStatus history
  put $ State $ Map.insert user (UserState habits history') users

chains ∷ User → Day → Query State (Map Habit Int)
chains user day = do
  st ← ask
  let usrSt = getUser user st
  return $ chainLengths (uHabits usrSt) $ map successfulHabits $
    historyIterator day (uHistory usrSt)

habitsStatus ∷ User → Day → Query State (Map Habit HabitStatus)
habitsStatus u day = do
  st ← ask
  let usrSt = getUser u st
  let DayState _ habitStatuses = getDay day $ uHistory usrSt
  return $ fillStatusBlanks (uHabits usrSt) habitStatuses

$(makeAcidic ''State
  ['addHabit, 'setHabitStatus, 'delHabit, 'userHabits, 'chains, 'habitsStatus])
