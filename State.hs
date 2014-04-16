{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}

module State
  ( Habit, State
  , HabitStatus(Success, Failure, Unspecified)
  , textHabit, habitText, userText, textUser
  , emptyState
  , userHabits, addHabit, delHabit, chains, habitsStatus, setHabitStatus
  , getHistory30, getNotes, addNote, delNote
  ) where

import ClassyPrelude
import Prelude.Unicode
import Data.SafeCopy
import Prelude (read)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Calendar (addDays)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as J


-- [[Datatypes]]
type User = ByteString
data Habit = Habit Text
data HabitStatus = Success(Maybe Double) | Failure(Maybe Double) | Unspecified
data DayState = DayState
  { dayNotes ∷ Set Text
  , dayHabits ∷ Map Habit HabitStatus
  }

data State = State(Map User UserState)
type History = Map Day DayState
data UserState = UserState
  { uHabits∷Set Habit
  , uHistory∷History
  }

deriving instance Eq Habit
deriving instance Ord Habit
deriving instance Show DayState
deriving instance Show Habit
deriving instance Show HabitStatus
deriving instance Show State
deriving instance Show UserState
deriving instance Typeable Habit
deriving instance Typeable HabitStatus
deriving instance Typeable State

$(deriveSafeCopy 0 'base ''Habit)
$(deriveSafeCopy 0 'base ''HabitStatus)
$(deriveSafeCopy 0 'base ''UserState)
$(deriveSafeCopy 0 'base ''DayState)
$(deriveSafeCopy 3 'base ''State)

$(J.deriveJSON J.defaultOptions ''Habit)
$(J.deriveJSON J.defaultOptions{J.sumEncoding=J.ObjectWithSingleField} ''HabitStatus)

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
  iter today = todayState : iter yesterday where
    yesterday = addDays (-1) today
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
getDay d hist = fromMaybe (DayState Set.empty Map.empty) $ Map.lookup d hist


-- [[State Operations]]
userHabits ∷ User → State → Set Habit
userHabits user = uHabits ∘ getUser user

addHabit ∷ User → Habit → State → State
addHabit user newhabit st@(State users) = do
  let usrSt = getUser user st
  let usrSt' = usrSt {uHabits = Set.insert newhabit $ uHabits usrSt}
  State $ Map.insert user usrSt' users

delHabit ∷ User → Habit → State → State
delHabit user habit st@(State users) =
  let UserState habits history = getUser user st in
  let habits' = Set.delete habit habits in
    State $ Map.insert user (UserState habits' history) users

updateHabitStatus ∷ Day → Habit → HabitStatus → History → History
updateHabitStatus day habit status history = result where
  DayState notes adherance = getDay day history
  dayState' = DayState notes (Map.insert habit status adherance)
  result = Map.insert day dayState' history

setHabitStatus ∷ User → Day → Habit → HabitStatus → State → State
setHabitStatus user day habit newStatus st@(State users) = do
  let (UserState habits history) = getUser user st in
    let history' = updateHabitStatus day habit newStatus history in
      State $ Map.insert user (UserState habits history') users

chains ∷ User → Day → State → Map Habit Int
chains user day st = do
  let usrSt = getUser user st in
    chainLengths (uHabits usrSt) $ map successfulHabits $
      historyIterator day (uHistory usrSt)

dayHabitStatus ∷ Day → UserState → Map Habit HabitStatus
dayHabitStatus day usrSt =
  let DayState _ habitStatuses = getDay day $ uHistory usrSt in
    fillStatusBlanks (uHabits usrSt) habitStatuses

getHistory30 ∷ User → Day → State → Map Day (Map Habit HabitStatus)
getHistory30 u day st = getHist $ getUser u st where
  getHist usrSt = Map.fromList [(d,dayHabitStatus d usrSt) | d←days]
  days = [addDays (-age) day | age←[0..29]]

habitsStatus ∷ User → Day → State → Map Habit HabitStatus
habitsStatus u day st = dayHabitStatus day $ getUser u st

getNotes ∷ User → Day → State → Set Text
getNotes user day st = dayNotes $ getDay day $ uHistory $ getUser user st

delNote ∷ User → Day → Text → State → State
delNote user day note st@(State users) =
  let UserState habits history = getUser user st in
  let DayState notes adherance = getDay day history in
  let dayState' = DayState (Set.delete note notes) adherance in
  let history' = Map.insert day dayState' history in
    State $ Map.insert user (UserState habits history') users

addNote ∷ User → Day → Text → State → State
addNote user day note st@(State users) =
  let UserState habits history = getUser user st in
  let DayState notes adherance = getDay day history in
  let dayState' = DayState (Set.insert note notes) adherance in
  let history' = Map.insert day dayState' history in
    State $ Map.insert user (UserState habits history') users
