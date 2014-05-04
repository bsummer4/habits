{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes, LambdaCase #-}

module State
  ( Habit, State
  , HabitStatus(Success, Failure, Unspecified)
  , textHabit, habitText, userText, textUser
  , emptyState
  , userHabits, addHabit, delHabit, chains, habitsStatus, setHabitStatus
  , getHistory30, getNotes, addNote, delNote, renameHabit
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
import Control.Lens


-- [[Datatypes]]
type User = ByteString
data Habit = Habit Text
data HabitStatus = Success(Maybe Double) | Failure(Maybe Double) | Unspecified
data DayState = DayState
  { _dNotes ∷ Set Text
  , _dHabits ∷ Map Habit HabitStatus
  }

data State = State(Map User UserState)
type History = Map Day DayState
data UserState = UserState
  { _uHabits∷Set Habit
  , _uHistory∷History
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
$(makeLenses ''UserState)
$(makeLenses ''DayState)

instance J.FromJSON ByteString where
  parseJSON o = J.parseJSON o >>= return∘encodeUtf8

instance J.ToJSON ByteString where
  toJSON = J.toJSON ∘ decodeUtf8


-- [[Constructors]]
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


-- [[Operations]]
infixr 5 ∪
(∪) = Map.union

user ∷ User → Lens' State UserState
user u = lens get set where
  get (State us) = fromMaybe (UserState Set.empty Map.empty) $ Map.lookup u us
  set (State us) v = State $ Map.insert u v us

day ∷ Day → Lens' History DayState
day d = lens get set where
  get hist = fromMaybe (DayState Set.empty Map.empty) $ Map.lookup d hist
  set hist v = Map.insert d v hist

userHabits ∷ User → State → Set Habit
userHabits u = (^. (user u∘uHabits))

addHabit ∷ User → Habit → State → State
addHabit u newhabit = (user u∘uHabits) %~ Set.insert newhabit

delHabit ∷ User → Habit → State → State
delHabit u newhabit = (user u∘uHabits) %~ Set.delete newhabit

renameHabit ∷ User → Habit → Habit → State → State
renameHabit u old new =
  ((user u∘uHistory)%~rehist) ∘ addHabit u new ∘ delHabit u old
  where
    rehist = Map.map $ dHabits%~rename
    rename adh = case Map.lookup old adh of
      Nothing → adh
      Just status → Map.insert new status adh

habitsStatus ∷ User → Day → State → Map Habit HabitStatus
habitsStatus u d = dayHabitStatus d ∘ (^. user u)

getNotes ∷ User → Day → State → Set Text
getNotes u d = view (user u ∘ uHistory ∘ day d ∘ dNotes)

delNote ∷ User → Day → Text → State → State
delNote u d note = (user u ∘ uHistory ∘ day d ∘ dNotes) %~ Set.delete note

addNote ∷ User → Day → Text → State → State
addNote u d note = (user u ∘ uHistory ∘ day d ∘ dNotes) %~ Set.insert note

successfulHabits ∷ DayState → Set Habit
successfulHabits = Set.fromList ∘ Map.keys ∘ Map.filter isSuccess ∘ _dHabits
  where isSuccess = \case {Success _ → True; _ → False }

updateHabitStatus ∷ Day → Habit → HabitStatus → History → History
updateHabitStatus d habit status = (day d∘dHabits) %~ (Map.insert habit status)

setHabitStatus ∷ User → Day → Habit → HabitStatus → State → State
setHabitStatus u d h status = (user u∘uHistory) %~ updateHabitStatus d h status

history ∷ Day → [Day]
history d = d : history(addDays (-1) d)

getHistory30 ∷ User → Day → State → Map Day (Map Habit HabitStatus)
getHistory30 u day st = Map.fromList $
  (\d→(d,dayHabitStatus d $ st^.user u)) <$> (take 30 $ history day)

chains ∷ User → Day → State → Map Habit Int
chains u d st = lengths where
  lengths = chainLengths (st^.user u^.uHabits) $ map successfulHabits $ iter d
  iter d = (st^.user u^.uHistory^.day d) : iter (addDays (-1) d) where

-- In ‘fillBlanks’ it's important that (∪) is left-biased, so that it always
-- chooses the actual habits over the unspecified ones.
dayHabitStatus ∷ Day → UserState → Map Habit HabitStatus
dayHabitStatus d st = fillBlanks (st^.uHabits) (st^.uHistory∘day d∘dHabits)
  where
    fillBlanks habits statuses = hFilter habits statuses ∪ unspecified habits
    hFilter habits = Map.filterWithKey (\k _→Set.member k habits)
    unspecified = Map.fromList ∘ map (\k→(k,Unspecified)) ∘ Set.toList

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
