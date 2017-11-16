module App.Common (unsafeEventValue, unsafeEventSelectedIndex, lensWithProps, lensOfListWithProps, midnight, tomorrow, toDateString, updateWhere, modifyWhere, updateListWhere, modifyListWhere, surroundIf, justIf, default, onlyIf, className, isJustWith, addDays, toMonthYearString, isFirstDayOfMonth, daysLeftInMonth, toDayString, sortWith, previousWeekday, nextWeekday, unsafeChecked) where
  
import Prelude 

import Data.Array (filter, elemIndex) as A
import Data.List (List(..), findIndex, updateAt, modifyAt, sortBy, findIndex, updateAt, modifyAt, filter)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust, month, year, day)
import Data.Date (lastDayOfMonth, diff, Weekday(..), weekday)
import Data.Either (Either(..), fromRight, either)
import Data.Enum (fromEnum, toEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (toNumber, floor)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromJust, maybe, fromMaybe, isJust)
import Data.String (joinWith, length)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), snd, curry, uncurry)
import Partial.Unsafe (unsafePartial)
import React.DOM.Dynamic (a)
import Unsafe.Coerce (unsafeCoerce)
import React (ReactElement)
import React.DOM.Props as RP

unsafeChecked :: forall event. event -> Boolean
unsafeChecked e = (unsafeCoerce e).target.checked

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

unsafeEventSelectedIndex :: forall event. event -> Int
unsafeEventSelectedIndex e = (unsafeCoerce e).target.selectedIndex

lensWithProps :: forall s t p. (s -> t) -> (s -> t -> s) -> (s -> p) -> Lens' s (Tuple p t)
lensWithProps get set props = lens getter setter
  where
  getter :: s -> Tuple p t
  getter s = Tuple (props s) (get s)
  
  setter :: s -> Tuple p t -> s
  setter s (Tuple _ a) = set s a

lensOfListWithProps :: forall s t p. (s -> List t) -> (s -> List t -> s) -> (s -> p) -> Lens' s (List (Tuple p t))
lensOfListWithProps get set props = lens getter setter
  where 
  getter :: s -> List (Tuple p t)
  getter s = map (Tuple (props s)) (get s)
  
  setter :: s -> List (Tuple p t) -> s
  setter s a = set s $ map snd a

midnight :: Time
midnight = unsafePartial fromJust $ Time <$> pure bottom <*> pure bottom <*> pure bottom <*> pure bottom

addDays :: Int -> Date -> Date
addDays d dt = maybe dt date $ adjust (Days $ toNumber d) (DateTime dt bottom)

tomorrow :: Date -> Date
tomorrow dt = addDays 1 dt

toDateString :: Date -> String
toDateString date = unsafePartial $ fromRight $ formatDateTime "D MMMM YYYY" $ DateTime date midnight

updateWhere :: forall a. (a -> Boolean) -> a -> List a -> List a
updateWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- updateAt i item list
  pure result

modifyWhere :: forall a. (a -> Boolean) -> (a -> a) -> List a -> List a
modifyWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- modifyAt i item list
  pure result

updateListWhere :: forall a. (a -> Boolean) -> a -> List a -> List a
updateListWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- updateAt i item list
  pure result 

modifyListWhere :: forall a. (a -> Boolean) -> (a -> a) -> List a -> List a
modifyListWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- modifyAt i item list
  pure result

surroundIf :: String -> String -> String -> String
surroundIf _ _ "" = ""
surroundIf start end text = start <> text <> end

justIf :: forall a. a -> Boolean -> Maybe a
justIf a condition = if condition then Just a else Nothing

isJustWith :: forall a. (a -> Boolean) -> Maybe a -> Boolean 
isJustWith = maybe false

default :: String -> String -> String
default defaultVal ""  = defaultVal
default _          val = val

onlyIf :: Boolean -> String -> String
onlyIf false _   = ""
onlyIf true  val = val

className :: Array String -> RP.Props
className = RP.className <<< joinWith " " <<< (A.filter $ (_ > 0) <<< length)

toMonthYearString :: Date -> String 
toMonthYearString date = show (month date) <> " " <> (show $ fromEnum $ year date)

contains :: forall a. Eq a => Array a -> a -> Boolean
contains arr x = isJust $ A.elemIndex x arr

positionalPostfix :: Int -> String
positionalPostfix n | [11, 12, 13] `contains` n = "th"
positionalPostfix n | n `mod` 10 == 1 = "st"
positionalPostfix n | n `mod` 10 == 2 = "nd"
positionalPostfix n | n `mod` 10 == 3 = "rd"
positionalPostfix _ = "th"

toDayString :: Date -> String 
toDayString date = let d = fromEnum $ day date
                   in show d <> positionalPostfix d

isFirstDayOfMonth :: Date -> Boolean
isFirstDayOfMonth date = case toEnum 1 of
                           Just d | day date == d -> true
                           _ -> false

daysLeftInMonth :: Date -> Int
daysLeftInMonth date = (floor daysDiff) + 1
  where
  lastDay = canonicalDate (year date) (month date) $ lastDayOfMonth (year date) (month date)
  (Days daysDiff) = diff lastDay date

sortWith :: forall a b. Ord b => (a -> b) -> List a -> List a
sortWith fn = sortBy $ comparing fn

previousWeekday :: Weekday -> Date -> Date
previousWeekday day date = let daysToSubtract = ((fromEnum $ weekday date) - (fromEnum day) + 7) `mod` 7
                           in addDays (-daysToSubtract) date

nextWeekday :: Weekday -> Date -> Date
nextWeekday day date = let daysToAdd = ((fromEnum day) - (fromEnum $ weekday date) + 7) `mod` 7
                       in addDays daysToAdd date                           