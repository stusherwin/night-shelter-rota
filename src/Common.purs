module App.Common (unsafeEventValue, unsafeEventSelectedIndex, lensWithProps, lensOfListWithProps, midnight, tomorrow, toDateString, updateWhere, modifyWhere, updateListWhere, modifyListWhere, surroundIf, justIf, default, onlyIf, className, ifJust) where
  
import Prelude 

import Data.Array (findIndex, updateAt, modifyAt, filter)
import Data.DateTime (DateTime(..), Date(..), Time(..), canonicalDate, date, adjust)
import Data.Either (Either(..), fromRight, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Lens (Lens', lens)
import Data.List (List(..), findIndex, updateAt, modifyAt) as L
import Data.Maybe (Maybe(..), fromJust, maybe, fromMaybe)
import Data.String (joinWith, length)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)
import React.DOM.Dynamic (a)
import Unsafe.Coerce (unsafeCoerce)
import React (ReactElement)
import React.DOM.Props as RP

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

lensOfListWithProps :: forall s t p. (s -> L.List t) -> (s -> L.List t -> s) -> (s -> p) -> Lens' s (L.List (Tuple p t))
lensOfListWithProps get set props = lens getter setter
  where 
  getter :: s -> L.List (Tuple p t)
  getter s = map (Tuple (props s)) (get s)
  
  setter :: s -> L.List (Tuple p t) -> s
  setter s a = set s $ map snd a

midnight :: Time
midnight = unsafePartial fromJust $ Time <$> pure bottom <*> pure bottom <*> pure bottom <*> pure bottom

tomorrow :: Date -> Date
tomorrow dt = maybe dt date $ adjust (Days 1.0) (DateTime dt bottom)

toDateString :: Date -> String
toDateString date = unsafePartial $ fromRight $ formatDateTime "D MMMM YYYY" $ DateTime date midnight

updateWhere :: forall a. (a -> Boolean) -> a -> Array a -> Array a
updateWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- updateAt i item list
  pure result

modifyWhere :: forall a. (a -> Boolean) -> (a -> a) -> Array a -> Array a
modifyWhere predicate item list = fromMaybe list $ do
  i <- findIndex predicate list
  result <- modifyAt i item list
  pure result

updateListWhere :: forall a. (a -> Boolean) -> a -> L.List a -> L.List a
updateListWhere predicate item list = fromMaybe list $ do
  i <- L.findIndex predicate list
  result <- L.updateAt i item list
  pure result 

modifyListWhere :: forall a. (a -> Boolean) -> (a -> a) -> L.List a -> L.List a
modifyListWhere predicate item list = fromMaybe list $ do
  i <- L.findIndex predicate list
  result <- L.modifyAt i item list
  pure result

surroundIf :: String -> String -> String -> String
surroundIf _ _ "" = ""
surroundIf start end text = start <> text <> end

justIf :: forall a. a -> Boolean -> Maybe a
justIf a condition = if condition then Just a else Nothing

ifJust :: forall a. (a -> Boolean) -> Maybe a ->Boolean 
ifJust = maybe false

default :: String -> String -> String
default defaultVal ""  = defaultVal
default _          val = val

onlyIf :: Boolean -> String -> String
onlyIf false _   = ""
onlyIf true  val = val

className :: Array String -> RP.Props
className = RP.className <<< joinWith " " <<< (filter $ (_ > 0) <<< length)