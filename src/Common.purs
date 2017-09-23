module App.Common (unsafeEventValue, unsafeEventSelectedIndex, lensWithProps, lensOfListWithProps) where
  
import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Data.Lens (Lens', lens)
import Data.List (List) as L
import Data.Tuple (Tuple(..), snd)

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