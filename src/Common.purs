module App.Common (unsafeEventValue, unsafeEventSelectedIndex, Volunteer) where
  
import Unsafe.Coerce (unsafeCoerce)

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

unsafeEventSelectedIndex :: forall event. event -> Int
unsafeEventSelectedIndex e = (unsafeCoerce e).target.selectedIndex

type Volunteer = { id :: Int
                 , name :: String }
