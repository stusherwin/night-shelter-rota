module Common (unsafeEventValue) where
  
import Unsafe.Coerce (unsafeCoerce)

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
