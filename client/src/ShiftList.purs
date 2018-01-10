module App.ShiftList (spec, initialState, changeCurrentVol, shiftUpdated, module ShiftListState) where
   
import Prelude 

import Control.Monad.Aff (delay)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, Weekday(..))
import Data.Either (Either(..))
import Data.Lens (Lens', lens, Prism', prism, over)
import Data.List (List(..), zipWith, length)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), uncurry, snd)
import React.DOM as RD 
import React.DOM.Props as RP
import Thermite as T
import React as R
import Control.Monad.Eff.Console (log, CONSOLE)

import App.Common (tomorrow, modifyWhere, toMonthYearString, isFirstDayOfMonth, addDays, previousWeekday, classNames, onlyIf)
import App.ShiftRules (ShiftRuleConfig)
import App.Types (Shift, Vol, VolShift)
import App.ShiftRow (initialState) as SR
import App.Row (spec) as R
import ShiftListState
 
shiftCount :: Int
shiftCount = 28

_rows :: Lens' State (List RowState)
_rows = lens _.rows _{rows = _}

_RowAction :: Prism' Action (Tuple Int RowAction)
_RowAction = prism (uncurry RowAction) unwrap
  where
  unwrap (RowAction i a) = Right (Tuple i a)
  unwrap a = Left a
 
spec :: T.Spec _ State _ Action
spec = 
  ( roster $ T.focus _rows _RowAction $ T.foreach \_ -> R.spec )
  <> footerSpec
  where
  roster :: T.Spec _ State _ Action -> T.Spec _ State _ Action
  roster = over T._render \render d p s c ->
    [ RD.div [ classNames [ "roster", onlyIf ( isJust s.roster.currentVol) "has-current-vol" ] ]
             $ render d p s c
    ]
      
  footerSpec :: T.Spec _ State _ Action
  footerSpec = T.simpleSpec performAction render
    where
    render :: T.Render State _ Action
    render dispatch _ state _ = []

    performAction :: T.PerformAction _ State _ Action
    performAction (RowAction _ PrevPeriod) _ _ = void do
      id <- lift $ liftEff $ firstElementIdByClassName "shift-row"
      st <- lift $ liftEff $ scrollTop
      ot <- lift $ liftEff $ elementOffsetTopById id
      let pos = ot - st
      _ <- T.modifyState \state -> loadPrevPeriod state
      ot' <- lift $ liftEff $ elementOffsetTopById id
      let st' = ot' - pos
      lift $ liftEff $ scrollTo $ st' + 1.0
    performAction (RowAction _ NextPeriod) _ _ = void do
      id <- lift $ liftEff $ lastElementIdByClassName "shift-row"
      st <- lift $ liftEff $ scrollTop
      ot <- lift $ liftEff $ elementOffsetTopById id
      let pos = ot - st
      _ <- T.modifyState \state -> loadNextPeriod state
      ot' <- lift $ liftEff $ elementOffsetTopById id
      let st' = ot' - pos
      lift $ liftEff $ scrollTo st'
    performAction _ _ _ = pure unit
    
initialState :: forall c. Maybe Vol -> List Shift -> ShiftRuleConfig -> State
initialState currentVol shifts config = 
  let startDate = previousWeekday Monday config.currentDate 
      endDate = addDays (shiftCount - 1) startDate
      roster = { currentVol
               , shifts
               , startDate
               , endDate
               , loading: false
               }
  in { roster
     , config
     , rows: rows roster config
     }
 
rows :: RosterState -> ShiftRuleConfig -> List RowState
rows roster config = rows' roster.startDate
  where   
  rows' :: Date -> List RowState
  rows' date | date > roster.endDate = 
      Cons (HeaderRow { text: "", showNext: true, showPrev: false })
    $ Nil
  rows' date | date == roster.startDate =
      Cons (HeaderRow { text: toMonthYearString date, showNext: false, showPrev: true })
    $ Cons (ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date
  rows' date | isFirstDayOfMonth date =
      Cons (HeaderRow { text: toMonthYearString date, showNext: false, showPrev: false })
    $ Cons (ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date
  rows' date =
      Cons (ShiftRow $ SR.initialState roster config date)
    $ rows' $ tomorrow date

preserveLoading :: List RowState -> List RowState -> List RowState
preserveLoading = zipWith row
  where
  row (ShiftRow old) (ShiftRow new) = ShiftRow new { loading = old.loading }
  row _ new = new
 
changeCurrentVol :: Maybe Vol -> State -> State
changeCurrentVol currentVol state =
  let roster' = state.roster { currentVol = currentVol
                             , loading = false
                             }
  in state { roster = roster'
           , rows = preserveLoading state.rows $ rows roster' state.config
           }

loadPrevPeriod :: State -> State
loadPrevPeriod state =
  -- if we've already added an extra set of shifts to the roster we don't want to add any more,
  -- so just bring the end date back as well
  let endDateDiff = if length state.rows > shiftCount + 4 {- no. of header rows that can appear -}
                       then (-shiftCount)
                       else 0
      roster' = state.roster { startDate = addDays (-shiftCount) state.roster.startDate
                             , endDate = addDays endDateDiff state.roster.endDate
                             , loading = false
                             }
  in state { roster = roster'
           , rows = rows roster' state.config
           }

loadNextPeriod :: State -> State
loadNextPeriod state =
  -- if we've already added an extra set of shifts to the roster we don't want to add any more,
  -- so just bring the start date forward as well
  let startDateDiff = if length state.rows > shiftCount + 4 {- no. of header rows that can appear -}
                       then shiftCount
                       else 0
      roster' = state.roster { startDate = addDays startDateDiff state.roster.startDate
                             , endDate = addDays shiftCount state.roster.endDate
                             , loading = false
                             }
  in state { roster = roster'
           , rows = rows roster' state.config
           }

shiftUpdated :: List Shift -> Date -> State -> State
shiftUpdated shifts date state =
  let roster' = state.roster { shifts = shifts }

      isShiftOnDate :: RowState -> Boolean
      isShiftOnDate (ShiftRow s) = s.date == date
      isShiftOnDate _ = false

      cancelLoading :: RowState -> RowState
      cancelLoading (ShiftRow s) = ShiftRow s{ loading = false }
      cancelLoading r = r 
  in state { roster = roster'
           , rows = modifyWhere isShiftOnDate cancelLoading $ preserveLoading state.rows $ rows roster' state.config
           }

foreign import scrollTop :: forall eff. Eff eff Number

foreign import scrollTo :: forall eff. Number -> Eff eff Unit

foreign import elementOffsetTopById :: forall eff. String -> Eff eff Number

foreign import firstElementIdByClassName :: forall eff. String -> Eff eff String

foreign import lastElementIdByClassName :: forall eff. String -> Eff eff String