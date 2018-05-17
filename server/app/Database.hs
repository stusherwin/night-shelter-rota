{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllVolunteers, getVolunteer, addVolunteer, updateVolunteer, getAllShifts, getVolunteerShifts, addVolunteerShift, removeVolunteerShift, updateVolunteerShift, activateVolunteer, deactivateVolunteer, getRota) where
  import Control.Monad (mzero, when)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  instance ToField OvernightPreference where
    toField PreferToBeAlone        = toDatabaseChar '1'
    toField PreferAnotherVolunteer = toDatabaseChar '2'

  instance FromField OvernightPreference where
    fromField f char = do
      c <- fromField f char
      case c of
        Just '1' -> return PreferToBeAlone
        Just '2' -> return PreferAnotherVolunteer
        _ -> mzero
  
  instance ToField OvernightGenderPreference where
    toField Male   = toDatabaseChar 'M'
    toField Female = toDatabaseChar 'F'

  instance FromField OvernightGenderPreference where
    fromField f char = do
      c <- fromField f char
      case c of
        Just 'M' -> return Male
        Just 'F' -> return Female
        _ -> mzero

  instance FromRow Volunteer where
    fromRow = Volunteer <$> field <*> field <*> field <*> field <*> field <*> field <*> field

  instance FromField ShiftDate where
    fromField f date = do
      sqlDate <- fromField f date
      case sqlDate of
        Just (Finite day) -> return $ toShiftDate day
        _ -> mzero

  instance ToField ShiftDate where
    toField = toField . fromShiftDate
             
  instance FromField ShiftType where
    fromField f char = do
      c <- fromField f char
      case c of
        Just 'O' -> return Overnight
        Just 'E' -> return Evening
        _ -> mzero

  instance ToField ShiftType where
    toField Overnight = toDatabaseChar 'O'
    toField Evening   = toDatabaseChar 'E'
                        
  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  
  getAllVolunteers :: ByteString -> IO [Volunteer]
  getAllVolunteers connectionString = do
    conn <- connectPostgreSQL connectionString
    result <- query_ conn
      " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer"
    close conn
    return result

  getVolunteer :: ByteString -> Int -> IO (Maybe Volunteer)
  getVolunteer connectionString id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer\
      \ where id = ?"
      (Only id)
    close conn
    return $ listToMaybe result

  addVolunteer :: ByteString -> VolunteerDetails -> IO Int
  addVolunteer connectionString details = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn
      " insert into volunteer\
      \   (name, intro, overnight_pref, overnight_gender_pref, notes, active)\
      \ values\
      \   (?, ?, ?, ?, ?, ?)\
      \ returning id"
      ( vdName details
      , vdIntro details
      , vdPref details
      , vdGenderPref details
      , vdNotes details
      , True
      )
    close conn
    return id
  
  updateVolunteer :: ByteString -> Int -> VolunteerDetails -> IO (Maybe Volunteer)
  updateVolunteer connectionString id details = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set name = ?\
      \   , intro = ?\
      \   , overnight_pref = ?\
      \   , overnight_gender_pref = ?\
      \   , notes = ?\
      \ where id = ?\
      \ returning id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      ( vdName details
      , vdIntro details
      , vdPref details
      , vdGenderPref details
      , vdNotes details
      , id
      )
    close conn
    return $ listToMaybe result
  
  activateVolunteer :: ByteString -> Int -> IO (Maybe Volunteer)
  activateVolunteer connectionString id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set active = true\
      \ where id = ?\
      \ returning id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      (Only id)
    close conn
    return $ listToMaybe result
  
  deactivateVolunteer :: ByteString -> Int -> IO (Maybe Volunteer)
  deactivateVolunteer connectionString id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set active = false\
      \ where id = ?\
      \ returning id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      (Only id)
    close conn
    return $ listToMaybe result

  getAllShifts :: ByteString -> IO [Shift]
  getAllShifts connectionString = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVols <- query_ conn
      " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ join volunteer_shift vs\
      \   on vs.volunteerId = v.id"
    rVolShifts <- query_ conn
      " select shiftDate, volunteerId, shiftType\
      \ from volunteer_shift"
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let volShifts = rVolShifts <&> \(d, id, st) -> (d, [VolunteerShift (fromJust $ IM.lookup id vols) st])
    let shifts = fromListWith (++) volShifts
    let result = (assocs shifts) <&> \(d, vols) -> Shift d vols
    return result
  
  getVolunteerShifts :: ByteString -> ShiftDate -> IO [VolunteerShift]
  getVolunteerShifts connectionString shiftDate = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVols <- query conn
      " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ join volunteer_shift vs\
      \   on vs.volunteerId = v.id\
      \ where shiftDate = ?"
      (Only shiftDate)
    rVolShifts <- query conn
      " select volunteerId, shiftType\
      \ from volunteer_shift\
      \ where shiftDate = ?"
      (Only shiftDate)
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
    return result
  
  addVolunteerShift :: ByteString -> ShiftDate -> Int -> ShiftType -> IO [VolunteerShift]
  addVolunteerShift connectionString shiftDate volId shiftType = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    -- Add should be idempotent, shouldn't throw an error if already exists
    _ <- execute conn
      " insert into volunteer_shift as vs\
      \   (shiftDate, volunteerId, shiftType)\
      \ values\
      \   (?, ?, ?)\
      \ on conflict (shiftDate, volunteerId) do update\
      \   set shiftType = excluded.shiftType\
      \   where vs.shiftDate = excluded.shiftDate and vs.volunteerId = excluded.volunteerId"
      ( shiftDate
      , volId
      , shiftType
      )
  
    rVols <- query conn
      " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ join volunteer_shift vs\
      \   on vs.volunteerId = v.id\
      \ where shiftDate = ?"
      (Only shiftDate)
    rVolShifts <- query conn
      " select volunteerId, shiftType\
      \ from volunteer_shift\
      \ where shiftDate = ?"
      (Only shiftDate)
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
    return result
      
  removeVolunteerShift :: ByteString -> ShiftDate -> Int -> IO (Maybe [VolunteerShift])
  removeVolunteerShift connectionString shiftDate volId = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    count <- execute conn
      " delete from volunteer_shift\
      \ where shiftDate = ? and volunteerId = ?"
      ( shiftDate
      , volId
      )
    if (count == 0) then
      return Nothing
    else do
      rVols <- query conn
        " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
        \ from volunteer v\
        \ join volunteer_shift vs\
        \   on vs.volunteerId = v.id\
        \ where shiftDate = ?"
        (Only shiftDate)
      rVolShifts <- query conn
        " select volunteerId, shiftType\
        \ from volunteer_shift\
        \ where shiftDate = ?"
        (Only shiftDate)
      close conn
      let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
      -- TODO: handle vol not existing?
      let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
      return $ Just result

  updateVolunteerShift :: ByteString -> ShiftDate -> Int -> ShiftType -> IO (Maybe [VolunteerShift])
  updateVolunteerShift connectionString shiftDate volId shiftType = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    count <- execute conn
      " update volunteer_shift\
      \ set shiftType = ?\
      \ where shiftDate = ? and volunteerId = ?"
      ( shiftType
      , shiftDate
      , volId
      )
    if (count == 0) then
      return Nothing
    else do
      rVols <- query conn
        " select id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
        \ from volunteer v\
        \ join volunteer_shift vs\
        \   on vs.volunteerId = v.id\
        \ where shiftDate = ?"
        (Only shiftDate)
      rVolShifts <- query conn
        " select volunteerId, shiftType\
        \ from volunteer_shift\
        \ where shiftDate = ?"
        (Only shiftDate)
      close conn
      let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
      -- TODO: handle vol not existing?
      let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
      return $ Just result

  getRota :: ByteString -> String -> IO (Maybe Int)
  getRota connectionString key = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " select id\
      \ from rota\
      \ where key = ?"
      (Only key)
    close conn
    return $ listToMaybe $ fmap fromOnly $ (result :: [Only Int])