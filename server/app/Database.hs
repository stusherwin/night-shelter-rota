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
    fromRow = Volunteer <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

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
  
  getAllVolunteers :: ByteString -> Int -> IO [Volunteer]
  getAllVolunteers connectionString rotaId = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer\
      \ where rota_id = ?"
      (Only rotaId)
    close conn
    return result

  getVolunteer :: ByteString -> Int -> Int -> IO (Maybe Volunteer)
  getVolunteer connectionString rotaId id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer\
      \ where id = ? and rota_id = ?"
      (id, rotaId)
    close conn
    return $ listToMaybe result

  addVolunteer :: ByteString -> Int -> VolunteerDetails -> IO Int
  addVolunteer connectionString rotaId details = do
    conn <- connectPostgreSQL connectionString
    [Only id] <- query conn
      " insert into volunteer\
      \   (rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active)\
      \ values\
      \   (?, ?, ?, ?, ?, ?, ?)\
      \ returning id"
      ( rotaId 
      , vdName details
      , vdIntro details
      , vdPref details
      , vdGenderPref details
      , vdNotes details
      , True
      )
    close conn
    return id
  
  updateVolunteer :: ByteString -> Int -> Int -> VolunteerDetails -> IO (Maybe Volunteer)
  updateVolunteer connectionString rotaId id details = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set name = ?\
      \   , intro = ?\
      \   , overnight_pref = ?\
      \   , overnight_gender_pref = ?\
      \   , notes = ?\
      \ where id = ? and rota_id = ?\ 
      \ returning id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      ( vdName details
      , vdIntro details
      , vdPref details
      , vdGenderPref details
      , vdNotes details
      , id
      , rotaId
      )
    close conn
    return $ listToMaybe result
  
  activateVolunteer :: ByteString -> Int -> Int -> IO (Maybe Volunteer)
  activateVolunteer connectionString rotaId id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set active = true\
      \ where id = ? and rota_id = ?\
      \ returning id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      (id, rotaId)
    close conn
    return $ listToMaybe result
  
  deactivateVolunteer :: ByteString -> Int -> Int -> IO (Maybe Volunteer)
  deactivateVolunteer connectionString rotaId id = do
    conn <- connectPostgreSQL connectionString
    result <- query conn
      " update volunteer\
      \ set active = false\
      \ where id = ? and rota_id = ?\
      \ returning id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active"
      (id, rotaId)
    close conn
    return $ listToMaybe result

  getAllShifts :: ByteString -> Int -> IO [Shift]
  getAllShifts connectionString rotaId = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVols <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ where rota_id = ?"
      (Only rotaId)
    rVolShifts <- query conn
      " select shift_date, volunteer_id, shift_type\
      \ from volunteer_shift vs\
      \ join volunteer v\
      \   on vs.volunteer_id = v.id\
      \ where rota_id = ?"
      (Only rotaId)
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let volShifts = rVolShifts <&> \(d, id, st) -> (d, [VolunteerShift (fromJust $ IM.lookup id vols) st])
    let shifts = fromListWith (++) volShifts
    let result = (assocs shifts) <&> \(d, vols) -> Shift d vols
    return result
  
  getVolunteerShifts :: ByteString -> Int -> ShiftDate -> IO [VolunteerShift]
  getVolunteerShifts connectionString rotaId shiftDate = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVols <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ where rota_id = ?"
      (Only rotaId)
    rVolShifts <- query conn
      " select volunteer_id, shift_type\
      \ from volunteer_shift vs\
      \ join volunteer v\
      \   on vs.volunteer_id = v.id\
      \ where shift_date = ? and rota_id = ?"
      (shiftDate, rotaId)
    close conn
    let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
    -- TODO: handle vol not existing?
    let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
    return result
  
  addVolunteerShift :: ByteString -> Int -> ShiftDate -> Int -> ShiftType -> IO (Maybe [VolunteerShift])
  addVolunteerShift connectionString rotaId shiftDate volId shiftType = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query

    rVol <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ where id = ? and rota_id = ?"
      (volId, rotaId)

    if (length (rVol :: [Volunteer]) == 0) then
      return Nothing
    else do
      -- Add should be idempotent, shouldn't throw an error if already exists
      _ <- execute conn
        " insert into volunteer_shift as vs\
        \   (shift_date, volunteer_id, shift_type)\
        \ values\
        \   (?, ?, ?)\
        \ on conflict (shift_date, volunteer_id) do update\
        \   set shift_type = excluded.shift_type\
        \   where vs.shift_date = excluded.shift_date and vs.volunteer_id = excluded.volunteer_id"
        ( shiftDate
        , volId
        , shiftType
        )
    
      rVols <- query conn
        " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
        \ from volunteer v\
        \ where rota_id = ?"
        (Only rotaId)

      rVolShifts <- query conn
        " select volunteer_id, shift_type\
        \ from volunteer_shift vs\
        \ join volunteer v\
        \   on vs.volunteer_id = v.id\
        \ where shift_date = ? and rota_id = ?"
        (shiftDate, rotaId)
      
      close conn
      let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
      -- TODO: handle vol not existing?
      let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
      return (Just result)
      
  removeVolunteerShift :: ByteString -> Int -> ShiftDate -> Int -> IO (Maybe [VolunteerShift])
  removeVolunteerShift connectionString rotaId shiftDate volId = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVol <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ where id = ? and rota_id = ?"
      (volId, rotaId)

    if (length (rVol :: [Volunteer]) == 0) then
      return Nothing
    else do
      count <- execute conn
        " delete from volunteer_shift\
        \ where shift_date = ? and volunteer_id = ?"
        ( shiftDate
        , volId
        )
      if (count == 0) then
        return Nothing
      else do
        rVols <- query conn
          " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
          \ from volunteer v\
          \ where rota_id = ?"
          (Only rotaId)

        rVolShifts <- query conn
          " select volunteer_id, shift_type\
          \ from volunteer_shift vs\
          \ join volunteer v\
          \   on vs.volunteer_id = v.id\
          \ where shift_date = ? and rota_id = ?"
          (shiftDate, rotaId)
        close conn
        let vols = IM.fromList $ map (\v -> (vId v, v)) (rVols :: [Volunteer])
        -- TODO: handle vol not existing?
        let result = rVolShifts <&> \(id, st) -> VolunteerShift (fromJust $ IM.lookup id vols) st
        return $ Just result

  updateVolunteerShift :: ByteString -> Int -> ShiftDate -> Int -> ShiftType -> IO (Maybe [VolunteerShift])
  updateVolunteerShift connectionString rotaId shiftDate volId shiftType = do
    conn <- connectPostgreSQL connectionString
    -- TODO: convert to single db query
    rVol <- query conn
      " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
      \ from volunteer v\
      \ where id = ? and rota_id = ?"
      (volId, rotaId)

    if (length (rVol :: [Volunteer]) == 0) then
      return Nothing
    else do
      count <- execute conn
        " update volunteer_shift\
        \ set shift_type = ?\
        \ where shift_date = ? and volunteer_id = ?"
        ( shiftType
        , shiftDate
        , volId
        )
      if (count == 0) then
        return Nothing
      else do
        rVols <- query conn
          " select id, rota_id, name, intro, overnight_pref, overnight_gender_pref, notes, active\
          \ from volunteer v\
          \ where rota_id = ?"
          (Only rotaId)

        rVolShifts <- query conn
          " select volunteer_id, shift_type\
          \ from volunteer_shift vs\
          \ join volunteer v\
          \   on vs.volunteer_id = v.id\
          \ where shift_date = ? and rota_id = ?"
          (shiftDate, rotaId)
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