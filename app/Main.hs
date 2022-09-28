{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.Map as M
import System.Environment
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

url = "https://ualberta.campusdish.com/api/menu/GetMenus"
url' = https "ualberta.campusdish.com" /: "api" /: "menu" /: "GetMenus"
plhId = "10717" :: T.Text
date = "09/22/2022" :: T.Text

breakfastId = "879" :: T.Text
lunchId = "880" :: T.Text
dinnerId = "881" :: T.Text
allDay = "3473" :: T.Text
periodMap = M.fromList [("breakfast", breakfastId), ("lunch", lunchId), ("dinner", dinnerId)]

showHiddenItems = False
hiddenStations = ["Salad Bar"]

data Item = MkItem
    { name :: T.Text
    , calories :: Int
    , stationId :: T.Text
    , hidden :: Bool
    } deriving (Show)

instance FromJSON Item where
    parseJSON (Object o) = do
        stationId <- o .: "StationId"
        p <- o .: "Product"
        name <- p .: "MarketingName"
        calories <- read <$> p .: "Calories"
        hidden <- o .: "IsDeemphasized"
        pure $ MkItem name calories stationId hidden

data Station = MkStation
    { sId :: T.Text
    , sName :: T.Text
    } deriving (Show)

instance FromJSON Station where
    parseJSON (Object o) = do
        id <- o .: "StationId"
        name <- o .: "Name"
        pure $ MkStation id name

getItems :: Object -> Parser (M.Map T.Text [Item])
getItems obj = do
    menu <- obj .: "Menu"
    mpsVal <- menu .: "MenuProducts"
    mpsArr <- withArray "items" pure mpsVal
    items <- mapM parseJSON mpsArr :: Parser (V.Vector Item)
    pure $ foldl (\acc i -> M.insertWith (++) (stationId i) [i] acc) M.empty items

getStationNames :: Object -> Parser (M.Map T.Text T.Text)
getStationNames obj = do
    menu <- obj .: "Menu"
    stationsVal <- menu .: "MenuStations"
    stationsArr <- withArray "stations" pure stationsVal
    stations <- mapM parseJSON stationsArr :: Parser (V.Vector Station)
    pure $ foldl (\acc s -> M.insertWith const (sId s) (sName s) acc) M.empty stations

downloadJSON :: T.Text -> T.Text -> IO Object
downloadJSON period date = do
    let queryParams = ("locationId" =: plhId)
                   <> ("mode" =: ("Daily" :: T.Text))
                   <> ("date" =: date)
                   <> ("periodId" =: period)
    resp <- runReq defaultHttpConfig $ req GET url' NoReqBody jsonResponse queryParams
    pure $ responseBody resp

parseResponse :: Object -> Result (M.Map T.Text [Item], M.Map T.Text T.Text)
parseResponse obj = do
    items <- parse getItems obj
    stationNames <- parse getStationNames obj
    pure (items, stationNames)

generateMarkdown :: M.Map T.Text [Item] ->  M.Map T.Text T.Text -> T.Text
generateMarkdown itemMap stations = foldl stationItems "" (M.toList itemMap)
    where
        stationItems :: T.Text -> (T.Text, [Item]) -> T.Text
        stationItems acc (stationID, items) = "### " <> (stations M.! stationID) <> "\n"
                                           <> T.concat (item <$> items)
                                           <> "\n"
                                           <> acc
        item :: Item -> T.Text
        item (MkItem name _ _ hidden) = if not hidden || showHiddenItems
                                          then "- " <> name <> "\n"
                                          else ""

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> go "0" "all"
        [dateOffset] -> go (T.pack dateOffset) "all"
        [dateOffset, period] -> go (T.pack dateOffset) (T.pack period)
    where
        go offset period = do
            -- get the date
            let dateOffset = read $ T.unpack offset :: Integer
            date <- addUTCTime (nominalDay * fromInteger dateOffset) <$> getCurrentTime
            timezone <- getCurrentTimeZone
            let zoneDate = utcToLocalTime timezone date
            let (y,m,d) = toGregorian $ localDay zoneDate
            let dateStr = T.pack $ show m <> "/" <> show d <> "/" <> show y

            let pList = if period == "all"
                then ["breakfast", "lunch", "dinner"]
                else [period]

            forM_ pList $ \p -> do
                T.putStrLn $ "# " <> p <> "\n"
                json <- downloadJSON (periodMap M.! p) dateStr
                case parseResponse json of
                    Error e -> T.putStrLn $ T.pack e
                    Success r -> T.putStrLn $ uncurry generateMarkdown r
