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

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do putStrLn "usage: capusdish-scraper <meal period>"
                putStrLn "meal periods are breakfast, lunch, dinner, and all"
        else do
            let queryParams = ("locationId" =: plhId)
                           <> ("mode" =: ("Daily" :: T.Text))
                           <> ("date" =: date)
                           <> ("periodId" =: periodMap M.! head args)
            resp <- runReq defaultHttpConfig $ req GET url' NoReqBody jsonResponse queryParams
            let json = responseBody resp :: Object

            let result = do
                    items <- parse getItems json
                    stationNames <- parse getStationNames json
                    pure (items, stationNames)
            case result of
                Error e -> putStrLn e
                Success (items, stationNames) -> forM_ (M.toList items) $ \(k, is) -> do
                    let stationName = stationNames M.! k
                    if stationName `notElem` hiddenStations
                        then do
                            T.putStrLn $ "--- " `T.append` stationName `T.append` " ---"
                            if showHiddenItems
                                then mapM_ print is
                                else mapM_ print (filter (not . hidden) is)
                    else pure ()
