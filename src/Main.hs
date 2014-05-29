{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Graphics.QML

import GUI.StocQt

main :: IO ()
main = do

    -- StockModel members.
    sid <- newMVar "" -- stockId
    sn  <- newMVar "" -- stockName
    sdc <- newMVar "d" -- stockDataCycle
    r   <- newMVar False -- ready
    sp  <- newMVar 0.0 -- stockPrice
    m   <- newMVar 0.0 -- stockPriceModified
    hp  <- newMVar 0.0 -- highestPrice
    hv  <- newMVar 0.0 -- highestVolume

    -- StockSettings
    ct <- newMVar "year" -- chartType
    cc <- newMVar "#ecc088" -- closeColor

    sis <- mapM newObjectDC $ genStockList stocks

    sm  <- newObjectDC $ StockModel sid sn sdc r sp m hp hv
    ss  <- newObjectDC $ StockSettings ct cc
    sc  <- newObjectDC $ StocQt sm sis ss

    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/stocqt.qml"
    , contextObject   = Just $ anyObjRef sc
    }
