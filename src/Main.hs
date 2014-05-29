{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Graphics.QML

import GUI.StocQt

main :: IO ()
main = do

    y   <- newMVar "year"
    sid <- newMVar ""
    sn  <- newMVar ""

    sis <- mapM newObjectDC $ genStockList stocks

    sm  <- newObjectDC $ StockModel sid sn
    ss  <- newObjectDC $ StockSettings y
    sc  <- newObjectDC $ StocQt sm sis ss

    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/stocqt.qml"
    , contextObject   = Just $ anyObjRef sc
    }
