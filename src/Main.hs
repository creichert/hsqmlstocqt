{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.QML

import GUI.StocQt

main :: IO ()
main = do

    sm  <- newObjectDC =<< defaultStockModel
    ss  <- newObjectDC =<< defaultStockSettings
    sis <- mapM newObjectDC $ genStockList stocks
    sc  <- newObjectDC $ StocQt sm sis ss

    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/stocqt.qml"
    , contextObject   = Just $ anyObjRef sc
    }
