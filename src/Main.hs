{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.QML

import GUI.StocQt

main :: IO ()
main = do

    sc <- newObjectDC $ StocQt

    -- doc <- getDataFileName "qml/HSTorChat.qml"
    runEngineLoop defaultEngineConfig {
      initialDocument = fileDocument "qml/stocqt.qml"
    , contextObject   = Just $ anyObjRef sc
    }
