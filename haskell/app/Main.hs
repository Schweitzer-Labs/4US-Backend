{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import Aws.Lambda
import qualified Lib


initializeContext :: IO ()
initializeContext = return ()

main :: IO ()
main = do
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
        (addStandaloneLambdaHandler "handler" Lib.handler)
        (addStandaloneLambdaHandler "anotherHandler" Lib.anotherHandler)


